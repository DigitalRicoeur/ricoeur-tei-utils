#lang racket/base

(require syntax/parse/define
         racket/contract
         racket/match
         racket/set
         racket/stream
         "instance-info.rkt"
         (for-syntax racket/base
                     syntax/parse
                     syntax/for-body
                     ))

(provide instance-set?
         instance-set/c
         in-instance-set
         for/instance-set
         for*/instance-set
         (contract-out
          [rename make-instance-set
                  instance-set
                  (->* {}
                       {(stream/c instance-info?)}
                       (instance-set/c))]
          [instance-set*
           (-> instance-info? ... (instance-set/c))]
          [instance-set->plain
           (-> (instance-set/c) (instance-set/c))]
          [instance-set-try-ref
           (-> (instance-set/c) symbol?
               (or/c #f instance-info?))]
          [instance-set-ref
           (-> (instance-set/c) symbol?
               instance-info?)]
          ))

(struct instance-set (plain? hsh)
  ;; The plain? field means the set is known to be plain.
  ;; For efficiency of set-remove, a set may actually
  ;; be plain w/o the plain? field being #t.
  #:transparent
  #:methods gen:equal+hash
  [(define/match (equal-proc a b rec)
     [{(instance-set _ a-hsh) (instance-set _ b-hsh) _}
      (rec a-hsh b-hsh)])
   (define (-hash-proc this rec)
     (rec (instance-set-hsh this)))
   (define hash-proc -hash-proc)
   (define hash2-proc -hash-proc)]
  #:methods gen:set
  [;; Minimal methods:
   (define (set-member? st v)
     (unless (instance-info? v)
       (raise-argument-error/info 'set-member? 1 st v))
     (hash-has-key? (instance-set-hsh st)
                    (instance-title/symbol v)))
   (define (set-add st v)
     (unless (instance-info? v)
       (raise-argument-error/info 'set-add 1 st v))
     (match-define (instance-set plain? hsh) st)
     (instance-set (and plain? (plain-instance-info? v))
                   (hash-set hsh
                             (instance-title/symbol v)
                             v)))
   (define (set-remove st v)
     (unless (instance-info? v)
       (raise-argument-error/info 'set-remove 1 st v))
     (struct-copy instance-set st
                  [hsh (hash-remove (instance-set-hsh st)
                                    (instance-title/symbol v))]))
   (define (set-first st)
     (define hsh (instance-set-hsh st))
     (define pos (hash-iterate-first hsh))
     (unless pos
       (raise-argument-error
        'set-first
        "(and/c instance-set? (not/c set-empty?))"
        st))
     (hash-iterate-value hsh pos))
   (define (set-empty? st)
     (hash-empty?
      (instance-set-hsh st)))
   (define (set-copy-clear st)
     empty-instance-set)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Extra (non-imperative) methods:
   (define (set->list st)
     (for/list ([v (unchecked:in-instance-set st)])
       v))
   (define set->stream set->list)
   (define (set-count st)
     (hash-count (instance-set-hsh st)))
   (define (set-rest st)
     (when (set-empty? st)
       (raise-argument-error
        'set-rest
        "(and/c instance-set? (not/c set-empty?))"
        st))
     (set-remove st (set-first st)))
   (define (set-map st proc)
     (for/list ([v (unchecked:in-instance-set st)])
       (proc v)))
   (define (set-for-each st proc)
     (for ([v (unchecked:in-instance-set st)])
       (proc v)))
   (define (in-set st)
     (unchecked:in-instance-set st))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Complex Methods:
   ;; set-clear ;; supported via set-remove and set->stream
   ;;   due to contract subtlty from docs for set-copy-clear
   #|set=?
      set-union
      set-intersect
      set-subtract
      set-symmetric-difference 
      subset?
      proper-subset?|#
   #|END #:methods gen:set|#])

(define empty-instance-set
  (instance-set #t #hasheq()))

(define (raise-argument-error/info who pos . args)
  (apply raise-argument-error
         who
         "instance-info?"
         pos
         args))

(define (instance-set-try-ref st sym)
  (hash-ref (instance-set-hsh st) sym #f))
            
(define (instance-set-ref st sym)
  (or (instance-set-try-ref st sym)
      (raise-arguments-error
       'instance-set-ref
       "no value found for key"
       "key" sym
       "set" st)))

(define (instance-set->plain st)
  (match-define (instance-set plain? hsh) st)
  (cond
    [plain?
     st]
    [else
     (instance-set #t
                   (for/hasheq ([info (in-immutable-hash-values hsh)])
                     (define plain (get-plain-instance-info info))
                     (values (instance-title/symbol plain)
                             plain)))]))

;                                                                  
;                            ;;                              ;;    
;                            ;;                              ;;    
;      ;;;   ;;;    ;; ;   ;;;;;;;  ;; ;;;    ;;       ;;; ;;;;;;; 
;    ;;   ; ;   ;   ;;; ;    ;;     ;;;      ;  ;    ;;   ;  ;;    
;    ;      ;   ;   ;;  ;;   ;;     ;;          ;;   ;       ;;    
;   ;;     ;;   ;;  ;;  ;;   ;;     ;;        ;;;;  ;;       ;;    
;    ;      ;   ;   ;;  ;;   ;;     ;;       ;  ;;   ;       ;;    
;    ;;   ; ;   ;   ;;  ;;    ;     ;;      ;;  ;;   ;;   ;   ;    
;      ;;;   ;;;    ;;  ;;     ;;;  ;;       ;;; ;     ;;;     ;;; 
;                                                                  

(struct flat-instance-set-contract (elem/c)
  #:transparent
  #:property prop:custom-write
  contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (match-lambda
            [(flat-instance-set-contract elem/c)
             (make-set-contract-name elem/c)])
   #:stronger (λ (this other)
                (cond
                  [(contract-stronger? instance-set? other)
                   #t]
                  [(flat-instance-set-contract? other)
                   (contract-stronger?
                    (flat-instance-set-contract-elem/c this)
                    (flat-instance-set-contract-elem/c other))]
                  [else
                   #f]))
   #:equivalent (match-lambda**
                  [{(flat-instance-set-contract this)
                    (flat-instance-set-contract other)}
                    (contract-equivalent? this other)]
                  [{_ _}
                   #f])
   #:first-order
   (match-lambda
     [(flat-instance-set-contract elem/c)
      (define elem?
        (flat-contract-predicate elem/c))
      (match-lambda
        [(instance-set _ hsh)
         (for/and ([val (in-immutable-hash-values hsh)])
           (elem? val))]
        [_ #f])])
   #:late-neg-projection
   (match-lambda
     [(flat-instance-set-contract elem/c)
      (define elem-late-neg
        (get/build-late-neg-projection elem/c))
      (λ (blame)
        (define elem+blame
          (elem-late-neg (blame-add-context blame
                                            "an element of")))
        (λ (val neg-party)
          (unless (instance-set? val)
            (raise-blame-error
             blame #:missing-party neg-party
             val
             '(expected: "instance-set?" given: "~e")
             val))
          (for ([info (unchecked:in-instance-set val)])
            (elem+blame info neg-party))
          val))])))
   
(define (make-set-contract-name elem/c)
  (build-compound-type-name 'instance-set/c elem/c))
             
(define/subexpression-pos-prop instance-set/c
  (case-lambda
    [() instance-set?]
    [(elem/c)
     (let ([elem/c (coerce-chaperone-contract 'instance-set/c elem/c)])
       (cond
         [(flat-contract? elem/c)
          (flat-instance-set-contract elem/c)]
         [else
          (rename-contract
           (and/c instance-set?
                  (set/c elem/c
                         #:cmp 'dont-care
                         #:kind 'dont-care))
           (make-set-contract-name elem/c))]))]))

;                          
;       ;;;                
;     ;;                   
;   ;;;;;;;  ;;;    ;; ;;; 
;     ;;    ;   ;   ;;;    
;     ;;    ;   ;   ;;     
;     ;;   ;;   ;;  ;;     
;     ;;    ;   ;   ;;     
;     ;;    ;   ;   ;;     
;     ;;     ;;;    ;;     
;                          

(define-for-syntax (make-for-transformer for-id who-id)
  (syntax-parser
    [(_ clauses raw-body ...+)
     #:with for?/fold/derived for-id
     #:with ((pre-body ...) (post-body ...))
     (split-for-body this-syntax
                     #'(raw-body ...))
     #`(for?/fold/derived
        #,this-syntax
        ([plain? #t]
         [hsh #hasheq()]
         #:result (instance-set plain? hsh))
        clauses
        pre-body ...
        (define info
          (let () post-body ...))
        (check-instance-info-result info '#,who-id)
        (values (and plain?
                     (plain-instance-info? info))
                (hash-set hsh
                          (instance-title/symbol info)
                          info)))]))
     
(define (check-instance-info-result info who)
  (unless (instance-info? info)
    (raise-result-error who
                        "instance-info?"
                        info)))

(define-syntax for/instance-set
  (make-for-transformer #'for/fold/derived
                        #'for/instance-set))

(define-syntax for*/instance-set
  (make-for-transformer #'for*/fold/derived
                        #'for*/instance-set))

;      ;           
;      ;;          
;   ;;;;;   ;; ;   
;      ;;   ;;; ;  
;      ;;   ;;  ;; 
;      ;;   ;;  ;; 
;      ;;   ;;  ;; 
;      ;;   ;;  ;; 
;      ;;   ;;  ;; 


(define-syntax-parser define-sequence-syntax/simple
  [(_ (name:id arg:id) rhs:expr)
   #:with (first-class)
   (generate-temporaries (list #'name))
   #'(begin
       (define (first-class arg)
         rhs)
       (define-sequence-syntax name
         (λ () (quote-syntax first-class))
         (syntax-parser
           [[(var:id) (_ (~var arg expr))]
            #'[(var) rhs]])))])

(define-sequence-syntax/simple (unchecked:in-instance-set st)
  (in-immutable-hash-values
   (instance-set-hsh st)))

(define (check-instance-set v who)
  (unless (instance-set? v)
    (raise-argument-error who "instance-set?" v))
  v)

(define-sequence-syntax/simple (in-instance-set st)
  (unchecked:in-instance-set
   (check-instance-set st 'in-instance-set)))

;                             
;                   ;;             
;                   ;;             
;  ; ;; ;;    ;;    ;;   ;;   ;;;  
;  ;; ;; ;   ;  ;   ;;  ;   ;;   ; 
;  ;; ;; ;;     ;;  ;; ;    ;    ; 
;  ;; ;; ;;   ;;;;  ;;;;   ;;;;;;;;
;  ;; ;; ;;  ;  ;;  ;;  ;   ;      
;  ;; ;; ;; ;;  ;;  ;;   ;  ;;   ; 
;  ;; ;; ;;  ;;; ;  ;;   ;;   ;;;  
;                                  
                                

(define make-instance-set
  (case-lambda
    [() 
     empty-instance-set]
    [(stm)
     (for/instance-set ([info (in-stream stm)])
       info)]))

(define instance-set*
  (case-lambda
    [() 
     empty-instance-set]
    [args
     (for/instance-set ([info (in-list args)])
       info)]))

