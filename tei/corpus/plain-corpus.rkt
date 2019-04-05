#lang racket/base

(require racket/contract
         racket/class
         racket/set
         racket/promise
         ricoeur/tei/base
         syntax/parse/define
         racket/stxparam
         racket/splicing
         (for-syntax racket/base
                     "class-exptime.rkt"
                     syntax/transformer))

(provide checksum-table/c
         corpus-mixin
         super-docs
         super-docs-evt
         super-docs-evt?
         (contract-out
          [plain-corpus%
           (class/c (init [docs (instance-set/c tei-document?)]))]
          [corpus-get-instance-info-set
           (-> (is-a?/c plain-corpus%)
               (instance-set/c))]
          [corpus-get-checksum-table
           (-> (is-a?/c plain-corpus%)
               checksum-table/c)]
          ))

(define/final-prop checksum-table/c
  (hash/c symbol? symbol?
          #:immutable #t))

(define-local-member-name initialize)

(define plain-corpus%
  (class* object% {(interface ()
                     [get-instance-info-set
                      (->m (instance-set/c))]
                     [get-checksum-table
                      (->m checksum-table/c)]
                     initialize)}
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Initialization:
    (init [docs (instance-set)])
    (define info
      (instance-set
       (set-map docs get-plain-instance-info)))
    (define pr:checksum-table
      (delay/thread
       (for/hasheq ([doc (in-instance-set docs)])
         (values (instance-title/symbol doc)
                 (tei-document-checksum doc)))))
    ;; Must come at the end of initialization:
    (initialize docs)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods:
    (define/public-final (get-instance-info-set)
      info)
    (define/public-final (get-checksum-table)
      (force pr:checksum-table))
    (define/pubment (initialize docs)
      (inner (void) initialize docs))
    #|END class plain-corpus%|#))

(define plain-corpus<%>
  (class->interface plain-corpus%))


(define gen:get-instance-info-set
  (generic plain-corpus<%> get-instance-info-set))
(define (corpus-get-instance-info-set c)
  (send-generic c gen:get-instance-info-set))

(define gen:get-checksum-table
  (generic plain-corpus<%> get-checksum-table))
(define (corpus-get-checksum-table c)
  (send-generic c gen:get-checksum-table))

;                                          
;                                          
;                                          
;                                          
;              ;               ;           
;              ;;              ;;          
;  ; ;; ;;  ;;;;;  ;;   ;;  ;;;;;   ;; ;   
;  ;; ;; ;     ;;    ;  ;      ;;   ;;; ;  
;  ;; ;; ;;    ;;    ; ;       ;;   ;;  ;; 
;  ;; ;; ;;    ;;     ;        ;;   ;;  ;; 
;  ;; ;; ;;    ;;    ; ;       ;;   ;;  ;; 
;  ;; ;; ;;    ;;   ;   ;      ;;   ;;  ;; 
;  ;; ;; ;;    ;;  ;;   ;;     ;;   ;;  ;; 
;                                          
;                                          
;                                          
;                                          

(module repr racket/base
  (require racket/match)
  (provide super-docs-evt?
           make-super-docs-repr
           do-super-docs-ref
           super-docs-repr-post!)
  (struct super-docs-evt (sema box [evt* #:auto #:mutable])
    #:auto-value #f
    #:property prop:evt
    (λ (this)
      (or (super-docs-evt-evt* this)
          (let ([evt* (wrap-evt (semaphore-peek-evt
                                 (super-docs-evt-sema this)
                                 (λ (_) (unbox* (super-docs-evt-box this)))))])
            (set-super-docs-evt-evt*! this evt*)
            evt*))))
  (define (make-super-docs-repr)
    (super-docs-evt (make-semaphore) (box #f)))
  (define (do-super-docs-ref it)
    (or (unbox* (super-docs-evt-box it))
        (raise (exn:fail:contract:variable
                (string-append
                 "super-docs: undefined;\n"
                 " cannot access documents before super-class initialization")
                (current-continuation-marks)
                'super-docs))))
  (define (super-docs-repr-post! it docs)
    (match-define (super-docs-evt sema bx _) it)
    (if (box-cas! bx #f docs)
        (semaphore-post sema)
        (raise-arguments-error
         'super-docs-repr-post!
         "already posted"
         "super-docs-repr" it
         "docs..." docs
         "old-value..." (unbox bx)))))
(require 'repr)

(define-rename-transformer-parameter local-repr
  (make-rename-transformer #'stxparam-false))

(define-rename-transformer-parameter in-method?
  (make-rename-transformer #'stxparam-false))
  
(define-syntax stxparam-false #f)

(define-for-syntax (super-docs-error-message)
  (if (syntax-parameter-value #'in-method?)
      "not allowed in a method body;\n only permitted in initialization and field expressions"
      "only allowed inside a corpus-mixin expression"))

(define-syntax-parser super-docs-evt
  [(_)
   #|#:fail-unless (and (syntax-local-value #'local-repr)
                      (not (syntax-local-value #'in-method?)))
   (super-docs-error-message)|#
   #'local-repr])

(define-syntax-parser super-docs
  [(_)
   #|#:fail-unless (and (syntax-local-value #'local-repr)
                      (not (syntax-local-value #'in-method?)))
   (super-docs-error-message)|#
   #'(do-super-docs-ref local-repr)])

(define-syntax-parser wrap-super-docs-class-clauses
  [(_ repr:id (class-clause:expr ...))
   (local-expand-class-clauses
    (syntax->list #'(class-clause ...))
    #:wrap-method (λ (raw)
                    #`(syntax-parameterize ([in-method? #t]) #,raw))
    #:wrap-init
    (λ (raw)
      #`(letrec-syntaxes+values
            ([{repr-renamer}
              (make-rename-transformer #'repr-simple-binding)])
          ([{repr-simple-binding} repr])
          (syntax-parameterize ([local-repr
                                 (make-rename-transformer #'repr-renamer)])
            #,raw))))])
  
(define-syntax-parser corpus-mixin
  ;#:track-literals
  [(_ (~describe "\"from\" interfaces" [from<%>:expr ...])
      (~describe "\"to\" interfaces" [to<%>:expr ...])
      (~describe "class clause" src-class-clause:expr)
      ...+)
   ;; this would be much nicer with a fix for any of
   ;;   - https://github.com/racket/racket/issues/2578
   ;;   - https://github.com/racket/racket/issues/1898
   ;;   - https://github.com/racket/racket/issues/2580
   ;;   - https://github.com/racket/racket/issues/2579
   #`(let ()
       (define-local-member-name repr/external)
       (mixin [plain-corpus<%> from<%> ...] [to<%> ...]
         (field [(repr repr/external) (make-super-docs-repr)])
         (define/augment (initialize docs)
           (super-docs-repr-post! repr docs)
           (inner (void) initialize docs))
         (wrap-super-docs-class-clauses
          repr [src-class-clause ...])
         (set! repr #f)))])
