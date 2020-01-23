#lang racket/base

(require racket/contract
         racket/class
         racket/match
         racket/unit
         racket/string
         racket/promise
         racket/set
         data/order
         syntax/parse/define
         ricoeur/tei/base
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/match
                     racket/list
                     racket/unit-exptime))

(module+ test
  (require rackunit
           (submod "..")))

(provide term/c
         search-languages/c
         searchable-document-set?
         document-search-results?
         search-result?
         lazy+eager-search-backend/c ;; mostly for docs
         (rename-out
          [match:document-search-results document-search-results]
          [match:search-result search-result])
         (contract-out
          [document-search-results-count
           (-> document-search-results?
               exact-positive-integer?)]
          [document-search-results-results
           (-> document-search-results?
               (non-empty-listof search-result?))]
          [search-result-<?
           (-> search-result? search-result? boolean?)]
          [search-result->?
           (-> search-result? search-result? boolean?)]
          [search-result-excerpt
           (-> search-result?
               (maybe/c (and/c string-immutable/c
                               trimmed-string-px)))]
          [searchable-document-set-do-term-search
           (->* {searchable-document-set?
                 term/c}
                {#:ricoeur-only? any/c
                 #:languages search-languages/c
                 #:book/article (or/c 'any 'book 'article)
                 #:exact? any/c}
                (instance-set/c document-search-results?))]
          ))

(module+ private
  (provide normalized-term?
           searchable-document-set<%>
           search^
           define-compound-search-unit
           define-lazy-search-unit
           (contract-out
            [tei-document->excerpt-max-allow-chars
             (-> tei-document?
                 exact-positive-integer?)]
            [pregexp-quote-normalized-term
             (-> normalized-term?
                 #:exact? any/c
                 string-immutable/c)]
            [normalized-term-string
             (-> normalized-term?
                 (and/c term/c
                        trimmed-string-px))]
            [segment-make-search-results
             (-> segment?
                 (listof (maybe/c (and/c string-immutable/c
                                         #px"[^\\s]")))
                 (listof search-result?))]
            [search-result-nullify-excerpt
             (-> search-result? search-result?)]
            [make-document-search-results
             (-> instance-info?
                 (non-empty-listof search-result?)
                 document-search-results?)]
            )))


(TODO/void add string-not-blank/c and
           string-immutable-not-blank/c #:
           use them in various places
           e.g. base-segment-body)

(define/final-prop term/c
  ;; n.b. \S only matches ASCII
  ;; Since #px"[^\\s]" has security implications,
  ;; we have to care that it's sound.
  (and/c string-immutable/c #px"[^\\s]"))

(struct normalized-term (string)
  #:transparent
  #:guard (λ (term name)
            (string->immutable-string
             (string-normalize-spaces ;; ???
              (string-trim term)))))

(define EXCERPT_RATIO
  ;the maximum portion of the document that may be
  ;shown in excerpts
  18/100)

(define* (tei-document->excerpt-max-allow-chars doc)
  #:with [(define cache (make-weak-hasheq))]
  (cond
    [(hash-ref cache doc #f)]
    [else
     (define doc-chars
       (string-length
        (string-normalize-spaces
         (string-trim
          (tei-document->plain-text
           doc
           #:include-header? #f)))))
     (define rslt
       (max (floor (* EXCERPT_RATIO doc-chars)) 1))
     (hash-set! cache doc rslt)
     rslt]))

;; regexp-quote-normalized-term : normalized-term? #:exact? any/c
;;   -> string-immutable/c
(define pregexp-quote-normalized-term
  (let* ([categories "\\p{P}|\\p{Z}|\\p{N}|\\p{Sc}|\\p{Sm}|\\p{Cc}"]
         ;; Old versions: "(?:^|[^[:alpha:]])" and "(?:[^[:alpha:]]|$)"
         ;; allowed any character but a-z or A-Z to terminate an exact match.
         ;; This could be inadequite with non-ASCII characters e.g. accent marks.
         ;; In new version, only the following unicode categories
         ;; can terminate an exact match:
         ;;   P :: Punctuation
         ;;   Z :: Separator
         ;;   N :: Number ;; allowing b/c this could be an unencoded footnote
         ;;   Sc :: Symbol, currency
         ;;   Sm :: Symbol, math
         ;;   Cc :: Other, control
         ;; There may be other categories that would be ok to allow, but
         ;; these seem to exclude all the troublesome ones.
         [exact?-px-prefix-str (string-append "(?:^|" categories ")")]
         [exact?-px-suffix-str (string-append "(?:" categories "|$)")])
    (λ (t #:exact? exact?)
      (define base
        (regexp-quote (normalized-term-string t) #f))
      (string->immutable-string
       (if exact?
           (string-append exact?-px-prefix-str
                          base
                          exact?-px-suffix-str)
           base)))))

(module+ test
  (let ([exact-apple-px (pregexp (pregexp-quote-normalized-term
                                  (normalized-term "apple")
                                  #:exact? #t))])
    (check-true (regexp-match? exact-apple-px "apple"))
    (check-true (regexp-match? exact-apple-px "apPle"))
    (check-true (regexp-match? exact-apple-px " apPle."))
    (check-true (regexp-match? exact-apple-px "$apPle1"))
    (check-false (regexp-match? exact-apple-px "appleé"))))

(define/final-prop search-languages/c
  (or/c 'any language-symbol/c (listof language-symbol/c)))
                                                       
;                                                          
;                                   ;;;;     ;;            
;                                     ;;     ;;            
;   ;; ;;;    ;;;     ;;    ;;  ;;    ;;   ;;;;;;;    ;;   
;   ;;;     ;;   ;  ;;  ;   ;;  ;;    ;;     ;;     ;;  ;  
;   ;;      ;    ;   ;      ;;  ;;    ;;     ;;      ;     
;   ;;     ;;;;;;;;   ;;    ;;  ;;    ;;     ;;       ;;   
;   ;;      ;           ;;  ;;  ;;    ;;     ;;         ;; 
;   ;;      ;;   ;  ;   ;    ; ;;;     ;      ;     ;   ;  
;   ;;        ;;;    ;;;      ; ;;      ;;     ;;;   ;;;   
;                                                          
;                                                          

(struct search-result (meta sub-counter excerpt)
  #:transparent
  #:property prop:segment
  (λ (it) (search-result-meta it)))

(define-match-expander match:search-result
  (syntax-parser
    [(_ excerpt-pat:expr)
     #'(search-result _ _ excerpt-pat)]))

(define (segment-make-search-results seg l-excerpts/raw)
  (define meta
    (segment-get-meta seg))
  (for/list ([raw-excerpt (in-list l-excerpts/raw)]
             [sub-counter (in-naturals)])
    (search-result meta
                   sub-counter 
                   (fmap (λ (s)
                           (string->immutable-string
                            (string-trim s)))
                         raw-excerpt))))
                   
(define (search-result-nullify-excerpt it)
  (struct-copy search-result it
               [excerpt (nothing)]))


(define search-result-order
  (order 'search-result-order
         search-result?
         (match-lambda**
           [{(and a (search-result a-meta a-sub-counter _))
             (and b (search-result b-meta b-sub-counter _))}
            (match (segment-order a-meta b-meta)
              ['=
               (real-order a-sub-counter b-sub-counter)]
              [rslt
               rslt])])))

(define search-result-<?
  (order-<? search-result-order))

(define (search-result->? a b)
  (eq? '> (search-result-order a b)))


(struct document-search-results (info count results)
  #:transparent
  #:property prop:instance-info
  (λ (it) (document-search-results-info it)))

(define (make-document-search-results info results)
  (TODO/void make-document-search-results #:
             add checks)
  (document-search-results (get-plain-instance-info info)
                           (length results)
                           results))

(define-match-expander match:document-search-results
  (syntax-parser
    [(_ (~alt (~optional (~seq #:count count-pat:expr)
                         #:defaults ([count-pat #'_]))
              (~optional (~seq #:results results-pat:expr)
                         #:defaults ([results-pat #'_])))
        ...)
     #'(document-search-results _ count-pat results-pat)]))

;                                                                          
;      ;             ;;                         ;;;                        
;      ;;            ;;                       ;;                           
;   ;;;;;   ;; ;   ;;;;;;;    ;;;   ;; ;;;  ;;;;;;;   ;;       ;;;    ;;;  
;      ;;   ;;; ;    ;;     ;;   ;  ;;;       ;;     ;  ;    ;;   ; ;;   ; 
;      ;;   ;;  ;;   ;;     ;    ;  ;;        ;;        ;;   ;      ;    ; 
;      ;;   ;;  ;;   ;;    ;;;;;;;; ;;        ;;      ;;;;  ;;     ;;;;;;;;
;      ;;   ;;  ;;   ;;     ;       ;;        ;;     ;  ;;   ;      ;      
;      ;;   ;;  ;;    ;     ;;   ;  ;;        ;;    ;;  ;;   ;;   ; ;;   ; 
;      ;;   ;;  ;;     ;;;    ;;;   ;;        ;;     ;;; ;     ;;;    ;;;  
;                                                                          
;                                                                          
                           
(define searchable-document-set<%>
  (interface ()
    [do-term-search
     (->m normalized-term?
          #:ricoeur-only? any/c
          #:languages (set/c language-symbol/c #:cmp 'eq #:kind 'immutable)
          #:book/article (or/c 'any 'book 'article)
          #:exact? any/c
          (instance-set/c document-search-results?))]))

(TODO/void searchable-document-set-do-term-search
           #: consider adding a singleton the-search-default-arg
           to avoid copying default values to all the higher-level
           wrapper functions)

(define (searchable-document-set? it)
  (infix: it is-a? searchable-document-set<%>))

(define •do-term-search
  (generic searchable-document-set<%>
           do-term-search))

(define (send-do-term-search sds
                             norm-term
                             #:ricoeur-only? ricoeur-only?
                             #:languages st:lang
                             #:book/article book/article
                             #:exact? exact?)
  (send-generic sds
                •do-term-search
                norm-term
                #:ricoeur-only? ricoeur-only?
                #:languages st:lang
                #:book/article book/article
                #:exact? exact?))

(define (searchable-document-set-do-term-search
         sds
         term
         #:ricoeur-only? [ricoeur-only? #t]
         #:languages [raw-languages 'any]
         #:book/article [book/article 'any]
         #:exact? [exact? #f])
  (TODO/void searchable-document-set-do-term-search
             #: more checks)
  (send-do-term-search sds
                       (normalized-term term)
                       #:ricoeur-only? ricoeur-only?
                       #:languages
                       (match raw-languages
                         ['any
                          (seteq 'en 'fr 'de)]
                         [(? list?)
                          (list->seteq raw-languages)]
                         [else
                          (seteq raw-languages)])
                       #:book/article book/article
                       #:exact? exact?))


                                  
;                      ;     ;;    
;                      ;;    ;;    
;   ;;  ;;  ;; ;    ;;;;;  ;;;;;;; 
;   ;;  ;;  ;;; ;      ;;    ;;    
;   ;;  ;;  ;;  ;;     ;;    ;;    
;   ;;  ;;  ;;  ;;     ;;    ;;    
;   ;;  ;;  ;;  ;;     ;;    ;;    
;    ; ;;;  ;;  ;;     ;;     ;    
;     ; ;;  ;;  ;;     ;;      ;;; 
;                                  

;; workaround for https://github.com/racket/racket/issues/2459
(define cache:initialize-search-backend/c
  (make-weak-hasheq))
(define (make:initialize-search-backend/c search-backend/c)
  (cond
    [(hash-ref cache:initialize-search-backend/c search-backend/c #f)]
    [else
     (define rslt
       (-> search-backend/c
           (instance-set/c tei-document?)
           searchable-document-set?))
     (hash-set! cache:initialize-search-backend/c search-backend/c rslt)
     ;; (eprintf "make:initialize-search-backend/c: new for ~e\n" search-backend/c)
     rslt]))

(define-signature search^
  {(contracted
    [search-backend/c contract?])
   (define-values-for-export
     {initialize-search-backend/c}
     (make:initialize-search-backend/c search-backend/c))
   initialize-search-backend/c
   (contracted
    [initialize-search-backend
     ;; workaround for https://github.com/racket/racket/issues/2459
     (make:initialize-search-backend/c search-backend/c)])
   })

(define-for-syntax (unit-exporting-sig? unit-id wanted-sig-id)
  (with-handlers ([exn:fail:syntax? (λ (e) #f)])
    (match-define-values {_ exports}
      (unit-static-signatures unit-id #'unit-exporting-sig?))
    (define untaged-exports
      (filter-map (match-lambda
                    [(cons #f sig-id) sig-id]
                    [_ #f])
                  exports))
    (for/or ([sig-id (in-list untaged-exports)])
      (let loop ([sig-id sig-id])
        (and sig-id
             (or (free-identifier=? wanted-sig-id sig-id)
                 (match/values
                     (signature-members sig-id #'unit-exporting-sig?)
                   [{sig-id _ _ _}
                    (loop sig-id)])))))))

(begin-for-syntax
  (define-syntax-class search-unit-id
    #:description "search^ unit id"
    #:attributes {@ tag link-id predicate contract initialize/c initialize}
    (pattern @:id
             #:fail-unless (unit-exporting-sig? #'@ #'search^)
             "expected a search^ unit id from define-unit"
             #:with (tag link-id predicate contract initialize/c initialize)
             (generate-temporaries
              (map (λ (sym) (format-id #'@ "~a:~a" #'@ sym))
                   '(tag
                     link-id
                     predicate
                     contract
                     initialize/c
                     initialize))))))


(define (contract->predicate c)
  (if (flat-contract? c)
      (flat-contract-predicate c)
      (contract-first-order c)))

(define-syntax-parser define-compound-search-unit 
  [(_ final-search@:id member:search-unit-id ...+)
   #`(begin
       (define-unit combining-search@
         ;; see https://github.com/racket/racket/issues/2196
         (import (tag member.tag
                      (rename search^
                              [member.contract
                               search-backend/c]
                              [member.initialize/c
                               initialize-search-backend/c]
                              [member.initialize
                               initialize-search-backend]))
                 ...)
         (export search^)
         (init-depend (tag member.tag search^)
                      ...)
         (define search-backend/c
           (or/c member.contract ...))
         (define member.predicate
           (contract->predicate member.contract))
         ...
         (define (initialize-search-backend it docs)
           (cond
             [(member.predicate it)
              (member.initialize it docs)]
             ...)))
       (define-compound-unit/infer final-search@ 
         (import)
         (export combined)
         (link [([member.link-id : search^])
                member.@]
               ...
               [([combined : search^])
                combining-search@
                (tag member.tag member.link-id) ...])))])

(define-simple-macro (define-search-unit-binding static@:id dynamic@:expr)
  (define-unit-binding static@
    dynamic@
    (import)
    (export search^)))

(define-signature lazy-search^ extends search^ ())
(define/final-prop (lazy+eager-search-backend/c inner/c)
  (let ([inner/c (coerce-contract 'lazy+eager-search-backend/c inner/c)])
    (rename-contract
     (or/c inner/c (list/c 'eager inner/c))
     (build-compound-type-name 'lazy+eager-search-backend/c inner/c))))
    
(define-unit lazy+eager-search@
  (import (prefix eager: search^))
  (export lazy-search^)
  (init-depend search^)
  (define search-backend/c
    (lazy+eager-search-backend/c eager:search-backend/c))
  (define (initialize-search-backend raw-backend docs)
    (match raw-backend
      [(list 'eager backend)
       (eager:initialize-search-backend backend docs)]
      [backend
       (new lazy-searchable-document-set%
            [search-backend backend]
            [docs docs])]))
  (define lazy-searchable-document-set%
    (class* object% {searchable-document-set<%>}
      (super-new)
      (inspect #f)
      (init search-backend docs)
      (define promise:s-d-s
        (delay/thread
         (eager:initialize-search-backend search-backend docs)))
      (define/public (do-term-search norm-term
                                     #:ricoeur-only? r?
                                     #:languages st:lang
                                     #:book/article b/a
                                     #:exact? e?)
        (send-do-term-search (force promise:s-d-s)
                             norm-term
                             #:ricoeur-only? r?
                             #:languages st:lang
                             #:book/article b/a
                             #:exact? e?)))))


(define (lazy-search-unit eager*@)
  (define-search-unit-binding eager@ eager*@)
  (compound-unit/infer
   (import)
   (export combined)
   (link [([eager : search^]) eager@]
         [([combined : lazy-search^])
          lazy+eager-search@ eager])))

(define-syntax-parser define-lazy-search-unit
  [(_ lazy@:id eager@:search-unit-id)
   #'(define-search-unit-binding lazy@
       (lazy-search-unit eager@))])