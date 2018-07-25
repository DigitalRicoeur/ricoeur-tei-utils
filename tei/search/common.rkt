#lang racket

(require racket/contract
         data/order
         syntax/parse/define
         ricoeur/tei/base
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/match
                     racket/list
                     racket/unit-exptime
                     ))

(module+ test
  (require rackunit
           #;(submod "..")))

(provide term/c
         searchable-document-set?
         document-search-results?
         ;document-search-results ;; match expander
         search-result?
         ;search-result ;; match expander
         (contract-out
          [search-result-<?
           (-> search-result? search-result? any/c)]
          [search-result->?
           (-> search-result? search-result? any/c)]
          [search-result-excerpt
           (-> search-result?
               (maybe/c (and/c string-immutable/c
                               trimmed-string-px)))]
          [searchable-document-set-do-term-search
           (->* {searchable-document-set?
                 term/c}
                {#:ricoeur-only? any/c
                 #:book/article (or/c 'any 'book 'article)
                 #:exact? any/c}
                (listof document-search-results?))]
          ))

(module+ private
  (provide EXCERPT_RATIO
           normalized-term?
           ;exact?-px-prefix-str
           ;exact?-px-suffix-str
           searchable-document-set<%>
           search^
           define-values/invoke-search-units/infer
           (contract-out
            [normalized-term-string
             (-> normalized-term?
                 (and/c term/c
                        trimmed-string-px))]
            [segment-make-search-results
             (-> segment?
                 (listof (maybe/c (and/c string?
                                         #px"\\S")))
                 (listof search-result?))]
            [search-result-nullify-excerpt
             (-> search-result? search-result?)]
            [make-document-search-results
             (-> instance-info?
                 (non-empty-listof search-result?)
                 document-search-results?)]
            )))


(define/final-prop term/c
  ;; Since #px"\\S" has security implications,
  ;; we have to care that it's sound.
  (and/c string-immutable/c #px"\\S"))

(define/final-prop trimmed-string-px
  #px"^\\S$|^\\S.*\\S$")

(struct normalized-term (string)
  #:transparent
  #:guard (λ (term name)
            (string->immutable-string
             (values ;string-normalize-spaces
              (string-trim term)))))

(define EXCERPT_RATIO
  ;the maximum portion of the document that may be
  ;shown in excerpts
  18/100)

(define-values (exact?-px-prefix-str exact?-px-suffix-str)
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
  (let ([categories "\\p{P}|\\p{Z}|\\p{N}|\\p{Sc}|\\p{Sm}|\\p{Cc}"])
    (values (string-append "(?:^|" categories ")")
            (string-append "(?:" categories "|$)"))))


(module+ test
  (let ([exact-apple-px (pregexp (string-append exact?-px-prefix-str
                                                (regexp-quote "apple" #f)
                                                exact?-px-suffix-str))])
    (check-true (regexp-match? exact-apple-px "apple"))
    (check-true (regexp-match? exact-apple-px "apPle"))
    (check-true (regexp-match? exact-apple-px " apPle."))
    (check-true (regexp-match? exact-apple-px "$apPle1"))
    (check-false (regexp-match? exact-apple-px "appleé"))))

                                                       
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
          #:book/article (or/c 'any 'book 'article)
          #:exact? any/c
          (listof document-search-results?))]))

(define (searchable-document-set? it)
  (infix: it is-a? searchable-document-set<%>))

(define •do-term-search
  (generic searchable-document-set<%>
           do-term-search))

(define (searchable-document-set-do-term-search
         sds
         term
         #:ricoeur-only? [ricoeur-only? #t]
         #:book/article [book/article 'any]
         #:exact? [exact? #f])
  (TODO/void searchable-document-set-do-term-search
             #: more checks)
  (send-generic sds
                •do-term-search
                (normalized-term term)
                #:ricoeur-only? ricoeur-only?
                #:book/article book/article
                #:exact? exact?))

;                                                  
;                                                  
;                                                  
;                                                  
;                            ;;                    
;                            ;;                    
;     ;;   ;     ;  ;; ;   ;;;;;;;    ;;   ;;   ;; 
;   ;;  ;   ;   ;   ;;; ;    ;;      ;  ;    ;  ;  
;    ;      ;   ;   ;;  ;;   ;;         ;;   ; ;   
;     ;;     ;  ;   ;;  ;;   ;;       ;;;;    ;    
;       ;;   ; ;    ;;  ;;   ;;      ;  ;;   ; ;   
;   ;   ;    ; ;    ;;  ;;    ;     ;;  ;;  ;   ;  
;    ;;;      ;     ;;  ;;     ;;;   ;;; ; ;;   ;; 
;             ;                                    
;            ;                                     
;          ;;                                      
;    

(define-signature search^
  {initialize-search-backend/c
   (contracted
    [search-backend/c contract?]
    [initialize-search-backend
     initialize-search-backend/c])
   (define-values-for-export
     {initialize-search-backend/c}
     (-> search-backend/c
         (listof tei-document?)
         searchable-document-set?))})

(define (contract->predicate c)
  (if (flat-contract? c)
      (flat-contract-predicate c)
      (contract-first-order c)))

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
                     (signature-members sig-id
                                        #'unit-exporting-sig?)
                   [{sig-id _ _ _}
                    (loop sig-id)])))))))

(begin-for-syntax
  (define-syntax-class search-unit-id
    #:description "search^ unit id"
    #:attributes {@ tag link-id predicate contract initialize}
    (pattern @:id
             #:fail-unless (unit-exporting-sig? #'@
                                                #'search^)
             "expected a search^ unit id from define-unit"
             #:with (tag link-id predicate contract initialize)
             (generate-temporaries
              (map (λ (sym) (format-id #'@ "~a:~a" #'@ sym))
                   '(tag
                     link-id
                     predicate
                     contract
                     initialize))))))


(define-syntax-parser define-values/invoke-search-units/infer 
  [(_ member:search-unit-id ...+)
   #`(begin
       (define-unit combining-search@
         (import (tag member.tag
                      (rename search^
                              [member.contract
                               search-backend/c]
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
                (tag member.tag member.link-id) ...]))
       #,(datum->syntax
          this-syntax
          (list #'define-values/invoke-unit/infer
                #'final-search@)))])


