#lang _-exp racket

(require ricoeur/tei/base
         (submod ricoeur/tei/oop/interfaces search)
         data/maybe
         json
         racket/serialize
         adjutor
         (for-syntax syntax/parse
                     adjutor
                     ))

(module+ test
  (require rackunit
           (submod "..")))

(provide term/c
         searchable-document-set?
         document-search-results<%>
         document-search-results
         search-result?
         search-result
         location-stack-entry/c
         location-stack/c
         (contract-out
          [search-documents
           (->* {term/c searchable-document-set?}
                (#:ricoeur-only? any/c
                 #:book/article (or/c 'any 'book 'article)
                 #:exact? any/c)
                (listof (is-a?/c document-search-results<%>)))]
          [document-search-results-title
           (-> (is-a?/c document-search-results<%>)
               string?)]
          [document-search-results-results
           (-> (is-a?/c document-search-results<%>)
               (non-empty-listof search-result?))]
          [search-result<?
           (-> search-result? search-result? any/c)]
          [search-result>?
           (-> search-result? search-result? any/c)]
          [search-result-author-string
           (-> search-result? string?)]
          [search-result-excerpt
           (-> search-result? (maybe/c non-empty-string?))]
          [search-result-page
           (-> search-result?
               (or/c (maybe/c string?)
                     (list/c (maybe/c string?) (maybe/c string?))))]
          [search-result-location-stack
           (-> search-result? location-stack/c)]
          ))

(module+ private
  (provide EXCERPT_RATIO
           exact?-px-prefix-str
           exact?-px-suffix-str
           abstract-searchable-document-set%
           pre-segment-meta/c
           (contract-out
            [document-search-results%
             (class/c (init [info (is-a?/c TEI-info<%>)]
                            [results (non-empty-listof search-result?)]))]
            [struct pre-segment ([title string?]
                                 [counter natural-number/c]
                                 [body string?]
                                 [meta pre-segment-meta/c]
                                 [resp #rx"^#.+"])
             #:omit-constructor]
            [prepare-pre-segments
             (-> (is-a?/c TEI<%>)
                 (listof pre-segment?))]
            [make-search-result
             (-> #:counter natural-number/c
                 #:sub-counter natural-number/c
                 #:meta pre-segment-meta/c
                 #:excerpt (maybe/c non-empty-string?)
                 search-result?)]
            [nullify-search-result-excerpt
             (-> search-result? search-result?)])
           ))
          

(define term/c
  (and/c non-empty-string? #px"\\S"))

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

(struct pre-segment (title counter body meta resp))

(define current-resp
  (make-parameter "#ricoeur"))

(define location-stack/c
  (opt/c
   (flat-murec-contract ([tail/c (or/c (list/c)
                                       (list/c 'front)
                                       (list/c 'back))]
                         [with-divs/c
                          (or/c tail/c
                                (cons/c location-stack-entry:div/c
                                        with-divs/c))]
                         [location-stack/c
                          (or/c with-divs/c
                                (cons/c location-stack-entry:note/c
                                        location-stack/c))])
                        location-stack/c)))

(define location-stack-jsexpr/c
  (opt/c
   (flat-murec-contract ([tail/c (or/c (list/c)
                                       (list/c "front")
                                       (list/c "back"))]
                         [with-divs/c
                          (or/c tail/c
                                (cons/c (list/c "div"
                                                (or/c "chapter" "part" "section" "dedication"
                                                      "contents" "intro" "bibl" "ack" "index")
                                                (or/c #f string?))
                                        with-divs/c))]
                         [location-stack-js/c
                          (or/c with-divs/c
                                (cons/c (list/c "note" (or/c "foot" "end") string? (or/c "transl" #f))
                                        location-stack-js/c))])
                        (and/c jsexpr? location-stack-js/c))))

(define/contract current-location-stack
  (parameter/c location-stack/c)
  (make-parameter '()))

(define (call-with-metadata thunk
                            #:resp [resp #f]
                            #:location [location #f])
  (parameterize ([current-resp (or resp (current-resp))]
                 [current-location-stack (if location
                                             (cons location
                                                   (current-location-stack))
                                             (current-location-stack))])
    (thunk)))

(define/contract (location-stack->jsexpr stack)
  (-> location-stack/c location-stack-jsexpr/c)
  (map (match-lambda
         ['front "front"]
         ['back "back"]
         [(list 'div type n)
          (list "div" type (from-just #f n))]
         [(list 'note place n transl)
          (list "note" place n transl)])
       stack))

(define/contract (jsexpr->location-stack stack)
  (-> location-stack-jsexpr/c location-stack/c)
  (map (match-lambda
         ["front" 'front]
         ["back" 'back]
         [(list "div" type n)
          (list 'div type (false->maybe n))]
         [(list "note" place n transl)
          (list 'note place n transl)])
       stack))

(define pre-segment-meta/c
  (opt/c
   (and/c jsexpr?
          (let ([page/c (or/c #f
                              string?
                              (list/c (or/c #f string?)
                                      (or/c #f string?)))])
            (hash/dc [k (or/c 'resp 'location-stack 'page)]
                     [v (k) (case k
                              [(resp) #rx"#.+"]
                              [(location-stack) location-stack-jsexpr/c]
                              [(page) page/c]
                              [else none/c])]
                     #:immutable #t
                     #:kind 'flat))
          (λ (hsh)
            (hash-keys-subset? #hasheq([resp . #t]
                                       [location-stack . #t]
                                       [page . #t])
                               hsh)))))

(define/contract (make-pre-segment #:title title
                                   #:counter counter
                                   #:body body
                                   #:page page) 
  (-> #:title string?
      #:counter natural-number/c
      #:body string?
      #:page (or/c (maybe/c string?)
                   (list/c (maybe/c string?) (maybe/c string?)))
      pre-segment?)
  (let ([resp (current-resp)])
    (pre-segment title counter body
                 (hasheq 'resp resp
                         'location-stack (location-stack->jsexpr
                                          (current-location-stack))
                         'page (match page
                                 [(list a b)
                                  (list (from-just #f a)
                                        (from-just #f b))]
                                 [it
                                  (from-just #f it)]))
                 resp)))

(struct pre-segment-accumulator (title next-counter so-far)
  #:property prop:procedure
  (λ (this #:body body
           #:page page) 
    (cond
      [(regexp-match? #px"^\\s*$"  body)
       this]
      [else
       (match-define (pre-segment-accumulator title next-counter so-far)
         this)
       (pre-segment-accumulator title
                                (add1 next-counter)
                                (cons (make-pre-segment #:title title
                                                        #:counter next-counter
                                                        #:body body
                                                        #:page page)
                                      so-far))])))

(define (title->pre-segment-accumulator t)
  (pre-segment-accumulator t 0 null))


                 

(define (prepare-pre-segments doc)
  (reverse
   (pre-segment-accumulator-so-far
    (send doc
          do-prepare-pre-segments
          pre-segment-accumulator?
          call-with-metadata
          title->pre-segment-accumulator))))

;                                                          
;                                                          
;                                                          
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
;                                                          
;


(define-member-name private-method (generate-member-key))

(define document-search-results<%>
  (interface (TEI-info<%>)
    [get-results (->m (non-empty-listof (recursive-contract search-result?)))]
    [count-results (->m exact-positive-integer?)]
    private-method))

(define document-search-results%
  (class* object% [document-search-results<%>]
    (super-new)
    (init [(:info info)]
          [(:results results)])
    (define/TEI-info info :info)
    (def
      [results :results]
      [count (length results)])
    (for ([rslt (in-list results)])
      (send rslt initialize-search-result this))
    (define/public-final (get-results)
      results)
    (define/public-final (count-results)
      count)
    (define/public-final (private-method)
      (void))
    #|END document-search-results%|#))

(define (document-search-results-title d)
  (send d get-title))

(define (document-search-results-results d)
  (send d get-results))

(define document-search-results?
  (is-a?/c document-search-results<%>))

(define-match-expander document-search-results
  (syntax-parser
    [(_ title-pat results-pat
        (~or (~optional (~seq #:date date-pat))
             (~optional (~seq #:count count-pat))
             (~optional (~seq #:citation cite-pate)))
        ...)
     #`(? document-search-results?
          (and (app document-search-results-title title-pat)
               (app document-search-results-results results-pat)
               #,@(list-when (attribute date-pat)
                    (list #'(app (λ (it) (send it get-publication-date))
                                 date-pat)))
               #,@(list-when (attribute count-pat)
                    (list #'(app (λ (it) (send it count-results))
                                 count-pat)))
               #,@(list-when (attribute cite-pate)
                    (list #'(app (λ (it) (send it get-citation))
                                 cite-pate)))))]))





(define-member-name get-counter (generate-member-key))

(define-member-name get-sub-counter (generate-member-key))

(define-member-name nullify-excerpt (generate-member-key))

(define-member-name initialize-search-result (generate-member-key))

(define search-result%
  (class object%
    (super-new)
    (define-syntax init-private-fields
      (syntax-parser
        [(_ name:id)
         #`(begin (init [(tmp name)])
                  (define name tmp))]
        [(_ [name:id default:expr])
         #`(begin (init [(tmp name) default])
                  (define name tmp))]
        [(_ spec ...+)
         #`(begin (init-private-fields spec) ...)]))
    (init-private-fields counter
                         sub-counter
                         excerpt
                         meta
                         [promise:page
                          (delay
                            (match (hash-ref meta 'page)
                              [(list a b)
                               (list (false->maybe a)
                                     (false->maybe b))]
                              [plain
                               (false->maybe plain)]))]
                         [promise:location-stack
                          (delay (jsexpr->location-stack
                                  (hash-ref meta 'location-stack)))]
                         [resp-string-field #f]
                         )
    (define/public (initialize-search-result info)
      (set! resp-string-field
            (delay (let ([ptr (hash-ref meta 'resp)])
                     (if (equal? ptr "#ricoeur")
                         "Paul Ricœur"
                         (send info
                               get-resp-string
                               (string->symbol (substring ptr 1))))))))
    (define/public (nullify-excerpt)
      (new this%
           [counter counter]
           [sub-counter sub-counter]
           [excerpt (nothing)]
           [meta meta]
           [promise:page promise:page]
           [promise:location-stack promise:location-stack]
           [resp-string-field resp-string-field]
           ))
    (define/public-final (get-author-string)
      (or (force resp-string-field)
          (error 'get-author-string "used before initialization")))
    (define/public-final (get-counter)
      counter)
    (define/public-final (get-sub-counter)
      sub-counter)
    (define/public-final (get-excerpt)
      excerpt)
    (define/public-final (get-page)
      (force promise:page))
    (define/public-final (get-location-stack)
      (force promise:location-stack))
    #|END search-result%|#))

(define (make-search-result #:counter counter
                            #:sub-counter sub-counter
                            #:meta meta
                            #:excerpt excerpt)
  (new search-result%
       [counter counter]
       [sub-counter sub-counter]
       [meta meta]
       [excerpt excerpt]))


(define (search-result-excerpt sr)
  (send sr get-excerpt))

(define (search-result-page sr)
  (send sr get-page))

(define (search-result-location-stack sr)
  (send sr get-location-stack))

(define (search-result-author-string sr)
  (send sr get-author-string))

(define search-result?
  (is-a?/c search-result%))

(define ((make-search-result-cf cf) a b)
  (let ([a:counter (send a get-counter)]
        [b:counter (send b get-counter)])
    (or [a:counter . cf . b:counter]
        (and (= a:counter b:counter)
             (let ([a:sub-counter (send a get-sub-counter)]
                   [b:sub-counter (send b get-sub-counter)])
               [a:sub-counter . cf . b:sub-counter])))))

(define (nullify-search-result-excerpt raw)
  (send raw nullify-excerpt))

(define search-result<?
  (make-search-result-cf <))

(define search-result>?
  (make-search-result-cf >))

(define-match-expander search-result
  (syntax-parser
    [(_ (~or (~optional (~seq #:excerpt excerpt))
             (~optional (~seq #:location-stack loc))
             (~optional (~seq #:author resp))
             (~optional (~seq #:page page)))
        ...)
     #`(? search-result?
          (and #,@(list-when (attribute excerpt)
                    (list #`(app search-result-excerpt excerpt)))
               #,@(list-when (attribute loc)
                    (list #`(app search-result-location-stack loc)))
               #,@(list-when (attribute resp)
                    (list #`(app search-result-author-string resp)))
               #,@(list-when (attribute page)
                    (list #`(app search-result-page page)))))]))

(define abstract-searchable-document-set%
  (class* object% [(interface ()
                     [do-search-documents
                      (->*m {term/c}
                            (#:ricoeur-only? any/c
                             #:book/article (or/c 'any 'book 'article)
                             #:exact? any/c)
                            (listof (is-a?/c document-search-results<%>)))])]
    (super-new)
    (abstract do-search-documents)))

(define searchable-document-set?
  (is-a?/c abstract-searchable-document-set%))

(define (search-documents term
                          sds
                          #:ricoeur-only? [ricoeur-only? #t]
                          #:book/article [book/article 'any]
                          #:exact? [exact? #f])
  (send sds do-search-documents
        term
        #:ricoeur-only? ricoeur-only?
        #:book/article book/article
        #:exact? exact?))



