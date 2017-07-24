#lang racket/base

(require ricoeur/tei/base
         adjutor
         data/maybe
         racket/serialize
         racket/class
         racket/match
         racket/list
         racket/contract
         racket/string
         (for-syntax racket/base
                     syntax/parse
                     adjutor
                     ))

(provide term/c
         searchable-document?
         document-search-results<%>
         document-search-results
         search-result?
         (rename-out [search-result* search-result])
         (contract-out
          [prepare-searchable-document
           (-> (is-a?/c TEI<%>) searchable-document?)]
          [search-documents
           (-> term/c (listof searchable-document?)
               (listof (is-a?/c document-search-results<%>)))]
          [document-search-results-title
           (-> (is-a?/c document-search-results<%>)
               string?)]
          [document-search-results-results
           (-> (is-a?/c document-search-results<%>)
               (listof search-result?))]
          [search-result<?
           (-> search-result? search-result? any/c)]
          [search-result>?
           (-> search-result? search-result? any/c)]
          [search-result-excerpt
           (-> search-result? (maybe/c string?))]
          [search-result-page-string 
           (-> search-result? (maybe/c string?))]
          ))

(define term/c
  (and/c non-empty-string? (not/c #px"^\\s*$")))

(define/contract (term->excerpt-pregexp term)
  (-> term/c pregexp?)
  (pregexp
   (string-append "(?:^|\\s).{,80}"
                  (regexp-quote (string-normalize-spaces term) #f)
                  ".{,80}(?:\\s|$)")))

(define EXCERPT_MAX_PEEK
  ;the maximum number of characters,
  ;outside of the term itself, included in a given excerpt
  160)

(define EXCERPT_RATIO
  ;the maximum portion of the document that may be
  ;shown in excerpts
  18/100)

(define (prepare-searchable-document obj)
  (new searchable-document%
       [obj obj]
       [segments
        (let loop ([to-go (send obj smoosh)] ;(listof (or/c string? (is-a?/c pb<%>)))
                   [counter 0] ;natural-number/c ;this counts segments
                   [pb (tag->element '(pb))] ;(is-a?/c pb<%>)
                   [this-so-far null]) ;(listof string?)
          (match to-go
            ['()
             (cond [(null? this-so-far) null]
                   [else
                    (list (new searchable-segment%
                               [str (string-normalize-spaces
                                     (string-join (reverse this-so-far)
                                                  " "))]
                               [counter counter]
                               [pb pb]))])]
            [(cons (? (is-a?/c pb<%>) next-pb) more)
             (cond [(null? this-so-far)
                    (loop more (add1 counter) next-pb null)]
                   [else
                    (cons (new searchable-segment%
                               [str (string-normalize-spaces
                                     (string-join (reverse this-so-far) ""))]
                               [counter counter]
                               [pb pb])
                          (loop more (add1 counter) next-pb null))])]
            [(cons (? string? this) more)
             (loop more counter pb (cons this this-so-far))]))]))

(define TEI-info-mixin 
  (mixin () (TEI-info<%>)
    (super-new)
    (abstract get-TEI-info)
    (public*
     [get-title (λ () (send (get-TEI-info) get-title))]
     [get-publication-date (λ () (send (get-TEI-info) get-publication-date))]
     [get-citation (λ () (send (get-TEI-info) get-citation))])))



;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;                                           ;;              ;;      ;;;;           
;                                           ;;              ;;        ;;           
;     ;;      ;;;     ;;    ;; ;;;     ;;;  ;; ;      ;;    ;;;;      ;;      ;;;  
;   ;;  ;   ;;   ;   ;  ;   ;;;      ;;   ; ;;; ;    ;  ;   ;;  ;     ;;    ;;   ; 
;    ;      ;    ;      ;;  ;;       ;      ;;  ;;      ;;  ;;  ;     ;;    ;    ; 
;     ;;   ;;;;;;;;   ;;;;  ;;      ;;      ;;  ;;    ;;;;  ;;  ;;    ;;   ;;;;;;;;
;       ;;  ;        ;  ;;  ;;       ;      ;;  ;;   ;  ;;  ;;  ;     ;;    ;      
;   ;   ;   ;;   ;  ;;  ;;  ;;       ;;   ; ;;  ;;  ;;  ;;  ;;  ;      ;    ;;   ; 
;    ;;;      ;;;    ;;; ;  ;;         ;;;  ;;  ;;   ;;; ;  ; ;;        ;;    ;;;  
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  


;(define-member-name do-term-search (generate-member-key))

(define searchable-document<%>
  (interface (TEI-info<%>)
    [do-term-search
     (->m natural-number/c pregexp?
          (recursive-contract
           (is-a?/c document-search-results<%>)))]))

(define searchable-document?
  (is-a?/c searchable-document<%>))

(define searchable-segment%
  (class object%
    (super-new)
    (init pb)
    (init-field str
                counter)
    (define page-string
      (send pb get-page-string))
    (define/public (do-term-search/segment excerpt-px)
      (for/list ([excerpt (in-list (regexp-match* excerpt-px str))]
                 [sub-counter (in-naturals)])
        (search-result counter
                       sub-counter
                       (just (string-trim excerpt))
                       page-string)))))


(define searchable-document%
  (class* (TEI-info-mixin object%) [searchable-document<%>]
    (super-new)
    (init obj
          [(:segments segments)])
    (define segments
      :segments)
    (define teiHeader
      (send obj get-teiHeader))
    (define/override (get-TEI-info)
      teiHeader)
    (define excerpt-max-allow-chars
      (let ([doc-chars (string-length (string-normalize-spaces
                                       (send obj to-plain-text)))])
        (* EXCERPT_RATIO doc-chars)))
    (define/public (do-term-search term-len excerpt-px)
      (new document-search-results%
           [info teiHeader]
           [results
            (limit-excess
             term-len
             (flatten
              (for/list ([seg (in-list segments)])
                (send seg do-term-search/segment excerpt-px))))]))
    (define/private (term-len->max-excerpts term-len)
      (define excerpt-max-length
        (+ EXCERPT_MAX_PEEK term-len))
      (inexact->exact
       (floor (/ excerpt-max-allow-chars
                 excerpt-max-length))))
    (define/private (limit-excess term-len raw-results)
      (define max-exerpts
        (term-len->max-excerpts term-len))
      (cond
        [(max-exerpts . >= . (length raw-results))
         raw-results]
        [else
         (define-values {ok to-nullify}
           (split-at raw-results max-exerpts))
         ;(displayln (cons (length raw-results) (length to-nullify)))
         (append ok
                 (for/list ([raw (in-list to-nullify)])
                   (struct-copy search-result raw
                                [excerpt nothing])))]))
    #|END searchable-document%|#))


(define (search-documents term l-docs)
  (let ([term-len (string-length term)]
        [excerpt-px (term->excerpt-pregexp term)])
    (map (λ (doc) (send doc do-term-search term-len excerpt-px))
         l-docs)))

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
    [get-results (->m (listof (recursive-contract search-result?)))]
    private-method))

(define document-search-results%
  (class* (TEI-info-mixin object%) [document-search-results<%>]
    (super-new)
    (init [(:info info)]
          [(:results results)])
    (def
      [info :info]
      [results :results])
    (define/override (get-TEI-info)
      info)
    (define/public (get-results)
      results)
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
             (~optional (~seq #:citation cite-pate)))
        ...)
     #`(? document-search-results?
          (and (app document-search-results-title title-pat)
               (app document-search-results-results results-pat)
               #,@(list-when (attribute date-pat)
                    (list #'(app (λ (it) (send it get-publication-date))
                                 date-pat)))
               #,@(list-when (attribute cite-pate)
                    (list #'(app (λ (it) (send it get-citation))
                                 cite-pate)))))]))

(serializable-struct search-result (counter
                                    sub-counter
                                    excerpt
                                    page-string
                                    )
  #:transparent)
  

(define ((make-search-result-cf cf) a b)
  (match-define (search-result a:counter a:sub-counter _ _)
    a)
  (match-define (search-result b:counter b:sub-counter _ _)
    b)
  (or [a:counter . cf . b:counter]
      (and (= a:counter b:counter)
           [a:sub-counter . cf . b:sub-counter])))

(define search-result<?
  (make-search-result-cf <))

(define search-result>?
  (make-search-result-cf >))

(define-match-expander search-result*
  (syntax-parser
    [(_ (~or (~optional (~seq #:excerpt excerpt)
                        #:defaults ([excerpt #'_]))
             (~optional (~seq #:page-string page-string)
                        #:defaults ([page-string #'_])))
        ...)
     #'(search-result _ _ excerpt page-string)]))

#|
(module+ main
  (define s
    (prepare-searchable-document
     (call-with-input-file
         "/Users/philip/code/ricoeur/texts/TEI/ideology-and-utopia.xml"
       read-TEI)))
  (search-documents "ideology" (list s)))
|#
