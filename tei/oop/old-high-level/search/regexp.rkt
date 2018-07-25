#lang racket/base

(require ricoeur/tei/oop-base
         adjutor
         data/maybe
         racket/serialize
         racket/class
         racket/match
         racket/list
         racket/contract
         racket/string
         "common.rkt"
         (submod "common.rkt" private)
         (for-syntax racket/base
                     syntax/parse
                     adjutor
                     ))

(module+ test
  (require rackunit
           (submod "..")))

(provide (contract-out
          [regexp-searchable-document-set
           (->* {}
                {(listof (is-a?/c TEI<%>))}
                searchable-document-set?)]
          ))

(define/contract (term->excerpt-pregexp term [exact? #f])
  (->* {term/c} {any/c} any) ; an invalid term could have security implications
  (pregexp
   (string-append "(?:^|\\s).{,80}"
                  (string-when exact?
                    exact?-px-prefix-str)
                  (regexp-quote (string-normalize-spaces term) #f)
                  (string-when exact?
                    exact?-px-suffix-str)
                  ".{,80}(?:\\s|$)")))

(define EXCERPT_MAX_PEEK
  ;the maximum number of characters,
  ;outside of the term itself, included in a given excerpt
  160)

(define regexp-searchable-document-set%
  (class abstract-searchable-document-set%
    (super-new)
    (init [docs '()])
    (define searchable
      (map prepare-searchable-document docs))
    (define/override-final (do-search-documents term
                                                #:ricoeur-only? [ricoeur-only? #t]
                                                #:book/article [book/article 'any]
                                                #:exact? [exact? #f])
      (let ([term-len (string-length term)]
            [excerpt-px (term->excerpt-pregexp term exact?)])
        (filter-map (λ (doc)
                      (send doc
                            do-term-search
                            term-len
                            excerpt-px
                            ricoeur-only?))
                    (case book/article
                      [(any) searchable]
                      [else (filter (λ (it)
                                      (eq? book/article
                                           (send it get-book/article)))
                                    searchable)]))))))

(define (regexp-searchable-document-set [docs '()])
  (new regexp-searchable-document-set%
       [docs docs]))

(define (prepare-searchable-document obj)
  (new searchable-document%
       [obj obj]
       [segments
        (for/list ([pre (in-list (prepare-pre-segments obj))])
          (new searchable-segment%
               [pre pre]))]))

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

(define searchable-segment%
  (class object%
    (super-new)
    (init pre)
    (match-define (pre-segment _ counter body meta resp)
      pre)
    (define this-by-ricoeur?
      (equal? resp "#ricoeur"))
    (define/public (do-term-search/segment excerpt-px ricoeur-only?)
      (cond
        [(or this-by-ricoeur? (not ricoeur-only?))
         (for/list ([excerpt (in-list (regexp-match* excerpt-px body))]
                    [sub-counter (in-naturals)])
           (make-search-result #:counter counter
                               #:sub-counter sub-counter
                               #:excerpt (just (string-trim excerpt))
                               #:meta meta))]
        [else
         null]))))


(define searchable-document%
  (class* object% [(interface (TEI-info<%>)
                     [do-term-search
                      (->m natural-number/c pregexp? any/c
                           (or/c #f (is-a?/c document-search-results<%>)))])]
    (super-new)
    (init obj
          [(:segments segments)])
    (define segments
      :segments)
    (define/TEI-info teiHeader
      (send obj get-teiHeader))
    (define excerpt-max-allow-chars
      (let ([doc-chars (string-length (string-normalize-spaces
                                       (send obj to-plain-text)))])
        (* EXCERPT_RATIO doc-chars)))
    (define/public (do-term-search term-len excerpt-px ricoeur-only?)
      (define init-rslts
        (flatten
         (for/list ([seg (in-list segments)])
           (send seg do-term-search/segment excerpt-px ricoeur-only?))))
      (cond
        [(null? init-rslts)
         #f]
        [else
         (new document-search-results%
              [info teiHeader]
              [results
               (limit-excess
                term-len
                init-rslts)])]))
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
         (append ok
                 (for/list ([raw (in-list to-nullify)])
                   (nullify-search-result-excerpt raw)))]))
    #|END searchable-document%|#))