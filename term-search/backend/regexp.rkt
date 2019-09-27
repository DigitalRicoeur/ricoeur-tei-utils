#lang racket/base

(require racket/contract
         racket/class
         racket/unit
         racket/list
         racket/string
         racket/set
         "common.rkt"
         (submod "common.rkt" private)
         ricoeur/tei/base)

(provide regexp@)

(define-unit-from-context regexp@ search^)

(module+ test
  (define-values/invoke-unit/infer regexp@))

(define search-backend/c
  'regexp)

(define (initialize-search-backend _ docs)
  (new regexp-searchable-document-set% [docs docs]))

(define regexp-searchable-document-set%
  (class* object% {searchable-document-set<%>}
    (super-new)
    (init [docs (instance-set)])
    (define s-docs:all
      (for/instance-set ([doc (in-instance-set docs)])
        (new searchable-document% [doc doc])))
    ;;;;;;;;;;
    (define/public-final (do-term-search norm-term
                                         #:ricoeur-only? ricoeur-only?
                                         #:languages st:lang
                                         #:book/article book/article
                                         #:exact? exact?)
      (def
        [book&article? (eq? 'any book/article)]
        [term-len
         (string-length (normalized-term-string norm-term))]
        [excerpt-px
         (normalized-term->excerpt-pregexp norm-term exact?)])
      (for/instance-set ([doc (in-instance-set s-docs:all)]
                         #:when (or book&article?
                                    (eq? book/article
                                         (instance-book/article doc)))
                         #:when (set-member? st:lang
                                             (instance-language doc))
                         [dsr/false (in-value
                                     (send doc
                                           do-term-search/doc
                                           term-len
                                           excerpt-px
                                           ricoeur-only?))]
                         #:when dsr/false)
        dsr/false))
    #|END class regexp-searchable-document-set%|#))


(define searchable-document%
  (class (instance-info-mixin object%)
    (init doc)
    (super-new [instance-info doc])
    (define segs
      (for/list ([base-seg (in-list (tei-document-segments doc))])
        (new searchable-segment%
             [base-seg base-seg])))
    (define excerpt-max-allow-chars
      (tei-document->excerpt-max-allow-chars doc))
    ;;;;;;;;;;
    (define/public-final (do-term-search/doc term-len
                                             excerpt-px
                                             ricoeur-only?)
      (define init-rslts
        (flatten
         (for/list ([seg (in-list segs)])
           (send seg do-term-search/segment excerpt-px ricoeur-only?))))
      (cond
        [(null? init-rslts)
         #f]
        [else
         (make-document-search-results
          this
          (limit-excess term-len init-rslts))]))
    (define/private (limit-excess term-len raw-results)
      (define max-exerpts
        (term-len->max-excerpts term-len))
      (cond
        [(infix: max-exerpts >= (length raw-results))
         raw-results]
        [else
         (define-values {ok to-nullify}
           (split-at raw-results max-exerpts))
         (append ok (map search-result-nullify-excerpt
                         to-nullify))]))
    (define/private (term-len->max-excerpts term-len)
      (define excerpt-max-length
        (+ EXCERPT_MAX_PEEK term-len))
      (inexact->exact
       (floor (/ excerpt-max-allow-chars
                 excerpt-max-length))))
    #|END class searchable-document%|#))


(define searchable-segment%
  (class object%
    (super-new)
    (init base-seg)
    (define meta
      (base-segment-meta base-seg))
    (define normalized-body
      (string->immutable-string
       (string-normalize-spaces
        (base-segment-body base-seg))))
    (define this-by-ricoeur?
      (segment-by-ricoeur? meta))
    ;;;;;;;;;;
    (define/public-final (do-term-search/segment excerpt-px
                                                 ricoeur-only?)
      (list-when (or this-by-ricoeur? (not ricoeur-only?))
        (segment-make-search-results
         meta
         (for/list ([raw-excerpt
                     (in-list (regexp-match* excerpt-px
                                             normalized-body))])
           (just (string->immutable-string raw-excerpt))))))
    #|END class searchable-segment%|#))
    

(define-values {EXCERPT_MAX_PEEK
                normalized-term->excerpt-pregexp}
  (let ()
    (define/contract EXCERPT_MAX_PEEK
      ;; the maximum number of characters,
      ;; outside of the term itself,
      ;; included in a given excerpt
      (and/c natural-number/c even?)
      160)
    (def
      [one-side-peek (/ EXCERPT_MAX_PEEK 2)]
      [before (format "(?:^|\\s).{,~a}" one-side-peek)]
      [after (format ".{,~a}(?:\\s|$)" one-side-peek)])
    (values
     EXCERPT_MAX_PEEK
     (λ (norm-term exact?)
       (pregexp
        (string-append before
                       (pregexp-quote-normalized-term
                        norm-term #:exact? exact?)
                       after))))))


