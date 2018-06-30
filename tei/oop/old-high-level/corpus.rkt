#lang racket

(require ricoeur/tei/oop-base
         "search.rkt"
         adjutor
         )

(module+ test
  (require rackunit))

(provide postgresql-data-source/c
         empty-corpus
         (except-out (all-from-out "search.rkt")
                     search-documents
                     searchable-document-set?
                     noop-searchable-document-set
                     regexp-searchable-document-set
                     postgresql-searchable-document-set)
         (contract-out
          [corpus%
           (class/c (init [docs (listof (is-a?/c TEI<%>))]
                          [search-backend search-backend/c]))]
          [directory-corpus%
           (class/c (init [path (and/c path-string? directory-exists?)]
                          [search-backend search-backend/c]))]
          [current-corpus
           (parameter/c (is-a?/c corpus%))]
          [term-search
           (->* {term/c}
                {#:ricoeur-only? any/c
                 #:book/article (or/c 'any 'book 'article)
                 #:exact? any/c}
                (listof (is-a?/c document-search-results<%>)))]
          [list-TEI-info
           (-> (listof (is-a?/c TEI-info<%>)))]
          [get-checksum-table
           (-> (hash/c string?
                       string?
                       #:immutable #t))]
          ))

(define search-backend/c
  (or/c #f 'noop postgresql-data-source/c))

(define-local-member-name resolved-docs)

(define/contract corpus%
  (class/c (init [docs (listof (is-a?/c TEI<%>))]
                 [search-backend search-backend/c]))
  (class* object% [(interface ()
                     [•list-TEI-info (->m (listof (is-a?/c TEI-info<%>)))]
                     [•get-checksum-table (->m (hash/c string?
                                                       string?
                                                       #:immutable #t))]
                     [on-initialize (->m (listof (is-a?/c TEI<%>))
                                         any)]
                     [•term-search
                      (->*m {term/c}
                            {#:ricoeur-only? any/c
                             #:book/article (or/c 'any 'book 'article)
                             #:exact? any/c}
                            (listof (is-a?/c document-search-results<%>)))])]
    (super-new)
    (init [(init:docs docs) '()]
          [search-backend #f]
          [(docs resolved-docs) (deduplicate-docs init:docs)])
    (for/fold/define ([headers '()])
                     ([doc (in-list docs)])
      (values (cons (send doc get-teiHeader)
                    headers)))
    (define pr:searchable-document-set
      (delay/thread/eager-errors
       ;#:handler ;TODO
       (cond
         [(eq? 'noop search-backend)
          noop-searchable-document-set]
         [search-backend
          (postgresql-searchable-document-set docs #:db search-backend)]
         [else
          (regexp-searchable-document-set docs)])))
    (define pr:checksum-table
      (delay/thread
       (for/hash ([doc (in-list docs)])
         (values (send doc get-title)
                 (send doc get-md5)))))
    ;; Must come at the end of initialization
    (parameterize ([currently-initializing? #t])
      (on-initialize docs))
    ;; Methods:
    (define/pubment (on-initialize docs)
      (unless (currently-initializing?)
        (raise-arguments-error 'on-initialize
                               "cannot be called except during corpus% initialization"
                               "corpus% instance" this
                               "argument" docs))
      (parameterize ([currently-initializing? #f])
        (inner (void) on-initialize docs)))
    (define/public-final (•get-checksum-table)
      (force pr:checksum-table))
    (define/public-final (•list-TEI-info)
      headers)
    (define/public-final (•term-search raw-term
                                       #:ricoeur-only? [ricoeur-only? #t]
                                       #:book/article [book/article 'any]
                                       #:exact? [exact? #f])
      (define term
        (string-normalize-spaces raw-term))
      (search-documents term (force pr:searchable-document-set)
                        #:exact? exact?
                        #:book/article book/article
                        #:ricoeur-only? ricoeur-only?))
    (define/private (deduplicate-docs docs)
      (remove-duplicates docs
                         #:key (λ (doc) (send doc get-title))))
    #|END class corpus%|#))

(define currently-initializing?
  (make-parameter #f))

(define empty-corpus
  (new corpus%
       [search-backend 'noop]))

(define current-corpus
  (make-parameter empty-corpus))

(define directory-corpus% 
  (class corpus%
    (init [(init:path path)])
    (define path
      (invariant-assertion absolute-path?
                           (simplify-path init:path)))
    (define/public-final (get-path)
      path)
    (super-new [docs
                (let ([dir-valid? (directory-validate-xml #:quiet? #t
                                                          path)])
                  (for*/list ([pth (in-directory path)]
                              #:when (xml-path? pth)
                              #:when (or dir-valid?
                                         (valid-xml-file? #:quiet? #t pth))
                              [maybe-doc (in-value
                                          (with-handlers
                                              ([exn:fail? (λ (e) #f)])
                                            (file->TEI pth)))]
                              #:when maybe-doc)
                    maybe-doc))])))


(define (get-checksum-table)
  (send (current-corpus) •get-checksum-table))

(define (term-search term
                     #:ricoeur-only? [ricoeur-only? #t]
                     #:book/article [book/article 'any]
                     #:exact? [exact? #f])
  (send (current-corpus)
        •term-search
        term
        #:ricoeur-only? ricoeur-only?
        #:book/article book/article
        #:exact? exact?))

(define (list-TEI-info)
  (send (current-corpus) •list-TEI-info))


#|
(require db)
(define rslts
  (parameterize ([current-corpus (new directory-corpus%
                                      [path "/Users/philip/code/ricoeur/texts/TEI"]
                                      [search-backend (postgresql-data-source
                                                       #:user "ricoeur"
                                                       #:database "term-search")])])
    (displayln "\n\nHere\n\n")
    (time (term-search "utopia")))) ;#:ricoeur-only? #f #:exact? #t)))
#;(for*/list ([dsr (in-list rslts)]
              [sr (in-list (document-search-results-results dsr))]
              [resp (in-value (search-result-author-string sr))]
              #:unless (regexp-match? #rx"^Paul Ric" resp))
    resp)
|#

