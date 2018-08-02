#lang racket/base

(require racket/contract
         racket/class
         racket/set
         racket/promise
         ricoeur/tei/base
         ricoeur/tei/search
         "corpus/plain-corpus.rkt"
         "corpus/search-corpus.rkt"
         )

(provide empty-corpus
         corpus-get-instance-info-set
         corpus-get-checksum-table
         corpus-do-term-search
         make-corpus-mixin
         (contract-out
          [corpus%
           (class/c
            (init [docs (instance-set/c tei-document?)]
                  [search-backend search-backend/c]))]
          [directory-corpus%
           (class/c
            (init [path (and/c path-string?
                               (if/c string? immutable? any/c)
                               directory-exists?)]
                  [search-backend search-backend/c]))]
          ;;;;
          [current-corpus
           (parameter/c (is-a?/c corpus%))]
          [get-instance-info-set
           (-> (instance-set/c))]
          [get-checksum-table
           (-> (hash/c symbol?
                       symbol?
                       #:immutable #t))]
          [term-search
           (->* {term/c}
                {#:ricoeur-only? any/c
                 #:book/article (or/c 'any 'book 'article)
                 #:exact? any/c}
                (instance-set/c document-search-results?))]
          ))

(define corpus%
  (class (corpus-search-mixin plain-corpus%)
    (super-new)
    (TODO/void corpus% docs argument
               #: could be more permissive "(e.g. accepting a list)"
               and centralize duplicate checking here)
    #|END class corpus%|#))

(define empty-corpus
  (new corpus%
       [docs (instance-set)]
       [search-backend '(eager noop)]))

(define directory-corpus% 
  (class corpus%
    (init path)
    ;; I used to have (invariant-assertion (absolute-path? (simplify-path path)))
    ;; Why?
    (super-new
     [docs
      (let ([dir-valid? (directory-validate-xml #:quiet? #t
                                                path)])
        (TODO/void Should there be an abstraction for reading in directory?
                   #: Happens here & in TEI Lint.
                   Would be faster to try valid-xml-file? on subgroups.)
        (instance-set
         (for/list ([pth (in-directory path)]
                    #:when (xml-path? pth)
                    #:when (or dir-valid?
                               (valid-xml-file? #:quiet? #t pth))
                    [maybe-doc (in-value
                                (with-handlers
                                    ([exn:fail? (λ (e) #f)])
                                  (file->tei-document pth)))]
                    #:when maybe-doc)
           maybe-doc)))])
    #|END class directory-corpus%|#))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-corpus
  (make-parameter empty-corpus))

(define (get-instance-info-set)
  (corpus-get-instance-info-set (current-corpus)))

(define (get-checksum-table)
  (corpus-get-checksum-table (current-corpus)))

(define (term-search term
                     #:ricoeur-only? [ricoeur-only? #t]
                     #:book/article [book/article 'any]
                     #:exact? [exact? #f])
  (corpus-do-term-search (current-corpus)
                         term
                         #:ricoeur-only? ricoeur-only?
                         #:book/article book/article
                         #:exact? exact?))

