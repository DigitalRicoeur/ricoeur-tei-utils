#lang racket

(require ricoeur/tei/base
         ricoeur/tei/search
         ricoeur/lib/postgresql-data-source
         adjutor
         )

(provide postgresql-data-source/c
         empty-corpus
         (except-out (all-from-out ricoeur/tei/search)
                     search-documents
                     searchable-document-set?
                     regexp-searchable-document-set
                     postgresql-searchable-document-set)
         (contract-out
          [corpus%
           (class/c (init [docs (listof (is-a?/c TEI<%>))]
                          [search-backend (or/c #f postgresql-data-source/c)]))]
          [directory-corpus%
           (class/c (init [path (and/c path-string? directory-exists?)]
                          [search-backend (or/c #f postgresql-data-source/c)]))]
          [current-corpus
           (parameter/c (is-a?/c corpus%))]
          [term-search
           (->* {term/c}
                {#:ricoeur-only? any/c}
                (listof (is-a?/c document-search-results<%>)))]
          [list-TEI-info
           (-> (listof (is-a?/c TEI-info<%>)))]
          ))

(define/contract corpus%
  ;TODO: enforce unique titles
  (class/c (init [docs (listof (is-a?/c TEI<%>))]
                 [search-backend (or/c #f postgresql-data-source/c)]))
  (class* object% [(interface ()
                     [•list-TEI-info (->m (listof (is-a?/c TEI-info<%>)))]
                     [•term-search
                      (->*m {term/c}
                            {#:ricoeur-only? any/c}
                            (listof (is-a?/c document-search-results<%>)))])]
    (super-new)
    (init [docs '()]
          [search-backend #f])
    (for/fold/define ([headers '()])
                     ([doc (in-list docs)])
      (values (cons (send doc get-teiHeader)
                    headers)))
    (define searchable-document-set
      (cond
        [search-backend
         (postgresql-searchable-document-set docs #:db search-backend)]
        [else
         (regexp-searchable-document-set docs)]))
    (define/public-final (•list-TEI-info)
      headers)
    (define/public-final (•term-search raw-term #:ricoeur-only? [ricoeur-only? #t])
      (define term
        (string-normalize-spaces raw-term))
      (search-documents term searchable-document-set
                        #:ricoeur-only? ricoeur-only?))
    #|END class corpus%|#))

(define empty-corpus
  (new corpus%))

(define current-corpus
  (make-parameter empty-corpus))

(define directory-corpus% 
  (class corpus%
    (init [(init:path path)])
    (define path
      init:path)
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
                                            (call-with-input-file pth
                                              read-TEI)))]
                              #:when maybe-doc)
                    maybe-doc))])))





(define (term-search term #:ricoeur-only? [ricoeur-only? #t])
  (send (current-corpus) •term-search term #:ricoeur-only? ricoeur-only?))

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
    (term-search "utopia" #:ricoeur-only? #f)))
(for*/list ([dsr (in-list rslts)]
            [sr (in-list (document-search-results-results dsr))]
            [resp (in-value (search-result-author-string sr))]
            #:unless (regexp-match? #rx"^Paul Ric" resp))
  resp)
|#  

