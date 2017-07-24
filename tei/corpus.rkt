#lang racket

(require ricoeur/tei/base
         ricoeur/tei/search
         adjutor
         )

(provide corpus%
         directory-corpus%
         empty-corpus
         (except-out (all-from-out ricoeur/tei/search)
                     search-documents
                     searchable-document?
                     prepare-searchable-document)
         (contract-out
          [current-corpus
           (parameter/c (is-a?/c corpus%))]
          [term-search
           (-> term/c
               (listof (is-a?/c document-search-results<%>)))]
          [list-TEI-info
           (-> (listof (is-a?/c TEI-info<%>)))]
          ))

(define-custom-hash-types string-ci-dict
  #:key? string?
  string-ci=?)

(define/contract corpus%
  ;TODO: enforce unique titles
  (class/c (init [docs (listof (is-a?/c TEI<%>))]))
  (class* object% [(interface ()
                     [•list-TEI-info (->m (listof (is-a?/c TEI-info<%>)))]
                     [•term-search
                      (->m term/c
                           (listof (is-a?/c document-search-results<%>)))])]
    (super-new)
    (init [docs '()])
    (for/fold/define ([headers '()]
                      [searchable-docs '()])
                     ([doc (in-list docs)])
      (values (cons (send doc get-teiHeader)
                    headers)
              (cons (prepare-searchable-document doc)
                    searchable-docs)))
    (define/public (•list-TEI-info)
      headers)
    (define search-cache
      (make-mutable-string-ci-dict))
    (define/public (•term-search raw-term)
      (define term
        (string-normalize-spaces raw-term))
      (dict-ref search-cache
                term
                (λ ()
                  (let ([rslts (search-documents term searchable-docs)])
                    (dict-set! search-cache term rslts)
                    rslts))))
    #|END class corpus%|#))

(define empty-corpus
  (new corpus%))

(define current-corpus
  (make-parameter empty-corpus))

(define/contract directory-corpus%
  (class/c (init [path (and/c path-string? directory-exists?)])) 
  (class corpus%
    (init [(init:path path)])
    (define path
      init:path)
    (define/public-final (get-path)
      path)
    (super-new [docs
                (for/list ([pth (in-directory path)]
                           #:when (xml-path? pth))
                  (call-with-input-file pth
                    read-TEI))])))





(define (term-search term)
  (send (current-corpus) •term-search term))

(define (list-TEI-info)
  (send (current-corpus) •list-TEI-info))

#|
(parameterize ([current-corpus (new directory-corpus%
                                    [path "/Users/philip/code/ricoeur/texts/TEI"])])
  ;(term-search "utopia"))
  (list-TEI-info))
|#  
