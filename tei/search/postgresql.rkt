#lang _-exp racket

(require ricoeur/tei/base
         adjutor
         db
         data/maybe
         json
         ricoeur/lib/postgresql-data-source
         "common.rkt"
         (submod "common.rkt" private)
         )

(module+ test
  (require rackunit
           (submod "..")))

(provide (contract-out
          [postgresql-searchable-document-set
           (->* {#:db postgresql-data-source/c} 
                {(listof (is-a?/c TEI<%>))}
                searchable-document-set?)]
          ))

(define postgresql-searchable-document-set%
  (class abstract-searchable-document-set%
    (super-new)
    (init [(dsn db)]
          [docs '()])
    (define db
      (let ([dsn (struct-copy data-source dsn)]) ;prevent mutation
        (virtual-connection
         (connection-pool
          (λ () (dsn-connect dsn))))))
    (initialize db docs)
    (define hsh:title->teiHeader+excerpt-max-allow-chars
      (for/hash ([doc (in-list docs)])
        (define doc-chars
          (string-length (string-normalize-spaces
                          (send doc to-plain-text))))
        (values (send doc get-title)
                (cons (send doc get-teiHeader)
                      (* EXCERPT_RATIO doc-chars)))))
    (define/override-final (do-search-documents term
                                                #:ricoeur-only? [ricoeur-only? #t]
                                                #:exact? [exact? #f])
      (define maybe-exact-regexp
        (and exact?
             (pregexp (string-append "(?:^|[^[:alpha:]])"
                                     (regexp-quote term #f)
                                     "(?:[^[:alpha:]]|$)"))))
      (for*/list ([{title l-vecs}
                   (in-query db
                             (if ricoeur-only?
                                 select-statement:ricoeur-only
                                 select-statement:all)
                             term
                             #:group #("segdocumenttitle"))]
                  [raw-results (in-value
                                (vecs->search-results l-vecs
                                                      maybe-exact-regexp))]
                  #:unless (null? raw-results))
        (match-define (cons info excerpt-max-allow-chars)
          (hash-ref hsh:title->teiHeader+excerpt-max-allow-chars title))
        (new document-search-results%
             [info info]
             [results
              (let loop ([to-go (sort raw-results
                                      search-result<?)]
                         [chars-so-far 0])
                (match to-go
                  ['() '()]
                  [(cons this more)
                   (define chars-with-this
                     (+ chars-so-far
                        (maybe 0 string-length (search-result-excerpt this))))
                   (cond
                     [(chars-with-this . >= . excerpt-max-allow-chars)
                      (map nullify-search-result-excerpt to-go)]
                     [else
                      (cons this (loop more chars-with-this))])]))])))
    (define/private (vecs->search-results l-vecs maybe-exact-regexp)
      (flatten
       (for/list ([vec (in-list l-vecs)])
         (match-define (vector counter meta ts_headline)
           vec)
         (for/list ([excerpt (in-list (regexp-split rx:FragmentDelimiter
                                                    ts_headline))]
                    [sub-counter (in-naturals)]
                    #:when (non-empty-string? excerpt)
                    #:when (or (not maybe-exact-regexp)
                               (regexp-match? maybe-exact-regexp
                                              excerpt)))
           (make-search-result #:counter counter
                               #:sub-counter sub-counter
                               #:meta meta
                               #:excerpt (just (string-trim excerpt)))))))
    #|END postgresql-searchable-document-set%|#))

(define (postgresql-searchable-document-set #:db db [docs '()])
  (new postgresql-searchable-document-set%
       [db db]
       [docs docs]))





(define FragmentDelimiter
  "<<&&&&&DigitalRicoeurFragmentDelimiter&&&&&>>")

(define rx:FragmentDelimiter
  (regexp (regexp-quote FragmentDelimiter)))

(define MaxWords
  18)

(define MinWords
  7)

(define (make-select-statement [extra-where ""])
  ƒ~a{
 SELECT
 segDocumentTitle,
 segCounter,
 segMeta,
 ts_headline(segBody, qry, 'StartSel="",StopSel="",ShortWord=0,MaxWords=ƒMaxWords,MinWords=ƒMinWords,MaxFragments=10000,FragmentDelimiter="ƒFragmentDelimiter"')
 FROM
 (SELECT segDocumentTitle,segCounter,segMeta,segBody,qry
 FROM tSegments, phraseto_tsquery($1) AS qry
 WHERE segTSV @@ qry ƒextra-where) AS tmp
 })

(define select-statement:all
  (make-select-statement))

(define select-statement:ricoeur-only
  (make-select-statement
   ƒ~a{AND segResp = '#ricoeur'}))


(define (initialize db docs)
  (query-exec db "DROP TABLE IF EXISTS tSegments")

  (query-exec db ƒstring-append{
 CREATE TABLE tSegments (
 segDocumentTitle text NOT NULL,
 segCounter int8 NOT NULL,
 segMeta jsonb NOT NULL,
 segBody text NOT NULL,
 segTSV tsvector NOT NULL,
 segResp text NOT NULL
 );
 })

  (for ([segs (in-slice 1000 (flatten (let ([num-docs (length docs)])
                                        (for/list ([doc (in-list docs)]
                                                   [n (in-naturals 1)])
                                          ;(displayln ƒ~a{Preparing ƒn / ƒnum-docs : ƒ(send doc get-title)})
                                          (prepare-pre-segments doc)))))]
        #:unless (null? segs))
    #|Avoid:
query-exec: wrong number of parameters for query
  expected: 19494
  given: 85030
|#
    (insert-pre-segments db segs)
    #;(displayln "added slice"))
  
  (query-exec db "CREATE INDEX tsv_idx ON tSegments USING gin(segTSV)")

  #;(displayln "initialized db")
    
  db)

(define insert:base-str
  "INSERT INTO tSegments (segDocumentTitle,segCounter,segMeta,segBody,segTSV,segResp) VALUES ")

(define (insert-pre-segments db l-pre-segments) ;must not be empty
  (for/fold/define ([l-query-strs null]
                    [backwards-l-args null])
                   ([pre-seg (in-list l-pre-segments)]
                    [arg-counter (in-naturals)])
    (match-define (pre-segment title counter body meta resp)
      pre-seg)
    (define base
      (* 5 arg-counter))
    (values (cons ƒ~a{
 ($ƒ(+ 1 base),
 $ƒ(+ 2 base),
 $ƒ(+ 3 base),
 $ƒ(+ 4 base),
 to_tsvector($ƒ(+ 4 base)),
 $ƒ(+ 5 base))
}
                  l-query-strs)
            (list* resp
                   body
                   meta
                   counter
                   title
                   backwards-l-args)))
  (apply query-exec
         db
         (string-join #:before-first insert:base-str
                      l-query-strs
                      ",")
         (reverse backwards-l-args)))