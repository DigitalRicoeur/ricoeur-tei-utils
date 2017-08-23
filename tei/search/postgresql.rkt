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
    (initialize db)
    (define hsh:title->teiHeader+excerpt-max-allow-chars
      (for/hash ([doc (in-list docs)])
        (define doc-chars
          (string-length (string-normalize-spaces
                          (send doc to-plain-text))))
        (values (send doc get-title)
                (cons (send doc get-teiHeader)
                      (* EXCERPT_RATIO doc-chars)))))
    (for/list ([segs (in-slice 1000 (flatten (map prepare-pre-segments
                                                  docs)))]
               #:unless (null? segs))
    #|Avoid:
query-exec: wrong number of parameters for query
  expected: 19494
  given: 85030
|#
      (insert-pre-segments db segs))
    (define/override-final (do-search-documents term #:ricoeur-only? [ricoeur-only? #t])
      (for/list ([{title l-vecs}
                  (in-query db
                            (if ricoeur-only?
                                select-statement:ricoeur-only
                                select-statement:all)
                            term
                            #:group #("segdocumenttitle"))])
        (match-define (cons info excerpt-max-allow-chars)
          (hash-ref hsh:title->teiHeader+excerpt-max-allow-chars title))
        (new document-search-results%
             [info info]
             [results
              (let loop ([to-go (sort (vecs->search-results l-vecs)
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
    (define/private (vecs->search-results l-vecs)
      (flatten
       (for/list ([vec (in-list l-vecs)])
         (match-define (vector counter meta ts_headline)
           vec)
         (for/list ([excerpt (in-list (regexp-split rx:FragmentDelimiter
                                                    ts_headline))]
                    [sub-counter (in-naturals)]
                    #:when (non-empty-string? excerpt))
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


(define (initialize db)
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

  (query-exec db "CREATE INDEX tsv_idx ON tSegments USING gin(segTSV)")
  
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