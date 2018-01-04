#lang _-exp racket

(require ricoeur/tei/base
         adjutor
         db
         data/maybe
         json
         ricoeur/lib/postgresql-data-source
         "common.rkt"
         (submod "common.rkt" private)
         (for-syntax syntax/parse
                     racket/base
                     ))

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
                                                #:book/article [book/article 'any]
                                                #:ricoeur-only? [ricoeur-only? #t]
                                                #:exact? [exact? #f])
      ;; Do actual work in a plain function so I can
      ;; inspect macro expansion.
      (really-do-search-documents term
                                  db
                                  hsh:title->teiHeader+excerpt-max-allow-chars
                                  #:book/article book/article
                                  #:ricoeur-only? ricoeur-only?
                                  #:exact? exact?))
    #|END postgresql-searchable-document-set%|#))

(define (postgresql-searchable-document-set #:db db [docs '()])
  (new postgresql-searchable-document-set%
       [db db]
       [docs docs]))




(define (really-do-search-documents term
                                    db
                                    hsh:title->teiHeader+excerpt-max-allow-chars
                                    #:book/article book/article
                                    #:ricoeur-only? ricoeur-only?
                                    #:exact? exact?)
  (define maybe-exact-regexp
    (and exact?
         (pregexp (string-append exact?-px-prefix-str
                                 (regexp-quote term #f)
                                 exact?-px-suffix-str))))
  (define (process-doc-results title raw-results)
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
                  (cons this (loop more chars-with-this))])]))]))
  (define-syntax run-query-with-args
    ;; This is syntax to get the performance benefits of in-query
    (syntax-parser
      [(_ statement:expr args:expr ...)
       #`(for*/list ([{title l-vecs}
                      (in-query db
                                statement
                                term
                                args ...
                                #:group grouping)]
                     [raw-results (in-value
                                   (vecs->search-results l-vecs
                                                         maybe-exact-regexp))]
                     #:unless (null? raw-results))
           (process-doc-results title raw-results))]))
  (case book/article
    [(any)
     (if ricoeur-only?
         (run-query-with-args select-statement:ricoeur-only)
         (run-query-with-args select-statement:all))]
    [else
     (if ricoeur-only?
         (run-query-with-args select-statement:ricoeur-only+book/article
                              (eq? 'book book/article))
         (run-query-with-args select-statement:all+book/article
                              (eq? 'book book/article)))]))


(define grouping
  #("segdocumenttitle"))

(define (vecs->search-results l-vecs maybe-exact-regexp)
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

;                                                  
;                                                  
;                                                  
;                                                  
;                   ;;;;                     ;;    
;                     ;;                     ;;    
;     ;;      ;;;     ;;      ;;;      ;;; ;;;;;;; 
;   ;;  ;   ;;   ;    ;;    ;;   ;   ;;   ;  ;;    
;    ;      ;    ;    ;;    ;    ;   ;       ;;    
;     ;;   ;;;;;;;;   ;;   ;;;;;;;; ;;       ;;    
;       ;;  ;         ;;    ;        ;       ;;    
;   ;   ;   ;;   ;     ;    ;;   ;   ;;   ;   ;    
;    ;;;      ;;;       ;;    ;;;      ;;;     ;;; 
;                                                  
;                                                  
;                                                  
;                                                  

(define-syntax define-FragmentDelimiter
  (syntax-parser
    [(_ (FragmentDelimiter:id rx-FragmentDelimiter:id)
        delim:str)
     #:with delim-rx
     (datum->syntax #'delim (regexp (regexp-quote (syntax->datum #'delim))))
     #'(define-values (FragmentDelimiter rx-FragmentDelimiter)
         (values delim delim-rx))]))

(define-FragmentDelimiter (FragmentDelimiter rx:FragmentDelimiter)
  "<<&&&&&DigitalRicoeurFragmentDelimiter&&&&&>>")

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

(define where:ricoeur-only
  "AND segResp = '#ricoeur'")

(define where:book/article
  "AND segDocumentIsBook = $2")

(define select-statement:all
  (make-select-statement))

(define select-statement:ricoeur-only
  (make-select-statement
   where:ricoeur-only))

(define select-statement:all+book/article
  (make-select-statement
   where:book/article))

(define select-statement:ricoeur-only+book/article
  (make-select-statement
   (string-append where:ricoeur-only " " where:book/article)))

;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;      ;               ;     ;;        ;            ;;;;       ;                   
;      ;;              ;;    ;;        ;;             ;;       ;;                  
;   ;;;;;   ;; ;    ;;;;;  ;;;;;;;  ;;;;;     ;;      ;;    ;;;;;   ;;;;;;    ;;;  
;      ;;   ;;; ;      ;;    ;;        ;;    ;  ;     ;;       ;;       ;   ;;   ; 
;      ;;   ;;  ;;     ;;    ;;        ;;       ;;    ;;       ;;      ;    ;    ; 
;      ;;   ;;  ;;     ;;    ;;        ;;     ;;;;    ;;       ;;     ;    ;;;;;;;;
;      ;;   ;;  ;;     ;;    ;;        ;;    ;  ;;    ;;       ;;     ;     ;      
;      ;;   ;;  ;;     ;;     ;        ;;   ;;  ;;     ;       ;;    ;      ;;   ; 
;      ;;   ;;  ;;     ;;      ;;;     ;;    ;;; ;      ;;     ;;   ;;;;;;    ;;;  
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  


(define (initialize db docs)
  (query-exec db "DROP TABLE IF EXISTS tSegments")
  (query-exec db ƒstring-append{
 CREATE TABLE tSegments (
 segDocumentTitle text NOT NULL,
 segDocumentIsBook bool NOT NULL,
 segCounter int8 NOT NULL,
 segMeta jsonb NOT NULL,
 segBody text NOT NULL,
 segTSV tsvector NOT NULL,
 segResp text NOT NULL
 );
 })
  (for ([l-seg+isBook? (in-slice 1000 (for/fold ([l-seg+isBook? '()])
                                                ([doc (in-list docs)])
                                        (define isBook?
                                          (eq? 'book (send doc get-book/article)))
                                        (append (map (λ (seg) (cons seg isBook?))
                                                     (prepare-pre-segments doc))
                                                l-seg+isBook?)))]
        #:unless (null? l-seg+isBook?))
    ;;Avoid:
    ;; query-exec: wrong number of parameters for query
    ;;   expected: 19494
    ;;   given: 85030
    (insert-pre-segments db l-seg+isBook?))
  (query-exec db "CREATE INDEX tsv_idx ON tSegments USING gin(segTSV)")
  db)

(define insert:base-str
  "INSERT INTO tSegments (segDocumentTitle,segDocumentIsBook,segCounter,segMeta,segBody,segTSV,segResp) VALUES ")

(define (insert-pre-segments db l-seg+isBook?) ;must not be empty
  (for/fold/define ([l-query-strs null]
                    [backwards-l-args null])
                   ([seg+isBook? (in-list l-seg+isBook?)]
                    [arg-counter (in-naturals)])
    (match-define (cons (pre-segment title counter body meta resp)
                        isBook?)
      seg+isBook?)
    (define base
      (* 6 arg-counter))
    (values (cons ƒ~a{
 ($ƒ(+ 1 base),
 $ƒ(+ 2 base),
 $ƒ(+ 3 base),
 $ƒ(+ 4 base),
 $ƒ(+ 5 base),
 to_tsvector($ƒ(+ 5 base)),
 $ƒ(+ 6 base))
}
                  l-query-strs)
            (list* resp
                   body
                   meta
                   counter
                   isBook?
                   title
                   backwards-l-args)))
  (apply query-exec
         db
         (string-join #:before-first insert:base-str
                      l-query-strs
                      ",")
         (reverse backwards-l-args)))
























