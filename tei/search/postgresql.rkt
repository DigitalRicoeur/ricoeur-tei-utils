#lang _-exp racket/base

(require racket/contract
         racket/class
         racket/unit
         racket/match
         racket/format
         racket/promise
         racket/sequence
         racket/string
         racket/list
         db/base
         syntax/parse/define
         "common.rkt"
         (submod "common.rkt" private)
         "data-source-contract.rkt"
         ricoeur/tei/base
         (for-syntax syntax/parse
                     racket/base
                     ))

(provide postgresql-data-source/c
         postgresql@)

(define-unit/search^ postgresql@
  (import)
  (export search^)
  (define search-backend/c
    (postgresql-data-source/c))
  (define (initialize-search-backend dsn docs)
    (new postgresql-searchable-document-set%
         [dsn dsn]
         [docs docs])))

(define postgresql-searchable-document-set%
  (class* object% {searchable-document-set<%>}
    (super-new)
    (init dsn
          [docs (instance-set)])
    (define db
      (virtual-connection
       (connection-pool
        (λ () (dsn-connect dsn)))))
    (define hsh:title/symbol->info+excerpt-max-allow-chars
      (let ([promise:initialize
             (delay/thread
              (call-with-transaction
               db
               (λ () (initialize db docs))))]
            [promise:hsh
             (delay/thread
              (make-hasheq:title/symbol->info+excerpt-max-allow-chars
               docs))])
        (force promise:initialize) ;; for effect
        (force promise:hsh)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public-final (do-term-search norm-term
                                         #:ricoeur-only? ricoeur-only?
                                         #:book/article book/article
                                         #:exact? exact?)
      (define term-string
        (normalized-term-string norm-term))
      (define maybe-exact-px
        (and exact? (pregexp (pregexp-quote-normalized-term norm-term
                                                            #:exact? #t))))
      (db-do-term-search db
                         hsh:title/symbol->info+excerpt-max-allow-chars
                         term-string
                         #:maybe-exact-px maybe-exact-px
                         #:ricoeur-only? ricoeur-only?
                         #:book/article book/article))
    #|END class postgresql-searchable-document-set%|#))


(define (make-hasheq:title/symbol->info+excerpt-max-allow-chars docs)
  (for/hasheq ([doc (in-instance-set docs)])
    (values (instance-title/symbol doc)
            (cons (get-plain-instance-info doc)
                  (tei-document->excerpt-max-allow-chars doc)))))
                                                 
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

;; db-do-term-search : connection?
;;                     (hash/c symbol? (cons/c instance-info?
;;                                             exact-positive-integer?))
;;                     (and/c term/c trimmed-string-px)
;;                     #:ricoeur-only? any/c
;;                     #:book/article (or/c 'any 'book 'article)
;;                     [#:maybe-exact-px (or/c #f pregexp?)]
;;                       -> (instance-set/c document-search-results?)
(define (db-do-term-search db
                           hsh:title/symbol->info+excerpt-max-allow-chars
                           term-string
                           #:ricoeur-only? ricoeur-only?
                           #:book/article book/article
                           #:maybe-exact-px [maybe-exact-px #f])
  ;;(printf "term-string: ~v\n" term-string)
  (group-search-results
   hsh:title/symbol->info+excerpt-max-allow-chars
   (select-search-results db term-string
                          #:ricoeur-only? ricoeur-only?
                          #:book/article book/article
                          #:maybe-exact-px maybe-exact-px)))


;; group-search-results : (hash/c symbol? (cons/c instance-info?
;;                                                exact-positive-integer?))
;;                        (cons/c (listof (non-empty-listof search-result?))
;;                          -> (instance-set/c document-search-results?)
;; Invariant: All of the values in each (non-empty-listof search-result?)
;;   are from the same segment.
;; Invariant: Every segment-title/symbol corresponds to a key in
;;   the hash table.
(define (group-search-results hsh:title/symbol->info+excerpt-max-allow-chars
                              l-seg-rslt-lists)
  (instance-set
   (for/list ([doc-rslt-forest
               (in-list (group-by (λ (seg-rslt-lst)
                                    (segment-title/symbol (car seg-rslt-lst)))
                                  l-seg-rslt-lists
                                  eq?))])
     (define doc-rslts
       (sort (flatten doc-rslt-forest) search-result-<?))
     (match-define (cons info excerpt-max-allow-chars)
       (hash-ref hsh:title/symbol->info+excerpt-max-allow-chars
                 (segment-title/symbol (car doc-rslts))))
     (make-document-search-results
      info
      (let loop ([doc-rslts doc-rslts]
                 [chars-so-far 0])
        (match doc-rslts
          ['() '()]
          [(cons this more)
           (define chars-with-this
             (+ chars-so-far
                (maybe 0 string-length (search-result-excerpt this))))
           (cond
             [(infix: chars-with-this > excerpt-max-allow-chars)
              (map search-result-nullify-excerpt doc-rslts)]
             [else
              (cons this (loop more chars-with-this))])]))))))

;; select-search-results : connection? (and/c term/c trimmed-string-px)
;;                         [#:maybe-exact-px (or/c #f pregexp?)]
;;                         #:ricoeur-only? any/c
;;                         #:book/article (or/c 'any 'book 'article)
;;                           -> (listof (non-empty-listof search-result?))
;; Invariant: All of the values in each (non-empty-listof search-result?)
;;   are from the same segment.
(define (select-search-results db
                               term-string
                               #:maybe-exact-px [maybe-exact-px #f]
                               #:ricoeur-only? ricoeur-only?
                               #:book/article book/article)
  (define valid-px
    (or maybe-exact-px
        #px"\\S"))
  (define (make-seg-results-list ts_headline)
    (for/list ([raw-excerpt
                (in-list (regexp-split rx:FragmentDelimiter
                                       ts_headline))]
               #:when (regexp-match? valid-px raw-excerpt))
      (just (string->immutable-string raw-excerpt))))
  (define-syntax-parser run-query-with-args
    ;; This is syntax to get the performance benefits of in-query
    [(_ statement:expr query-arg:expr ...)
     #`(for*/list ([{serialized-meta ts_headline}
                    (in-query db
                              statement
                              term-string
                              query-arg ...)]
                   [l-maybe-string
                    (in-value (make-seg-results-list ts_headline))]
                   #:unless (null? l-maybe-string))
         (segment-make-search-results
          (deserialize-from-string serialized-meta)
          l-maybe-string))])
  (case book/article
    [(any)
     (run-query-with-args (if ricoeur-only?
                              select-statement:ricoeur-only
                              select-statement:all))]
    [else
     (run-query-with-args (if ricoeur-only?
                              select-statement:ricoeur-only+book/article
                              select-statement:all+book/article)
                          (eq? 'book book/article))]))
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values {FragmentDelimiter rx:FragmentDelimiter}
  (let ([FragmentDelimiter
         "<<&&&&&DigitalRicoeurFragmentDelimiter&&&&&>>"])
    (values FragmentDelimiter
            (regexp (regexp-quote FragmentDelimiter)))))

(TODO/void Consider the sql module.)

(define MaxWords
  18)

(define MinWords
  7)

(define (make-select-statement [extra-where ""])
  (string->immutable-string
   ƒ~a{
 SELECT
 segSerializedMeta,
 ts_headline(segBody, qry, 'StartSel="",StopSel="",ƒ;
 ShortWord=0,MaxWords=ƒMaxWords,MinWords=ƒMinWords,ƒ;
 MaxFragments=10000,FragmentDelimiter="ƒFragmentDelimiter"')
 FROM
 (SELECT segSerializedMeta,segBody,qry
 FROM tSegments, phraseto_tsquery($1) AS qry
 WHERE segTSV @@ qry ƒextra-where) AS tmp
 }))

(define where:ricoeur-only
  "AND segIsByRicoeur = TRUE")

(define where:book/article
  "AND segDocumentIsBook = $2")

(define select-statement:all
  (virtual-statement
   (make-select-statement)))

(define select-statement:ricoeur-only
  (virtual-statement
   (make-select-statement
    where:ricoeur-only)))

(define select-statement:all+book/article
  (virtual-statement
   (make-select-statement
    where:book/article)))

(define select-statement:ricoeur-only+book/article
  (virtual-statement
   (make-select-statement
    (string-append where:ricoeur-only " " where:book/article))))

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
                                                                                
(module create-table racket/base
  (require racket/contract
           racket/format
           db
           adjutor
           (for-syntax syntax/parse
                       racket/base))
  (provide str
           (contract-out
            [maybe-create-table
             (-> connection? (or/c 'created #f))]))
  ;; If we need to make a backwards-incompatible change to the
  ;; table schema, increment this constant to force the table to be rebuilt.
  ;; History:
  ;;  - Version 1: after re-write of prepare-pre-segments
  ;;  - Version 2: during major re-write for DSL/ADT
  ;;  - Version 3: force rebuild after bug fix from DSL/ADT port
  (define/contract TABLE-FORMAT-VERSION
    natural-number/c
    3)

  ;; create-table : connection? -> any
  ;; Effect: Creates the search table in the database
  (define (create-table db)
    (query-exec db ƒ~a{
 CREATE TABLE tSegments (
 segDocumentTitle text NOT NULL,
 segSerializedMeta text NOT NULL,
 segBody text NOT NULL,
 segTSV tsvector NOT NULL,
 ƒ; The following are denormalized for simplicity
 segIsByRicoeur bool NOT NULL,
 segDocumentIsBook bool NOT NULL,
 segDocumentHash text NOT NULL, 
 segTableFormatVersion int2 NOT NULL DEFAULT ƒ|TABLE-FORMAT-VERSION|,
 PRIMARY KEY (segDocumentTitle,segSerializedMeta)
 );
 }))

  (define-syntax str
    (syntax-parser
      [(_ part:str ...)
       (datum->syntax this-syntax
                      (apply string-append
                             (syntax->datum #'(part ...))))]))

  ;; table-format-version-ok? : connection? -> boolean?
  ;; Returns #true IFF the table for search already
  ;; exists in the current format, according to
  ;; TABLE-FORMAT-VERSION.
  (define (table-format-version-ok? db)
    (any->boolean
     (and
      ;; Check that the column exists
      (member "segTableFormatVersion"
              (query-list db
                          ƒstr{
 SELECT column_name
 FROM information_schema.columns
 WHERE table_name='tsegments'
 })
              string-ci=?)
      ;; Check that the value is correct
      (= TABLE-FORMAT-VERSION
         (query-value db
                      ƒstr{
 SELECT segTableFormatVersion
 FROM tSegments
 LIMIT 1
 })))))

  ;; maybe-create-table : connection? -> (or/c 'created #f)
  ;; Effect: Creates or re-builds the search table
  ;;   in the database if needed
  ;; Returns 'created if the table was (re)built, #f otherwise.
  (define (maybe-create-table db)
    (let/ec return
      (when (table-exists? db "tSegments")
        (if (table-format-version-ok? db)
            (return #f)
            (query-exec db "DROP TABLE tSegments")))
      (create-table db)
      'created))
  #|END module create-table|#)
(require 'create-table)


;; must-add-doc? : connection? tei-document? -> boolean?
;; Returns #false IFF the table for search already
;;  contains up-to-date entries for the given TEI document.
;; Effect: Deletes any out-of-date rows for the given TEI document
;;  from the database.
(define (must-add-doc? db doc)
  (define title
    (instance-title doc))
  (define maybe-old-checksum-string
    (query-maybe-value
     db
     ƒstr{
 SELECT segDocumentHash
 FROM tSegments
 WHERE segDocumentTitle = $1
 LIMIT 1
}
     title))
  (cond
    [(and maybe-old-checksum-string
          (eq? (string->symbol maybe-old-checksum-string)
               (tei-document-checksum doc)))
     #f]
    [else
     (when maybe-old-checksum-string
       (query-exec db
                   "DELETE FROM tSegments WHERE segDocumentTitle = $1"
                   title))
     #t]))


(struct row-to-insert (title/string ; segDocumentTitle
                       serialized-meta ; segSerializedMeta
                       body ; segBody
                       is-by-ricoeur? ; segIsByRicoeur
                       is-book? ; segDocumentIsBook
                       checksum/string) ; segDocumentHash
  #:transparent)
                       

;; build-l-to-insert : (->* (connection? (instance-set/c tei-document?))
;;                          (#:empty-table? any/c)
;;                          (listof row-to-insert?))
;; Effect: Deletes any out-of-date rows for the given TEI documents
;;  from the database, via must-add-doc?.
;; Returns a row-to-insert structure for each row that must be inserted.
(define (build-l-to-insert db docs #:empty-table? [empty-table? #f])
  (for*/list ([doc (in-instance-set docs)]
              #:when (or empty-table?
                         (must-add-doc? db doc))
              [title/string (in-value (instance-title doc))]
              [is-book? (in-value (eq? 'book (instance-book/article doc)))]
              [checksum/string (in-value (string->immutable-string
                                          (symbol->string
                                           (tei-document-checksum doc))))]
              [base-seg (in-list (tei-document-segments doc))])
    (match-define (base-segment meta body)
      base-seg)
    (row-to-insert title/string
                   (serialize-to-string meta)
                   body
                   (segment-by-ricoeur? meta)
                   is-book?
                   checksum/string)))


;; initialize : connection? (instance-set/c tei-document?) -> any
;; Effects:
;;  - If the given database doesn't already have a search
;;    table in the current format, creates one, perhaps
;;    dropping an obsolete table.
;;  - Populates the created table with rows for any new or changed
;;    TEI documents from the given set.
(define (initialize db docs)
  (define fresh-table?
    (maybe-create-table db))
  (define l-to-insert
    (build-l-to-insert db docs #:empty-table? fresh-table?))
  (define no-change?
    (null? l-to-insert))
  (unless no-change?
    (query-exec db "DROP INDEX IF EXISTS tsv_idx")
    (for ([l-to-insert (in-slice 1000 l-to-insert)]
          #:unless (null? l-to-insert))
      ;;Do in slices to avoid:
      ;; query-exec: wrong number of parameters for query
      ;;   expected: 19494
      ;;   given: 85030
      (insert-rows db l-to-insert))
    (query-exec db "CREATE INDEX tsv_idx ON tSegments USING gin(segTSV)")))


;; insert-rows : connection? (non-empty-listof row-to-insert?) -> any
;; Effect: Inserts the given rows into the search table.
;; Invariants: There must be some rows, and there must be fewer
;;  than PostgreSQL's internal limit for inserting at once.
(define (insert-rows db l-to-insert)
  (define-values {query-string args}
    (build-insert-rows-query l-to-insert))
  (apply query-exec db query-string args))


;; build-insert-rows-query : (non-empty-listof row-to-insert?)
;;   -> (values string? (listof any/c))
;; Returns a query string and a list of query arguments
;;  suitable for use with query-exec in insert-rows.
;; Invariants: There must be some rows, and there must be fewer
;;  than PostgreSQL's internal limit for inserting at once.
(define* (build-insert-rows-query l-to-insert)
  #:with [(define insert:base-str
            ƒstr{
           INSERT INTO tSegments (
           segDocumentTitle,
           segSerializedMeta,
           segBody,
           segTSV,
           segIsByRicoeur,
           segDocumentIsBook,
           segDocumentHash
           ) VALUES })]
  (TODO/void build-insert-rows-query: cache to avoid rebuilding string) 
  (for/fold ([l-query-strs null]
             [backwards-l-args null]
             #:result
             (values (string->immutable-string
                      (string-join #:before-first insert:base-str
                                   l-query-strs
                                   ","))
                     (reverse backwards-l-args)))
            ([this-row (in-list l-to-insert)]
             [arg-counter (in-naturals)])
    (match-define (row-to-insert title/string 
                                 serialized-meta 
                                 body 
                                 is-by-ricoeur? 
                                 is-book? 
                                 checksum/string) 
      this-row)
    (define base
      (* 6 arg-counter))
    (values (cons ƒ~a{
 ($ƒ|(+ 1 base)|, ƒ; segDocumentTitle
 $ƒ|(+ 2 base)|, ƒ; segSerializedMeta
 $ƒ|(+ 3 base)|, ƒ; segBody
 to_tsvector($ƒ|(+ 3 base)|), ƒ; segTSV
 $ƒ|(+ 4 base)|, ƒ; segIsByRicoeur
 $ƒ|(+ 5 base)|, ƒ; segDocumentIsBook
 $ƒ|(+ 6 base)|) ƒ; segDocumentHash
}
                  l-query-strs)
            (list* checksum/string
                   is-book?
                   is-by-ricoeur?
                   body
                   serialized-meta
                   title/string
                   backwards-l-args))))



