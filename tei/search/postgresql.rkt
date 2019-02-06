#lang _-exp racket/base

(require racket/contract
         racket/class
         racket/unit
         racket/match
         racket/format
         racket/promise
         racket/set
         racket/list
         racket/sequence
         syntax/parse/define
         db/base
         sql
         "common.rkt"
         (submod "common.rkt" private)
         "data-source-contract.rkt"
         "sql-support.rkt"
         ricoeur/tei/base
         (for-syntax racket/base))

(provide postgresql-data-source/c
         postgresql@)

(module+ test
  (provide (all-defined-out))
  (current-sql-dialect 'postgresql))

(define-unit-from-context postgresql@ search^)

(module+ test
  (define-values/invoke-unit/infer postgresql@))

;; sql module:
;; - ident-qq is not a table-ref-ast? or a table-expr-ast?
;; - sql-ast->string
;;    - broken on unquote parameters
;;    - support connection? as dialect
;; - optimization to re-use numbered query parameter
;; - add #:default for create-table
;;    - complication: no query parameters in CREATE TABLE, it seems

(TODO/void support logging)

(define search-backend/c
  (postgresql-data-source/c))

(define (initialize-search-backend dsn docs)
  (new postgresql-searchable-document-set%
       [dsn dsn]
       [docs docs]))

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
             (delay/thread (initialize db docs))]
            [promise:hsh
             (delay/thread
              (make-hasheq:title/symbol->info+excerpt-max-allow-chars
               docs))])
        (force promise:initialize) ;; for effect
        (force promise:hsh)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public-final (do-term-search norm-term
                                         #:ricoeur-only? ricoeur-only?
                                         #:languages st:lang
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
                         #:languages st:lang
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

;; db-do-term-search;; (->* {connection?
;;       (hash/c symbol? (cons/c instance-info?
;;                               exact-positive-integer?))
;;       (and/c term/c trimmed-string-px)
;;       #:ricoeur-only? any/c
;;       #:languages (set/c language-symbol/c #:cmp 'eq #:kind 'immutable)
;;       #:book/article (or/c 'any 'book 'article)}
;;      {#:maybe-exact-px (or/c #f pregexp?)}
;;      (listof (non-empty-listof search-result?)))
(define (db-do-term-search db
                           hsh:title/symbol->info+excerpt-max-allow-chars
                           term-string
                           #:ricoeur-only? ricoeur-only?
                           #:languages st:lang
                           #:book/article book/article
                           #:maybe-exact-px [maybe-exact-px #f])
  (group-search-results
   hsh:title/symbol->info+excerpt-max-allow-chars
   (select-search-results db term-string
                          #:ricoeur-only? ricoeur-only?
                          #:languages st:lang
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
  (for/instance-set
      ([doc-rslt-forest
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
             (cons this (loop more chars-with-this))])])))))


;; select-search-results
;; (->* {connection?
;;       (and/c term/c trimmed-string-px)
;;       #:ricoeur-only? any/c
;;       #:languages (set/c language-symbol/c #:cmp 'eq #:kind 'immutable)
;;       #:book/article (or/c 'any 'book 'article)}
;;      {#:maybe-exact-px (or/c #f pregexp?)}
;;      (listof (non-empty-listof search-result?)))
;; Invariant: All of the values in each (non-empty-listof search-result?)
;;   are from the same segment.
(define (select-search-results db
                               term-string
                               #:maybe-exact-px [maybe-exact-px #f]
                               #:ricoeur-only? ricoeur-only?
                               #:languages st:lang
                               #:book/article book/article)
  (define valid-px
    (or maybe-exact-px
        #px"[^\\s]"))
  (define (make-seg-results-list headline)
    (for/list ([raw-excerpt
                (in-list (regexp-split rx:FragmentDelimiter
                                       headline))]
               #:when (regexp-match? valid-px raw-excerpt))
      (just (string->immutable-string raw-excerpt))))
  (for*/list ([lang (in-immutable-set st:lang)]
              [{serialized-meta ts_headline}
               (in-query db
                         (make-select-statement
                          term-string
                          #:lang lang
                          #:ricoeur-only? ricoeur-only?
                          #:book/article book/article))]
              [l-maybe-string
               (in-value (make-seg-results-list ts_headline))]
              #:unless (null? l-maybe-string))
    (segment-make-search-results
     (deserialize-from-string serialized-meta)
     l-maybe-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values {FragmentDelimiter rx:FragmentDelimiter}
  (let ([FragmentDelimiter
         "<<&&&&&DigitalRicoeurFragmentDelimiter&&&&&>>"])
    (values FragmentDelimiter
            (regexp (regexp-quote FragmentDelimiter)))))

(define MaxWords
  18)

(define MinWords
  7)

(define headline-options-ast
  (scalar-expr-qq
   (ScalarExpr:INJECT ƒ,~a{'StartSel="",StopSel="",ƒ;
 ShortWord=0,MaxWords=ƒMaxWords,MinWords=ƒMinWords,ƒ;
 MaxFragments=10000,FragmentDelimiter="ƒFragmentDelimiter"'})))

(define (make-select-statement term-string
                               #:lang lang
                               #:ricoeur-only? ricoeur-only?
                               #:book/article book/article)
  (define search-config (lang->search-config-ast lang))
  (define-alias scalar scalar-expr-qq)
  (define where-expr-ast
    (for/fold ([where (scalar (@@ segTSV qry))])
              ([this (in-list
                      (list (and ricoeur-only?
                                 (scalar (is-true segIsByRicoeur)))
                            (case book/article
                              [(book)
                               (scalar (is-true segDocumentIsBook))]
                              [(article)
                               (scalar (is-false segDocumentIsBook))]
                              [else
                               #f])))]
               #:when this)
      (scalar (and (ScalarExpr:AST ,where)
                   (ScalarExpr:AST ,this)))))
  (select
   segSerializedMeta
   (ts_headline (ScalarExpr:AST ,search-config)
                segBody
                qry
                (ScalarExpr:AST ,headline-options-ast))
   #:from
   (as (select
        segSerializedMeta segBody qry
        #:from
        (Ident:AST ,(lang->tSegments-id-ast lang))
        (as (select (as (phraseto_tsquery (ScalarExpr:AST ,search-config)
                                          ,term-string)
                        qry))
            dontcare)
        #:where
        (ScalarExpr:AST ,where-expr-ast))
       tmp)))


(module+ test
  (for* ([lang '(en fr de)]
         [ricoeur-only? '(#t #f)]
         [book/article '(any book article)])
    (make-select-statement
     "recursive"
     #:lang lang
     #:ricoeur-only? ricoeur-only?
     #:book/article book/article)))

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

;; If we need to make a backwards-incompatible change to the
;; table schema, increment this constant to force the table to be rebuilt.
;; History:
;;  - Version 1: after re-write of prepare-pre-segments
;;  - Version 2: during major re-write for DSL/ADT
;;  - Version 3: force rebuild after bug fix from DSL/ADT port
;;  - Version 4: Multiple language support
(define/contract TABLE-FORMAT-VERSION
  natural-number/c
  4)

(define-index-names-struct index-names (tSegments
                                        [tsv_idx "~a_tsv_idx"]
                                        [resp_idx "~a_resp_idx"]
                                        [isBook_idx "~a_isBook_idx"])
  #:qq index-names-qq
  #:drop drop-indeces
  (λ (idx)
    (string-append "DROP INDEX IF EXISTS " idx))
  #:create (create-indeces db)
  (define (create name col/use)
    (query-exec db ƒ~a{CREATE INDEX ƒname ON ƒtSegments ƒcol/use}))
  (create tsv_idx "USING gin(segTSV)")
  (create isBook_idx "(segDocumentIsBook)")
  (create resp_idx "(segIsByRicoeur)"))


(define-segment-tables
  #:tSegments-id-asts tSegments-id-asts
  #:lang->tSegments-id-ast lang->tSegments-id-ast
  #:lang->search-config-ast lang->search-config-ast
  #:lang->index-names lang->index-names
  #:index-names-qq index-names-qq
  [en "'english'" tSegmentsEN]
  [fr "'french'" tSegmentsFR]
  [de "'german'" tSegmentsDE])

(define tSegTableFormatVersion-id-ast
  (ident-qq (Ident: "tSegTableFormatVersion")))

(define all-create-statements
  (list*
   (create-table
    (Ident:AST ,tSegTableFormatVersion-id-ast)
    #:columns
    [segTableFormatVersion int2 #:not-null])
   (insert #:into (Ident:AST ,tSegTableFormatVersion-id-ast)
           #:set [segTableFormatVersion ,TABLE-FORMAT-VERSION])
   (for/list ([tSegments (in-list tSegments-id-asts)])
     (create-table
      (Ident:AST ,tSegments)
      #:columns
      [segDocumentTitle text #:not-null]
      [segSerializedMeta text #:not-null]
      [segBody text #:not-null]
      [segTSV tsvector #:not-null]
      ;; The following are denormalized for simplicity
      [segIsByRicoeur bool #:not-null]
      [segDocumentIsBook bool #:not-null]
      [segDocumentHash text #:not-null]
      #:constraints
      (primary-key segDocumentTitle segSerializedMeta)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define needed-tables-strings
  (map ast->postgresql
       (cons tSegTableFormatVersion-id-ast
             tSegments-id-asts)))

;; table-format-version-ok? : connection? -> boolean?
;; Returns #true IFF ALL of the tables for search already
;; exist in the current format, according to
;; TABLE-FORMAT-VERSION.
(define (table-format-version-ok? db)
  (define tables-found
    (list-tables db))
  (and
   ;; Check tables exist
   (for/and ([table-str (in-list needed-tables-strings)])
     (member table-str tables-found string-ci=?))
   ;; Check that the column exists
   (member (lift (ast->postgresql (ident-qq segTableFormatVersion)))
           (query-list
            db
            (lift
             (select column_name
                     #:from information_schema.columns
                     #:where
                     (= table_name ,(ast->postgresql
                                     tSegTableFormatVersion-id-ast)))))
           string-ci=?)
   ;; Check that the value is correct
   (equal? (list TABLE-FORMAT-VERSION)
           (query-list
            db
            (lift
             (select segTableFormatVersion
                     #:from (Ident:AST ,tSegTableFormatVersion-id-ast)))))))


;; maybe-create-tables : connection? -> (or/c 'created #f)
;; Effect: Creates or re-creates the search tables
;;   in the database if needed.
;;   (Does not populate the tables.)
;; Returns 'created if the tables were (re)created, #f otherwise.
(define (maybe-create-tables db)
  (cond
    [(table-format-version-ok? db)
     #false]
    [else
     ;; drop if exists
     (for ([tbl (in-list
                 (lift (cons (ast->postgresql (ident-qq tSegments))
                             ;; ^ for compatability
                             needed-tables-strings)))])
       (query-exec db (string-append "DROP TABLE IF EXISTS " tbl)))
     ;; create
     (for ([create-stmnt (in-list all-create-statements)])
       (query-exec db create-stmnt))
     'created]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; must-add-doc?
;; (-> connection? table-ref-ast? tei-document?
;;     #:index-names (or/c #f index-names?)
;;     -> boolean?)
;; Returns #false IFF the referenced table for search already
;;  contains up-to-date entries for the given TEI document.
;; Effect: Deletes any out-of-date rows for the given TEI document
;;  from the database.
;;  If #:index-names is non-false, drops all derived indeces from
;;  the table.
(define (must-add-doc? db tSegments doc #:index-names index-names?)
  (define title
    (instance-title doc))
  (define maybe-old-checksum-string
    (query-maybe-value
     db
     (select segDocumentHash
             #:from (Ident:AST ,tSegments)
             #:where (= segDocumentTitle ,title)
             #:limit 1)))
  (cond
    [(and maybe-old-checksum-string
          (eq? (string->symbol maybe-old-checksum-string)
               (tei-document-checksum doc)))
     #false]
    [else
     (when index-names?
       (drop-indeces db index-names?))
     (when maybe-old-checksum-string
       (query-exec db
                   (delete #:from (Name:AST ,tSegments)
                           #:where (= segDocumentTitle ,title))))
     #true]))


;; build-to-insert
;; (-> connection? (instance-set/c tei-document?) #:empty-db? any/c
;;     (hash/c symbol? (listof base-segment?) #:immutable #t))
;; Effect: Deletes any out-of-date rows for the given TEI documents
;;  from the database, via must-add-doc?.
;;  Drops indeces for any tables from which rows have been deleted.
;; Returns: A hash mapping languages to lists of the
;;  segments that must be inserted.
(define (build-to-insert db docs #:empty-db? empty-db?)
  (for*/fold ([to-insert #hasheq()])
             ([doc (in-instance-set docs)]
              [lang (in-value (instance-language doc))]
              [so-far (in-value (hash-ref to-insert lang null))]
              #:when
              (or empty-db?
                  (must-add-doc? db
                                 (lang->tSegments-id-ast lang)
                                 doc
                                 #:index-names ;; only first time for language
                                 (and (null? so-far)
                                      (lang->index-names lang)))))
    (hash-set to-insert lang (append (tei-document-segments doc)
                                     so-far))))






(define* (symbol->immutable-string sym)
  #:with [(define cache (make-weak-hasheq))]
  (cond
    [(hash-ref cache sym #f)]
    [else
     (define rslt
       (datum-intern-literal
        (symbol->string sym)))
     (hash-set! cache sym rslt)
     rslt]))
  

(define (initialize db docs)
  (call-with-transaction
   db
   (λ () 
     (define fresh-db?
       (maybe-create-tables db))
     (for ([{lang segs}
            (in-immutable-hash
             (build-to-insert db docs #:empty-db? fresh-db?))])
       ;; not for* w/ in-value b/c need to call create-indeces
       ;; at the end of each lang
       (define tSegments (lang->tSegments-id-ast lang))
       (define search-config (lang->search-config-ast lang))
       (for ([segs (in-slice 1000 segs)])
         (query-exec
          db
          (for/insert-statement
              (#:into (Name:AST ,tSegments)
               [base-seg (in-list segs)])
            (match-define (base-segment meta body)
              base-seg)
            #:set
            [segDocumentTitle ,(instance-title base-seg)]
            [segSerializedMeta ,(serialize-to-string meta)]
            [segBody ,body]
            ;; giving language as unquote parameter throws
            ;;   sql-statement: unsupported type; typeid: 3734
            [segTSV (to_tsvector (ScalarExpr:AST ,search-config)
                                 ,body)]
            [segIsByRicoeur ,(segment-by-ricoeur? meta)]
            [segDocumentIsBook ,(eq? (instance-book/article base-seg)
                                     'book)]
            [segDocumentHash ,(symbol->immutable-string
                               (segment-document-checksum meta))])))
       ;; After all segs inserted for this lang:
       (create-indeces db (lang->index-names lang))))))


(define-syntax-parser for/insert-statement 
  [(_ (#:into name-ast-form
       for-clause ...)
      body-or-break ...
      #:set
      [column-ident-form
       scalar-expr-form] ...)
   #`(insert
      #:into name-ast-form
      #:columns column-ident-form ...
      #:from
      (TableExpr:AST
       ,(make-values*-table-expr-ast
         (for/fold/derived #,this-syntax
                           ([so-far '()])
           (for-clause ...)
           body-or-break ...
           (cons (list (scalar-expr-qq scalar-expr-form)
                       ...)
                 so-far)))))])



