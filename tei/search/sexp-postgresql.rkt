#lang racket

(require db
         sql
         ricoeur/tei/base
         syntax/parse/define)

;; sql module:
;; - ident-qq is not a table-ref-ast? or a table-expr-ast?
;; - sql-ast->string
;;    - broken on unquote parameters
;;    - support connection? as dialect
;; - optimization to re-use numbered query parameter
;; - add ability to create dynamically named table
;; - add #:default for create-table
;;    - complication: no query parameters in CREATE TABLE, it seems

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define-simple-macro (create-tSegmentsXX-statement tSegments:id)
  (create-table
   tSegments
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
   (primary-key segDocumentTitle segSerializedMeta)))

(define-syntax-parser define-segment-tables
  [(_ (~alt (~once (~seq #:tSegments-id-asts tSegments-id-asts:id))
            (~once (~seq #:tSegments-create-statements
                         tSegments-create-statements:id))
            (~once (~seq #:lang->tSegments-id-ast lang->tSegments-id-ast:id))
            (~once (~seq #:lang->search-config-ast lang->search-config-ast:id)))
      ...
      [lang:id config:str tSegments:id] ...)
   #:with (tSegmentsXX-id-ast ...) (generate-temporaries #'(tSegments ...))
   #`(begin
       (define tSegments-create-statements
         (list (create-tSegmentsXX-statement tSegments) ...))
       (define tSegmentsXX-id-ast (ident-qq tSegments)) ...
       (define tSegments-id-asts (list tSegmentsXX-id-ast ...))
       (define (lang->tSegments-id-ast cf)
         (case cf
           [(lang) tSegmentsXX-id-ast]
           ...
           [else (raise-argument-error 'lang->tSegments-id-ast
                                       (~a (or/c 'lang ...))
                                       cf)]))
       (define lang->search-config-ast
         (let ([lang (scalar-expr-qq (ScalarExpr:INJECT config))]
               ...)
           (λ (cf)
             (case cf
               [(lang) lang]
               ...
               [else (raise-argument-error 'lang->search-config-ast
                                           (~a (or/c 'lang ...))
                                           cf)])))))])

(define-segment-tables
  #:tSegments-id-asts tSegments-id-asts
  #:tSegments-create-statements tSegments-create-statements
  #:lang->tSegments-id-ast lang->tSegments-id-ast
  #:lang->search-config-ast lang->search-config-ast
  [en "'english'" tSegmentsEN]
  [fr "'french'" tSegmentsFR]
  [de "'german'" tSegmentsDE])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-parser lift
  [(_ it:expr)
   (syntax-local-lift-expression #'it)])

(define (ast->postgresql ast)
  (sql-ast->string ast 'postgresql))

(define tSegTableFormatVersion-id-ast
  (table-ref-qq (Ident: "tSegTableFormatVersion")))

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
   ;; check tables exist
   (for/and ([table-str (in-list needed-tables-strings)])
     (member table-str tables-found string-ci=?))
   ;; Check that the column exists
   (member (ast->postgresql (ident-qq segTableFormatVersion))
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
           (query-list db
                       (lift
                        (select segTableFormatVersion
                                #:from (Ident: "tSegTableFormatVersion")))))))


;; maybe-create-table : connection? -> (or/c 'created #f)
;; Effect: Creates or re-builds the search tables
;;   in the database if needed.
;;   (Does not populate the tables.(
;; Returns 'created if the tables were (re)built, #f otherwise.
(define (maybe-create-table db)
  (cond
    [(table-format-version-ok? db)
     #false]
    [else
     ;; drop if exists
     (for ([tbl (in-list
                 (lift (cons (ast->postgresql (table-ref-qq tSegments))
                             ;; ^ for compatability
                             needed-tables-strings)))])
       (query-exec db (string-append "DROP TABLE IF EXISTS " tbl)))
     ;; create
     (for ([create-stmnt
            (in-list
             (lift (list*
                    (create-table
                     (Ident: "tSegTableFormatVersion")
                     #:columns
                     [segTableFormatVersion int2 #:not-null])
                    (insert #:into (Ident: "tSegTableFormatVersion")
                            #:set [segTableFormatVersion ,TABLE-FORMAT-VERSION])
                    tSegments-create-statements)))])
       (query-exec db create-stmnt))
     'created]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; must-add-doc? : connection? table-ref-ast? tei-document? -> boolean?
;; Returns #false IFF the referenced table for search already
;;  contains up-to-date entries for the given TEI document.
;; Effect: Deletes any out-of-date rows for the given TEI document
;;  from the database.
(define (must-add-doc? db tSegments doc)
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
     (when maybe-old-checksum-string
       (query-exec db
                   (delete #:from (Name:AST ,tSegments)
                           #:where (= segDocumentTitle ,title))))
     #true]))

#;
(to_tsvector (ScalarExpr:AST ,(scalar-expr-qq (ScalarExpr:INJECT ,"'english'")))
             ,"What lovely raindrops!")

;; giving language as unquote parameter throws
;;   sql-statement: unsupported type; typeid: 3734




