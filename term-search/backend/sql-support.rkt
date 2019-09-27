#lang _-exp racket/base

(require db/base
         sql
         racket/contract
         racket/match
         racket/format
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))

(provide lift
         define-segment-tables
         define-index-names-struct
         ast->postgresql)


(define-syntax-parser lift
  [(_ it:expr)
   (syntax-local-lift-expression #'it)])

(define (ast->postgresql ast)
  (sql-ast->string ast 'postgresql))


(define-syntax-parser define-segment-tables
  [(_ (~alt (~once (~seq #:tSegments-id-asts tSegments-id-asts:id))
            (~once (~seq #:lang->index-names lang->index-names:id))
            (~once (~seq #:index-names-qq index-names-qq:id))
            (~once (~seq #:lang->tSegments-id-ast lang->tSegments-id-ast:id))
            (~once (~seq #:lang->search-config-ast lang->search-config-ast:id)))
      ...
      [lang:id config:str tSegments:id] ...)
   #:with (tSegmentsXX-id-ast ...) (generate-temporaries #'(tSegments ...))
   #`(begin
       (define tSegmentsXX-id-ast (ident-qq tSegments)) ...
       (define tSegments-id-asts (list tSegmentsXX-id-ast ...))
       (define (raise-lang-error who it)
         (raise-argument-error who (~a (or/c 'lang ...)) it))
       (define/lang-lambda lang->tSegments-id-ast
         raise-lang-error
         [lang tSegmentsXX-id-ast] ...)
       (define/lang-lambda lang->index-names
         raise-lang-error
         [lang (index-names-qq tSegments)] ...)
       (define/lang-lambda lang->search-config-ast
         raise-lang-error
         [lang (scalar-expr-qq (ScalarExpr:INJECT config))] ...))])

(define-syntax-parser define/lang-lambda
  [(_ name:id
      raise-lang-error:id
      [lang:id rhs:expr] ...)
   #`(define name
       (let ([lang rhs] ...)
         (λ (cf)
           (case cf
             [(lang) lang] ...
             [else (raise-lang-error 'name cf)]))))])


(define-syntax-parser define-index-names-struct
  [(_ name (table:id [field:id field-fmt:str] ...+)
      #:qq qq:id
      #:drop drop:id drop-proc:expr
      #:create (create:id db:id)
      create-body:expr ...+)
   #:with (field-kw ...)
   (for/list ([f (in-list (syntax->datum #'(field ...)))])
     (string->keyword (symbol->string f)))
   #`(begin
       (struct name (table field ...)
         #:transparent
         #:guard (λ (table field ... _)
                   (values (ast->postgresql table)
                           (ast->postgresql field) ...)))
       (define drop
         (let ([drop drop-proc])
           (λ (db this)
             (match this
               [(name _ field ...)
                (query-exec db (drop field)) ...
                (void)]))))
       (define (create db this)
         (match this
           [(name table field ...)
            create-body ...]))
       (define-syntax-parser qq
         #:disable-colon-notation
         [(_ (~var tbl id))
          #:with (idx (... ...))
          (for/list ([fmt (in-list '(field-fmt ...))])
            (format-id #'tbl fmt #'tbl))
          #'(name (ident-qq tbl) (ident-qq idx) (... ...))]))])


