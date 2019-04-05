#lang racket/base

(require syntax/parse
         syntax/parse/lib/function-header
         syntax/flatten-begin
         syntax/id-set
         syntax/define
         racket/list
         (for-template racket/base
                       racket/class))

(provide local-expand-class-clauses)

(define class-keywords
  (syntax->list #'(inspect
                   init init-field field
                   inherit-field
                   init-rest init-rest
                   public pubment public-final
                   override overment override-final
                   augment augride augment-final
                   private abstract
                   inherit inherit/super inherit/inner
                   rename-super rename-inner)))

(define-literal-set class-keywords-literals #:for-template
  (inspect
   init init-field field
   inherit-field
   init-rest
   public pubment public-final
   override overment override-final
   augment augride augment-final
   private abstract
   inherit inherit/super inherit/inner
   rename-super rename-inner))

(define the-stop-list
  (list* #'begin #'define-syntax #'define-syntaxes #'define #'define-values
         class-keywords))

(define-literal-set expanded-class-clause-literals #:for-template
  #:literal-sets [class-keywords-literals]
  (#'define-syntax define-syntaxes define define-values))

(define-syntax-class define-1-value
  #:attributes [name rhs]
  #:description #f
  #:literals [define define-values]
  (pattern (define-values (name:id) rhs:expr))
  (pattern (define name:id rhs:expr))
  (pattern (define lhs:function-header body:expr ...)
           #:do [(define-values [-name -rhs]
                   (normalize-definition #'(define lhs body ...) #'λ #f #t))]
           #:with name -name
           #:with rhs -rhs))

(define-syntax-class maybe-renamed
  #:attributes [internal external]
  (pattern external:id
           #:with internal #'external)
  (pattern [internal:id external:id]))
#|
(define-syntax-class (method-procedure [wrap values])
  #:attributes parsed
  #:literals [chaperone-procedure
              lambda #%plain-lambda case-lambda let-values letrec-values]
  (pattern ((~and lam (~or* lambda #%plain-lambda)) kw-formals body:expr ...)
           #:with parsed #`(lam kw-formals #,(wrap #'(let () body ...))))
  (pattern (case-lambda [formals body:expr ...+] ...)
           #:with (rhs ...) (map wrap (syntax->list #'((let () body ...) ...)))
           #:with parsed #'(case-lambda [formals rhs] ...)
|#
(define (local-expand-class-clauses raw*
                                    #:wrap-init [wrap-init values]
                                    #:wrap-method [wrap-method values])
  (define declared-method-names (mutable-bound-id-set))
  (define expanded-clauses
    (flatten
     (let local-expand-class-clauses ([raw* raw*])
       (for/list ([raw (in-list raw*)])
         (syntax-parse (local-expand raw (syntax-local-context) the-stop-list)
           ;#:track-literals
           #:literals [begin]
           #:literal-sets [expanded-class-clause-literals]
           [(begin clause:expr ...)
            (local-expand-class-clauses
             (flatten-all-begins this-syntax))]
           [((~or* public pubment public-final
                   override overment override-final
                   augment augride augment-final
                   private abstract)
             :maybe-renamed ...)
            (for ([id (in-list (syntax->list #'(internal ...)))])
              (bound-id-set-add! declared-method-names id))
            this-syntax]
           [_
            this-syntax])))))
  (define-syntax-class init-decl
    #:attributes [parsed]
    (pattern (~and parsed (~or* :id [(:id :id)])))
    (pattern [lhs:maybe-renamed raw:expr]
             #:with parsed #`[maybe-renamed #,(wrap-init #'raw)]))
  #`(begin
      #,@(for/list ([stx (in-list expanded-clauses)])
           (syntax-parse stx
             ;#:track-literals
             [:define-1-value
               #:with wrapped-rhs
               (if (bound-id-set-member? declared-method-names #'name)
                   (wrap-method #'rhs)
                   (wrap-init #'rhs))
               #'(define [name] wrapped-rhs)]
             [(define-values [(~between name:id 2 +inf.0) ...] rhs:expr)
              #`(define-values [name ...] #,(wrap-init #'rhs))]
             [(init decl:init-decl ...)
              #'(init decl.parsed ...)]
             [(init-field decl:init-decl ...)
              #'(init-field decl.parsed ...)]
             [(field [lhs:maybe-renamed rhs:expr] ...)
              #:with (wrapped-rhs ...) (map wrap-init (syntax->list #'(rhs ...)))
              #'(field [lhs rhs] ...)]
             [other:expr
              (wrap-init #'other)]))))










         
