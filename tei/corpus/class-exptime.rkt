#lang racket/base

(require racket/class
         (for-syntax racket/base
                     syntax/parse
                     syntax/parse/lib/function-header
                     syntax/flatten-begin
                     syntax/id-set
                     syntax/define
                     racket/list))

(provide (for-syntax local-expand-class-clauses))

(begin-for-syntax
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

  (define-literal-set class-keywords-literals
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

  (define-literal-set expanded-class-clause-literals
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
  #|END begin-for-syntax|#)

(define-for-syntax (local-expand-class-clauses raw*
                                               #:extra-stop-list [extra-stop-list values]
                                               #:wrap-init [wrap-init values]
                                               #:wrap-method [wrap-method values])
  (define local-stop-list (append extra-stop-list the-stop-list))
  (define declared-method-names (mutable-bound-id-set))
  (define expanded-clauses
    (flatten
     (let expand/record-method-names ([raw* raw*])
       (for/list ([raw (in-list raw*)])
         (syntax-parse (local-expand raw (syntax-local-context) local-stop-list)
           #:track-literals
           #:literals [begin]
           #:literal-sets [expanded-class-clause-literals]
           [(begin clause:expr ...)
            #`(begin
                #,@(expand/record-method-names
                    (syntax->list #'(clause ...))))]
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
             #:with parsed #`[lhs #,(wrap-init #'raw)]))
  (let wrap-expanded ([expanded-clauses expanded-clauses])
    #`(begin
        #,@(for/list ([stx (in-list expanded-clauses)])
             (syntax-parse stx
               #:track-literals
               #:literals [begin]
               #:literal-sets [expanded-class-clause-literals]
               [(begin clause:expr ...)
                (wrap-expanded (syntax->list #'(clause ...)))]
               [:define-1-value
                 #:with wrapped-rhs
                 (if (bound-id-set-member? declared-method-names #'name)
                     (wrap-method #'rhs)
                     (wrap-init #'rhs))
                 #'(define-values [name] wrapped-rhs)]
               [(define-values [(~between name:id 2 +inf.0) ...] rhs:expr)
                #`(define-values [name ...] #,(wrap-init #'rhs))]
               [(init decl:init-decl ...)
                #'(init decl.parsed ...)]
               [(init-field decl:init-decl ...)
                #'(init-field decl.parsed ...)]
               [(field [lhs:maybe-renamed rhs:expr] ...)
                #:with (wrapped-rhs ...) (map wrap-init (syntax->list #'(rhs ...)))
                #'(field [lhs rhs] ...)]
               [((~or* inspect inherit-field
                       init-rest
                       public pubment public-final
                       override overment override-final
                       augment augride augment-final
                       private abstract
                       inherit inherit/super inherit/inner
                       rename-super rename-inner)
                 . _)
                this-syntax]
               [other:expr
                (wrap-init #'other)])))))










         