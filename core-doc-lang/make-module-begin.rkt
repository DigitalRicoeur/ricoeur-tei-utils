#lang racket/base

(require syntax/parse/define
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     syntax/strip-context
                     "lift.rkt"
                     ))

(provide (for-syntax make-module-begin)
         (rename-out
          [child-module+ module+]
          [child-module* module*]
          ))

(define-syntax-parser child-module+
  ;; inspired by https://groups.google.com/d/msg/racket-users/H7vilh3KcD4/WoKcRUXe-PAJ
  ;; This will use the normal #%module-begin
  [(_ . stuff)
   #'(module+ . stuff)])

(define-syntax-parser child-module*
  [(_ . stuff)
   #'(module* . stuff)])

(define-for-syntax (make-module-begin
                    #:doc-lang [doc-lang-datum 'ricoeur/core-doc-lang/doc-time]
                    #:module-begin [module-begin-stx #'#%module-begin])
  (syntax-parser
    [(_ body-for-doc ...)
     #:with #%module-begin module-begin-stx
     #:with test (datum->syntax this-syntax 'test)
     #:with doc (datum->syntax this-syntax 'doc)
     #:with for-doc-lang
     (datum->syntax this-syntax 
                    doc-lang-datum
                    (vector (syntax-source this-syntax) 1 0 1 1))
     #:do [(define doctime-introduce
             (make-syntax-introducer #t))
           (define (adjust-scopes-for-doctime stx)
             (syntax-local-introduce
              (doctime-introduce (strip-context stx) 'add)))
           (define this-syntax-delta-introducer
             (make-syntax-delta-introducer this-syntax #f))]
     #:with (_ _ _ (_ . expanded-doctime))
     (expand (list* #'module 'temp-name (adjust-scopes-for-doctime #'for-doc-lang)
                    ;; scribble/lp2 uses a different module language here than
                    ;; for the actual module*: not clear why
                    '#:fake
                    (adjust-scopes-for-doctime #'(doc body-for-doc ...))))
     #:with (runtime-body:expr ...)
     (filter-map (syntax-parser
                   [form:lifted-for-runtime
                    (this-syntax-delta-introducer
                     ;; I think strip-context isn't quite right:
                     ;; we want to keep internal scope differences, 
                     ;; just remove the other module's scope.
                     ;; But it seems to mostly work.
                     (strip-context
                      #'(begin form.body ...)))]
                   [_ #f])
                 (syntax->list #'expanded-doctime))
     #:with submodule-declarations
     #`(begin
         (module* doc #,(adjust-scopes-for-doctime #'for-doc-lang)
           . #,(adjust-scopes-for-doctime #'(doc body-for-doc ...)))
         (module+ test
           (require (submod ".." doc))))
     #`(#%module-begin
        submodule-declarations
        runtime-body ...)]))





