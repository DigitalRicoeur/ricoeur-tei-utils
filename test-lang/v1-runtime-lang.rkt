#lang racket

(require syntax/parse/define
         (for-syntax "state.rkt"
                     syntax/strip-context
                     ))

(provide (except-out (all-from-out racket)
                     #%module-begin)
         (rename-out [module-begin #%module-begin]))

(module reader syntax/module-reader
  ricoeur/test-lang/runtime-lang)

(define-syntax-parser module-begin
  [(_ body:expr ...)
   #:with (lifted-stx)
   (generate-temporaries '(lifted-stx))
   #:with doc-lang (datum->syntax this-syntax
                                  'ricoeur/test-lang/doc-lang)
   #:do [(define submod
           (local-expand #`(#%plain-module-begin
                            (module doc doc-lang
                              #:lift lifted-stx
                              body ...))
                         'module-begin
                         null))]
   #:with (runtime-body ...)
   (syntax-parse submod
     [(_ (_ _ _ (_ . forms)))
      (for/or ([stx (in-list (syntax->list #'forms))])
        (syntax-parse stx
          #:datum-literals {define-syntaxes quote-syntax}
          [(define-syntaxes (n)
             (quote-syntax (runtime-body ...)))
           #:fail-unless (eq? (syntax->datum #'n)
                              (syntax->datum #'lifted-stx))
           "not the lifted-stx id"
           (syntax->list #'(runtime-body ...))]
          [_
           #f]))])
   #`(#%module-begin
      (module* . #,(syntax-parse submod
                     [(_ (_ . more))
                      #'more]))
      runtime-body ...)])


