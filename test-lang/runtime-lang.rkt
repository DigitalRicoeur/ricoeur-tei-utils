#lang racket

(require syntax/parse/define
         (for-syntax "state.rkt"
                     syntax/strip-context
                     ))

(provide (except-out (all-from-out racket)
                     #%module-begin
                     +)
         show
         (rename-out [* +])
         (rename-out [module-begin #%module-begin]))

(module reader syntax/module-reader
  ricoeur/test-lang/runtime-lang)

(define-for-syntax (make-check-syntax-original stx)
  (syntax-property (let ([lst (syntax->list stx)])
                     (if lst
                         #`(#,@(map make-check-syntax-original
                                    lst))
                         stx))
                   'original-for-check-syntax #t))
                   
(define-syntax-parser module-begin
  [(_ body:expr ...)
   #:with (lifted-stx doc-module-begin)
   (generate-temporaries '(lifted-stx doc-module-begin))
   #:with doc-lang (datum->syntax this-syntax
                                  'ricoeur/test-lang/doc-lang
                                  (vector (syntax-source this-syntax) 1 0 1 1))
   #:do [(define introduce
           (make-syntax-introducer #t))]
   #:with (runtime-body ...)
   (let ([target (make-runtime-lift-target)])
     ;; Thanks https://groups.google.com/d/msg/racket-users/zpe27qAdHG0/iWWdxpuZEAAJ
     (parameterize ([current-runtime-lift-target target])
       (local-expand #`(#%plain-module-begin
                        (require (rename-in #,(introduce
                                               (strip-context #'doc-lang))
                                            [#,(introduce
                                                (datum->syntax #f
                                                               '#%module-begin))
                                             doc-module-begin]))
                        (expand-for-effect doc-module-begin
                                           #,@(syntax->list
                                               (introduce
                                                (strip-context #'(body ...))))))
                     'module-begin
                     null))
     (map (compose1 make-check-syntax-original
                    introduce
                    (make-syntax-delta-introducer #'doc-lang
                                                  #f))
          (runtime-lift-target->list target)))
   #`(#%module-begin
      (module* doc doc-lang
        body ...)
      runtime-body ...)])

(define-syntax-parser expand-for-effect
  [(_ doc-module-begin:id body ...)
   (local-expand #`(doc-module-begin body ...)
                 'module-begin
                 null)
   #'(void)])

(define-syntax-parser show
  [(_ v:expr)
   (println #'v)
   #''(show v)])

