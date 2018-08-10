#lang racket

(require (for-syntax "runtime-lift-state.rkt")
         syntax/parse/define
         (only-in scribble/decode ;; I think this is ok
                  pre-flow?)
         adjutor
         )

(provide begin-for-runtime
         begin-for-test
         begin-for-runtime/derived
         )

(module+ private
  (provide begin-for-runtime/private
           (for-syntax currently-expanding-prose-body?
                       )))

(define-syntax-parser begin-for-runtime/private
  [(_ #:expr doctime-expr:expr
      body:expr ...)
   ;; syntax-local-introduce is needed here to manually
   ;; remove the macro scope, since the body syntax is stashed
   ;; away instead of being returned to the expander
   (lift-for-runtime! (syntax->list (syntax-local-introduce #'(body ...))))
   #'doctime-expr])

(begin-for-syntax
  (define currently-expanding-prose-body?
    (make-parameter #f)))

(begin-for-syntax
  (define-syntax-class pre-flow/c
    #:attributes {c}
    #:description #f
    #:literals {void}
    (pattern (void)
             #:with c this-syntax)
    (pattern other
             #:declare other
             (expr/c #'pre-flow?) ;; rationale: make-defelement-flow
             #:with c #'other.c)))

(define-syntax-parser begin-for-runtime/derived
  #:context (syntax-parse this-syntax
              [(_ orig-datum _ ...)
               #'orig-datum])
  [(_ orig-datum
      (~optional (~seq #:expr doctime-expr:pre-flow/c)
                 #:defaults ([doctime-expr.c #'(void)]))
      body:expr ...)
   #:fail-unless (case (syntax-local-context)
                   [(module) #t]
                   [(expression) (currently-expanding-prose-body?)]
                   [else #f])
   (format "~a\n  local context: ~v"
           "only allowed at module level or in element definition prose body"
           (syntax-local-context))
   #'(begin-for-runtime/private
       #:expr doctime-expr.c
       body ...)])

(define-syntax-parser begin-for-runtime
  [(_ body:expr ...)
   #`(begin-for-runtime/derived
       #,this-syntax
       body ...)])

(define-syntax-parser begin-for-test
  [(_ body:expr ...)
   #`(begin-for-runtime/derived
       #,this-syntax
       (module+ test
         body ...))])






