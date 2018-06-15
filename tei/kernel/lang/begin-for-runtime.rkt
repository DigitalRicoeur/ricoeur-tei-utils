#lang racket

(require (for-syntax "runtime-lift-state.rkt")
         syntax/parse/define
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
   (lift-for-runtime! (syntax->list #'(body ...)))
   #'doctime-expr])

(begin-for-syntax
  (define currently-expanding-prose-body?
    (make-parameter #f)))

(define-syntax-parser begin-for-runtime/derived
  #:context (syntax-parse this-syntax
              [(_ orig-datum _ ...)
               #'orig-datum])
  [(_ orig-datum
      (~optional (~seq #:expr doctime-expr:expr)
                 #:defaults ([doctime-expr #'(void)]))
      body:expr ...)
   #:fail-unless (case (syntax-local-context)
                   [(module) #t]
                   [(expression) (currently-expanding-prose-body?)]
                   [else #f])
   (format "~a\n  local context: ~v"
           "only allowed at module level or in element definition prose body"
           (syntax-local-context))
   #'(begin-for-runtime/private
       #:expr doctime-expr
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






