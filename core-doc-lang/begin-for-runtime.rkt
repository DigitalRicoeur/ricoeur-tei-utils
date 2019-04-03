#lang racket/base

(require syntax/parse/define
         (only-in scribble/decode
                  pre-flow?)
         (for-syntax racket/base
                     "lift.rkt"))

(provide begin-for-runtime
         begin-for-test
         begin-for-runtime/derived)

(module+ prose-body
  (provide (for-syntax local-expand-prose-body-expression)))

(define-syntax-parser begin-for-runtime/private
  [(_ #:expr doctime-expr:expr
      body:expr ...)
   ;; syntax-local-introduce is NOT needed here because
   ;; the body syntax is returned to the expander
   (lift-for-runtime! (syntax->list #'(body ...)))
   #'doctime-expr])

(begin-for-syntax
  (define currently-expanding-prose-body?
    (make-parameter #f))
  (define (syntax-local-context/prose-body)
    (case (syntax-local-context)
      [(module) 'module]
      [(expression)
       (and (currently-expanding-prose-body?) 'prose-body)]
      [else #f]))
  (define (local-expand-prose-body-expression stx)
    ;; should this error unless (syntax-local-context/prose-body) ?
    (parameterize ([currently-expanding-prose-body? #t])
      (local-expand stx 'expression null))))

(begin-for-syntax
  (define-syntax-class pre-flow/c
    #:attributes {c}
    #:description #f
    #:literals {void}
    (pattern (void _ ...)
             #:with c this-syntax)
    (pattern other
             #:declare other
             (expr/c #'pre-flow?)
             #:with c #'other.c)))

(define-syntax-parser begin-for-runtime/derived
  #:context (syntax-parse this-syntax
              [(_ orig-datum _ ...)
               #'orig-datum])
  [(_ orig-datum
      (~optional (~seq #:expr doctime-expr:pre-flow/c)
                 #:defaults ([doctime-expr.c #'(void)]))
      body:expr ...)
   #:fail-unless (syntax-local-context/prose-body)
   (format "~a\n  local context: ~v"
           "only allowed at module level or in a prose body context"
           (syntax-local-context))
   #'(begin-for-runtime/private
       #:expr doctime-expr.c
       body ...)])

(define-syntax-parser begin-for-runtime
  [(_ (~optional (~seq #:expr doctime:expr))
      body:expr ...)
   #`(begin-for-runtime/derived
       #,this-syntax
       (~? (~@ #:expr doctime))
       body ...)])

(define-syntax-parser begin-for-test
  [(_ body:expr ...)
   #`(begin-for-runtime/derived
       #,this-syntax
       (module+ test
         body ...))])






