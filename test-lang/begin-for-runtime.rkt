#lang racket

(require (for-syntax "state.rkt")
         syntax/parse/define
         )

(provide begin-for-runtime)

(define-syntax-parser begin-for-runtime
  [(_ body:expr ...)
   (lift-for-runtime! (syntax->list #'(body ...)))
   #'(void)])
