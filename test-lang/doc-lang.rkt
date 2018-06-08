#lang racket

(require "begin-for-runtime.rkt"
         )

(provide (all-from-out racket)
         (all-from-out "begin-for-runtime.rkt"))
#|
(define-syntax-parser module-begin
  [(_ #:lift lifted-stx-name:id
      body:expr ...)
   #'(#%module-begin body ...
                     (do-lift lifted-stx-name))]
  #;[(_ body:expr ...)
   #'(#%module-begin body ...)])

(define-syntax-parser do-lift
  [(_ lifted-stx-name:id)
   #:with (runtime-body ...)
   (runtime-lift-target->list (current-runtime-lift-target))
    #`(define-syntax lifted-stx-name
        #'(runtime-body ...))])
|#   