#lang racket/base

(require "base.rkt"
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide define/field
         define-fields
         define-values/fields
         define/field/derived
         define-values/fields/derived
         (for-syntax field-name/maybe-opts
                     ))

(begin-for-syntax
  (define-syntax-class (field-name/maybe-opts orig-datum)
    #:description #f
    #:attributes {name declaration}
    (pattern (~describe "field name" name:id)
             #:with declaration
             #`(field/derived #,orig-datum name))
    (pattern (~describe "parenthesized field name with options"
                        [(~describe "field name" name:id)
                         opt ...])
             #:with declaration
             #`(field/derived #,orig-datum name opt ...))))


(define-syntax-parser define/field
  [(_ f:expr rhs:expr ...)
   #`(define/field/derived #,this-syntax f rhs ...)])

(define-syntax-parser define-fields
  [(_ [f:expr rhs:expr ...] ...)
   #`(begin (define/field/derived #,this-syntax f rhs ...) ...)])

(define-syntax-parser define-values/fields
  [(_ (f:expr ...) rhs:expr ...)
   #`(define-values/fields/derived #,this-syntax (f ...) rhs ...)])


(define-syntax-parser define/field/derived
  #:context (syntax-parse this-syntax
              [(_ orig-datum _ ...)
               #'orig-datum])
  [(_ orig-datum
      (~var f (field-name/maybe-opts #'orig-datum))
      rhs:expr)
   #`(begin f.declaration
            (define f.name
              rhs))]
  [(_ orig-datum
      (~var f (field-name/maybe-opts #'orig-datum))
      rhs:expr ...)
   #`(begin f.declaration
            (define f.name
              (let ()
                rhs ...)))])

(define-syntax-parser define-values/fields/derived
  #:context (syntax-parse this-syntax
              [(_ orig-datum _ ...)
               #'orig-datum])
  [(_ orig-datum
      ((~var f (field-name/maybe-opts #'orig-datum)) ...)
      rhs:expr)
   #`(begin f.declaration ...
            (define-values (f.name ...)
              rhs))]
  [(_ orig-datum
      ((~var f (field-name/maybe-opts #'orig-datum)) ...)
      rhs:expr ...)
   #`(begin f.declaration ...
            (define-values (f.name ...)
              (let ()
                rhs ...)))])


