#lang racket/base

(require "base.rkt"
         "../stxparam.rkt"
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     (only-in adjutor list-when)
                     racket/sequence
                     syntax/parse/experimental/template ;; coming in Racket 7
                     ))

(provide define/field
         define-fields
         define-values/fields
         define/field/derived
         define-values/fields/derived
         (for-syntax field-name/maybe-opts
                     ))

;; #:infer should be an option for field, not managed here

(begin-for-syntax
  (define-syntax-rule (splice1-when expr)
    (let ([stx expr])
      (list-when stx
        (list stx))))
  (define-syntax-class (field-name/maybe-opts orig-datum [infer? #f])
    #:description #f
    #:attributes {name declaration}
    (pattern (~describe "field name" name:id)
             #:with declaration
             #`(field/derived #,orig-datum name
                              #,@(splice1-when infer?)))
    (pattern (~describe "parenthesized field name with options"
                        [(~describe "field name" name:id)
                         opt ...])
             #:with declaration
             #`(field/derived #,orig-datum name
                              #,@(splice1-when infer?)
                              opt ...)))
  (define-syntax-class expr-or-infer
    #:description #f
    (pattern _:expr)
    (pattern #:infer))
  #|END begin-for-syntax|#)


(define-syntax-parser define/field
  [(_ body:expr-or-infer ...)
   #`(define/field/derived #,this-syntax body ...)])

(define-syntax-parser define-fields
  [(_ (~optional (~and infer? #:infer)) [f:expr rhs:expr ...] ...)
   (quasitemplate
    (begin (define/field/derived #,this-syntax (?? infer?) f rhs ...) ...))])

(define-syntax-parser define-values/fields
  [(_ body:expr-or-infer ...)
   #`(define-values/fields/derived #,this-syntax body ...)])


(define-syntax-parser define/field/derived
  #:context (syntax-parse this-syntax
              [(_ orig-datum _ ...)
               #'orig-datum])
  [(_ orig-datum (~optional (~and infer? #:infer))
      (~var f (field-name/maybe-opts #'orig-datum
                                     (attribute infer?)))
      rhs:expr)
   #`(begin f.declaration
            (define f.name
              rhs))]
  [(_ orig-datum (~optional (~and infer? #:infer))
      (~var f (field-name/maybe-opts #'orig-datum
                                     (attribute infer?)))
      rhs:expr ...)
   #`(begin f.declaration
            (define f.name
              (let ()
                rhs ...)))])

(define-syntax-parser define-values/fields/derived
  #:context (syntax-parse this-syntax
              [(_ orig-datum _ ...)
               #'orig-datum])
  [(_ orig-datum (~optional (~and infer? #:infer))
      ((~var f (field-name/maybe-opts #'orig-datum
                                      (attribute infer?)))
       ...)
      rhs:expr)
   #`(begin f.declaration ...
            (define-values (f.name ...)
              rhs))]
  [(_ orig-datum (~optional (~and infer? #:infer))
      ((~var f (field-name/maybe-opts #'orig-datum
                                      (attribute infer?)))
       ...)
      rhs:expr ...)
   #`(begin f.declaration ...
            (define-values (f.name ...)
              (let ()
                rhs ...)))])

