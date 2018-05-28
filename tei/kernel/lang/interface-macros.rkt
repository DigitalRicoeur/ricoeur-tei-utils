#lang racket/base

(require "adt.rkt"
         "../interfaces.rkt"
         (submod "../interfaces.rkt" for-lang)
         "../xexpr/contract-utils.rkt"
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide add-resp-field
         )

(define-syntax-parser add-resp-field
  [(_ (~optional (~seq #:key key-id:id)
                 #:defaults ([key-id #'resp])))
   #:with (f.name) (generate-temporaries '(resp-field))
   #`(begin
       (define/field f.name
         (attributes-ref #|local-attributes-arg|# null 'key-id))
       (lift-property prop:resp
                      (λ (this)
                        (get-field f.name this))))])