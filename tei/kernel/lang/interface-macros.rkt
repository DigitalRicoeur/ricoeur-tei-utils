#lang racket/base

(require "adt.rkt"
         "../interfaces.rkt"
         (submod "../interfaces.rkt" for-lang)
         "../xexpr/contract-utils.rkt"
         racket/contract
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide add-resp-field
         )

(define-syntax-parser add-resp-field
  [(_ (~alt (~once (~seq attrs)
                   #:name "attributes expression")
            (~optional (~seq #:key key-id:id)
                       #:name "#:key clause"
                       #:defaults ([key-id #'resp])))
      ...)
   #:declare attrs (expr/c #'(listof (list/c symbol? string?))
                           #:name "attributes expression")
   #:with (f.name) (generate-temporaries '(resp-field))
   #`(begin
       (define/field f.name
         (attributes-ref attrs.c 'key-id))
       (lift-property prop:resp
                      (λ (this)
                        (get-field f.name this))))])