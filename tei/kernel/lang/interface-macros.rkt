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

(provide declare-resp-field
         declare-paragraphs-status-field
         )

(define-syntax-parser declare-resp-field
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
       (define/field [f.name #:check (or/c symbol? #f)]
         ;; FIXME
         (attributes-ref attrs.c 'key-id))
       (lift-property prop:resp
                      (λ (this)
                        (get-field f.name this))))])

(define-syntax-parser declare-paragraphs-status-field
  [(_ body)
   #:declare body (expr/c #'guess-paragraphs-status/c
                          #:name "guess paragraphs status expression")
   #'(begin (define/field par-status
              body.c)
            (lift-property prop:guess-paragraphs-status
                           (λ (this)
                             (get-field par-status this))))])



                          