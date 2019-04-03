#lang racket/base

(require "adt.rkt"
         ricoeur/tei/kernel/interfaces
         (submod ricoeur/tei/kernel/interfaces for-lang)
         ricoeur/kernel/pre-kernel-lib
         (submod ricoeur/tei/kernel/base-structs for-lang)
         racket/contract
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))

(provide declare-resp-field
         declare-paragraphs-status-field
         (recontract-out prop:element->plain-text))

(define-syntax-parser declare-resp-field
  [(_ (~alt (~once (~seq attrs)
                   #:name "attributes expression")
            (~optional (~seq #:key key-id:id)
                       #:name "#:key clause"
                       #:defaults ([key-id #'resp])))
      ...)
   #:declare attrs
   (expr/c #'(listof (list/c symbol? string-immutable/c))
           #:name "attributes expression")
   #:with (str-field-name sym-field-name)
   (generate-temporaries '(str-field-name sym-field-name))
   #`(begin
       (define/field [sym-field-name #:check (or/c symbol? #f)]
         (let ([str (attributes-ref attrs.c 'key-id)])
           (and str
                (resp-fragment-string->symbol str))))
       (lift-property prop:resp
                      (λ (this) (get-field sym-field-name this))))])

(define-syntax-parser declare-paragraphs-status-field
  [(_ body)
   #:declare body (expr/c #'guess-paragraphs-status/c
                          #:name "guess paragraphs status expression")
   #'(begin (define/field par-status
              body.c)
            (lift-property prop:guess-paragraphs-status
                           (λ (this)
                             (get-field par-status this))))])



