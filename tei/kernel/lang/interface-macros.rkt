#lang racket/base

(require "adt.rkt"
         "../interfaces.rkt"
         (submod "../interfaces.rkt" for-lang)
         "../pre-kernel-lib.rkt"
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
   #:with (str-field-name sym-field-name)
   (generate-temporaries '(str-field-name sym-field-name))
   #`(begin
       (define/field [str-field-name #:check (or/c resp-string/c #f)
                                     #:hide]
         (attributes-ref attrs.c 'key-id))
       (define/field [sym-field-name #:check (or/c symbol? #f)]
         (and str-field-name
              (resp-string->symbol str-field-name)))
       (lift-property prop:resp
                      (cons (λ (this) (get-field sym-field-name this))
                            (λ (this) (get-field str-field-name this)))))])

(define-syntax-parser declare-paragraphs-status-field
  [(_ body)
   #:declare body (expr/c #'guess-paragraphs-status/c
                          #:name "guess paragraphs status expression")
   #'(begin (define/field par-status
              body.c)
            (lift-property prop:guess-paragraphs-status
                           (λ (this)
                             (get-field par-status this))))])



                          