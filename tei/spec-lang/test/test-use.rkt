#lang racket

(require "test3.rkt"
         "test4.rkt"
         "test-duplicate.rkt"
         ricoeur/tei/kernel
         (submod ricoeur/tei/kernel private)
         ricoeur/tei/spec-lang/link
         (submod ricoeur/tei/spec-lang/link
                 private))

(provide tei-xexpr/c
         dynamic-tei-xexpr/c
         any-tei-xexpr/c
         tei-element-name/c)


(show-elements-specification-transformer
 custom-spec)


(define-values/elements-specifications [custom-spec
                                        spec
                                        ;duplicate-spec
                                        ]
  #:tei-xexpr/c tei-xexpr/c
  #:dynamic-tei-xexpr/c dynamic-tei-xexpr/c
  #:any-tei-xexpr/c any-tei-xexpr/c
  #:xexpr->tei-element xexpr->element #:define/contract
  #:tei-element-name/c tei-element-name/c)
  
