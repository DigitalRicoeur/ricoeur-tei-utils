#lang racket

(require "specification/specification.rkt"
         (submod ricoeur/tei/kernel private)
         )

(provide tei-xexpr/c
         static-tei-xexpr/c
         any-tei-xexpr/c
         tei-element-name/c
         )

(define-values/elements-specifications [spec
                                        ]
  #:tei-xexpr/c tei-xexpr/c
  #:static-tei-xexpr/c static-tei-xexpr/c
  #:any-tei-xexpr/c any-tei-xexpr/c
  #:tei-element-name/c tei-element-name/c)





