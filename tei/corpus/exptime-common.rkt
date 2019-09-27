#lang racket/base

(require syntax/parse)

(provide from-interfaces
         to-interfaces
         class-clause)

(define-syntax-class from-interfaces
  #:description "\"from\" interfaces"
  #:attributes {[<%> 1]}
  (pattern [<%>:expr ...]))

(define-syntax-class to-interfaces
  #:description "\"to\" interfaces"
  #:attributes {[<%> 1]}
  (pattern [<%>:expr ...]))

(define-syntax-class class-clause
  #:description "class clause"
  (pattern :expr))
