#lang racket

(require syntax/parse/define)

(define-syntax-parser show
  [(_ v:expr)
   #`(cons #'v v)])

(show (+ 1 2))