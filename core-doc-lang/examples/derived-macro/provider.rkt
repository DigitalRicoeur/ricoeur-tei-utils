#lang racket

(require syntax/parse/define
         (only-in "../../doc-time.rkt"
                  begin-for-runtime))

(provide show)

(define-syntax-parser show
  [(_ v)
   #'(begin-for-runtime
       #:expr (show/doctime v)
       v)]
  [_
   #:do [(println this-syntax)]
   #:fail-when #t "failed"
   (error 'show "shouldn't get here")])

(define (show/doctime v)
  (printf "I am at doctime!\t~v\n" v))

