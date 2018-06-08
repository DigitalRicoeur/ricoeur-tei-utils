#lang racket/base

(require ricoeur/tei/kernel
         "def-from-spec.rkt"
         racket/match
         racket/list
         racket/contract
         )

(provide (contract-out
          [tei-get-page-breaks
           (-> tei-element?
               (listof tei-pb?))]
          ))

(define (tei-get-page-breaks this)
  (flatten
   (get-page-breaks/raw this)))

(define (get-page-breaks/raw this)
  (match this
    [(elements-only-element 'pb _ _)
     this]
    [(or (content-containing-element _ _ body)
         (elements-only-element _ _ _ #:elements-only body))
       (map get-page-breaks/raw body)]
    [_
     null]))









