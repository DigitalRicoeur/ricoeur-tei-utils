#lang racket

(require ricoeur/tei/oop-kernel
         data/maybe
         gregor
         adjutor
         "element.rkt"
         "elements-only.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     ))

(provide get-page-breaks<%>
         pb<%>
         pb?
         can-get-page-breaks?
         get-page-breaks-mixin
         )        

(define get-page-breaks<%>
  (interface (element<%>)
    [get-page-breaks (->m (listof (recursive-contract (is-a?/c pb<%>))))]))

(define pb<%>
  (interface (get-page-breaks<%>)
    [get-page-string (->m (maybe/c string?))]
    [get-kind (->m (or/c 'none 'number 'roman 'other))]
    [get-numeric (->m (maybe/c number?))]
    ))

(define pb?
  (is-a?/c pb<%>))

(define can-get-page-breaks?
  (is-a?/c get-page-breaks<%>))

(define get-page-breaks-mixin
  (mixin {element<%>} {get-page-breaks<%>}
    (super-new)
    (inherit get-body)
    (define/public (get-page-breaks) 
      (flatten
       (for/list ([child (in-list (get-body))]
                  #:when (can-get-page-breaks? child))
         (send child get-page-breaks))))))



