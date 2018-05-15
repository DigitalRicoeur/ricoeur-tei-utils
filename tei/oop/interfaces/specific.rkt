#lang racket

(require ricoeur/tei/oop-kernel
         data/maybe
         gregor
         adjutor
         "elements-only.rkt"
         "pb.rkt"
         "guess-paragraphs.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     ))

(provide div-type/c
         div<%>
         note<%>
         )

(define div-type/c
  (or/c 'chapter 'part 'section 'dedication
        'contents 'intro 'bibl 'ack 'index))

(define div<%>
  (interface (elements-only<%>
              guess-paragraphs<%>
              get-page-breaks<%>)
    [get-type (->m  div-type/c)]
    [get-n (->m (maybe/c string?))]))
              
(define note<%>
  (interface (guess-paragraphs<%>
              get-page-breaks<%>)
    [get-place (->m (or/c 'foot 'end))]
    [get-n (->m string?)]
    [get-transl? (->m (or/c #f 'transl))]))






