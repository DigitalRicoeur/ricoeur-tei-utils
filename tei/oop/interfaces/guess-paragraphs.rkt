#lang racket

(require xml 
         ricoeur/tei/xexpr/normalize
         ricoeur/tei/xexpr/tei-xexpr-contracts
         data/maybe
         gregor
         adjutor
         "pb.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     ))

(provide guess-paragraphs<%>
         p<%>
         ab<%>
         guess-paragraphs-mixin
         )

(define guess-paragraphs<%>
  (interface (get-page-breaks<%>)
    [guess-paragraphs
     (->i {[this any/c]}
          {#:mode [mode (or/c 'blank-lines 'line-breaks)]}
          [_ (this) (is-a?/c (object-interface this))])]))

(define p<%>
  (interface (get-page-breaks<%>)))

(define ab<%>
  (interface (get-page-breaks<%>)
    [do-guess-paragraphs (->*m {}
                               {#:mode (or/c 'blank-lines 'line-breaks)}
                               (listof (or/c (is-a?/c pb<%>)
                                             (is-a?/c p<%>))))]))

(define ab?
  (is-a?/c ab<%>))

(define guess-paragraphs?
  (is-a?/c guess-paragraphs<%>))

(define guess-paragraphs-mixin
  (mixin {get-page-breaks<%>} {guess-paragraphs<%>}
    (super-new)
    (inherit get-name get-attributes get-body)
    (define/public (guess-paragraphs #:mode [mode 'blank-lines])
      (new this%
           [name (get-name)]
           [attributes (get-attributes)]
           [body (flatten
                  (for/list ([child (in-list (get-body))])
                    (cond
                      [(ab? child)
                       (send child do-guess-paragraphs #:mode mode)]
                      [(guess-paragraphs? child)
                       (send child guess-paragraphs #:mode mode)]
                      [else
                       child])))]))))


