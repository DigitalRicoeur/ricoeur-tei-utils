#lang racket/base

(require racket/contract
         "base-structs.rkt"
         )

(provide tei-element-can-have-resp?
         (contract-out
          [tei-element-resp
           (->* {tei-element?}
                {(or/c 'ricoeur #f)}
                (or/c symbol? #f))]
          ))

(module+ for-lang
  (provide (contract-out
            [prop:resp
             (struct-type-property/c
              (-> any/c (or/c symbol? #f)))]
            )))

(define-values {prop:resp
                tei-element-can-have-resp?
                get-get-resp}
  (make-struct-type-property 'prop:resp))


(define (tei-element-resp e [default 'ricoeur])
  (or ((get-get-resp e) e)
      default))
