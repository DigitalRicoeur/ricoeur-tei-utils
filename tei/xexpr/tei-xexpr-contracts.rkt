#lang racket/base

(require racket/unit
         "signatures.rkt"
         "specification.rkt"
         )

(provide tei-xexpr/c
         any-tei-xexpr/c
         tei-element-name/c
         )

(module+ test
  (require rackunit
           racket/contract
           (submod "..")))

(define-values/invoke-unit/infer
  (link tei-xexpr-contracts@ element-contracts@))


(module+ test
  (check-not-exn
   (λ () (contract-exercise tei-xexpr/c))))
