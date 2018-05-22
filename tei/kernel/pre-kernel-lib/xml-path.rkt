#lang racket/base

(require racket/path
         racket/contract
         )

(provide (contract-out
          [xml-path?
           (-> path-string? any)]
          ))

(define (xml-path? pth)
  (let ([ext (path-get-extension pth)])
    (and ext (regexp-match? #rx"^(?i:\\.xml)$" ext))))



