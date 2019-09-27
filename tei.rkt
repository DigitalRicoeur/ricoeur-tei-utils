#lang racket/base

(require ricoeur/tei/base)

(require-provide (provide-only ricoeur/tei/base)
                 ricoeur/tei/corpus)

(module+ test
  #;ricoeur/tei/search
  (require rackunit/docs-complete)

  (define reexported-undocumented
    '(UTC
      define*/contract))

  (check-docs 'ricoeur/tei/base #:skip reexported-undocumented)

  (check-docs 'ricoeur/tei #:skip reexported-undocumented)
  #|END module+ test|#)


