#lang racket/base

(require racket/contract
         racket/unit
         ricoeur/tei/base
         "backend/noop.rkt"
         "backend/regexp.rkt"
         "backend/postgresql.rkt"
         (submod "backend/common.rkt" private))

(require-provide "backend/common.rkt")

(module+ test
  (require rackunit
           (submod "..")))

(provide noop-searchable-document-set
         search-backend/c
         postgresql-data-source/c
         (contract-out
          [initialize-search-backend
           initialize-search-backend/c]
          ))

(define-compound-search-unit eager-search@
  noop@
  postgresql@
  regexp@)

(define-lazy-search-unit search@ eager-search@)

(define-values/invoke-unit/infer search@)

(module+ test
  (check-pred contract? search-backend/c)
  (check-equal?
   (searchable-document-set-do-term-search
    (initialize-search-backend 'regexp (instance-set))
    "apple")
   (instance-set)))

