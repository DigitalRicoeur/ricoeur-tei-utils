#lang racket/base

(require racket/contract
         racket/unit
         ricoeur/tei/base
         "search/noop.rkt"
         "search/regexp.rkt"
         "search/postgresql.rkt"
         (submod "search/common.rkt" private)
         )

(require-provide "search/common.rkt"
                 )

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
  search-backend/c
  (searchable-document-set-do-term-search
   (initialize-search-backend 'regexp (instance-set))
   "apple"))

