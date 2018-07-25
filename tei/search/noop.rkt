#lang racket

(require "common.rkt"
         (submod "common.rkt" private)
         )

(provide noop@
         noop-searchable-document-set
         )

(define noop-searchable-document-set
  (new (class* object% {searchable-document-set<%>}
         (super-new)
         (define/public-final (do-term-search t
                                              #:ricoeur-only? r
                                              #:book/article b
                                              #:exact? e)
           '()))))

(define search-backend/c
  'noop)

(define (initialize-search-backend docs)
  noop-searchable-document-set)

(define-unit-from-context noop@
  search^)
