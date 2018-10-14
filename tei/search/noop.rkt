#lang racket/base

(require "common.rkt"
         (submod "common.rkt" private)
         ricoeur/tei/base
         racket/class
         racket/unit
         )

(provide noop@
         noop-searchable-document-set
         )

(define noop-searchable-document-set
  (new (class* object% {searchable-document-set<%>}
         (super-new)
         (define/public-final (do-term-search t
                                              #:ricoeur-only? r
                                              #:languages st:lang
                                              #:book/article b
                                              #:exact? e)
           (instance-set)))))

(define-unit/search^ noop@
  (import)
  (export search^)
  (define search-backend/c
    'noop)
  (define (initialize-search-backend _ docs)
    noop-searchable-document-set))

