#lang racket/base

(require (submod "common.rkt" private)
         ricoeur/tei/base
         racket/class
         racket/unit)

(provide noop@
         noop-searchable-document-set)

(define-unit-from-context noop@ search^)

(module+ test
  (define-values/invoke-unit/infer noop@))

(define noop-searchable-document-set
  (new (class* object% {searchable-document-set<%>}
         (super-new)
         (define/public-final (do-term-search t
                                              #:ricoeur-only? r
                                              #:languages st:lang
                                              #:book/article b
                                              #:exact? e)
           (instance-set)))))

(define search-backend/c
  'noop)

(define (initialize-search-backend _ docs)
  noop-searchable-document-set)
