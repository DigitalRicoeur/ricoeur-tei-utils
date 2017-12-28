#lang racket

(require "common.rkt"
         (submod "common.rkt" private)
         )

(provide noop-searchable-document-set)

(define noop-searchable-document-set
  (new (class abstract-searchable-document-set%
         (super-new)
         (define/override-final (do-search-documents term
                                                     #:ricoeur-only? [ricoeur-only? #t]
                                                     #:exact? [exact? #f])
           '()))))

