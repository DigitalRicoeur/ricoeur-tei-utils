#lang racket

(require "test3.rkt"
         "test4.rkt"
         "test-duplicate.rkt"
         ricoeur/tei/kernel
         (submod ricoeur/tei/kernel private)
         ricoeur/tei/spec-lang/link
         (submod ricoeur/tei/spec-lang/link
                 private))

(define-combined-elements-specification merged
  [custom-spec spec])

(show-elements-specification-transformer merged)
