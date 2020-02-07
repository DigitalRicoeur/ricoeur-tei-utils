#lang racket/base

(require ricoeur/tei/kernel
         (only-in adjutor/unstable multi))

(require-provide (provide-only ricoeur/tei/kernel)
                 (multi ricoeur/tei/base
                        (def-from-spec
                         get-page-breaks
                         guess-paragraphs
                         segments)))
