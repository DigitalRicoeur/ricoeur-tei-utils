#lang racket/base

(require ricoeur/tei/kernel
         )

(require-provide (provide-only ricoeur/tei/kernel)
                 (multi ricoeur/tei/base
                        (def-from-spec
                         get-page-breaks
                         guess-paragraphs
                         segments
                         )))

