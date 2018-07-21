#lang racket/base

(require adjutor
         )

(require-provide ricoeur/tei/kernel
                 (multi ricoeur/tei/base
                        (def-from-spec
                         get-page-breaks
                         guess-paragraphs
                         segments
                         )))

