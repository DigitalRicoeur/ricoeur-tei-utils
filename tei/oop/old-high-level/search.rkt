#lang racket/base

(require adjutor)

(require-provide (multi ricoeur/tei/oop/old-high-level/search
                        (common
                         regexp
                         postgresql
                         noop)))
