#lang racket/base

(require adjutor)

;; pre-kernel-lib re-exports some things that have no
;; TEI-related dependencies and no other expensive
;; dependencies.

(require-provide (multi "pre-kernel-lib"
                        ("maybe.rkt"
                         "utils.rkt"
                         "xml-path.rkt"
                         "output-to-file.rkt"
                         ))
                 (provide-only adjutor)
                 gregor)
 
