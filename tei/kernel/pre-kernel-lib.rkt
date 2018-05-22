#lang racket/base

(require adjutor)

;; pre-kernel-lib re-exports some things that have no
;; TEI-related dependencies and no other expensive
;; dependencies.

(require-provide "pre-kernel-lib/maybe.rkt"
                 "pre-kernel-lib/utils.rkt"
                 adjutor
                 gregor)
 
