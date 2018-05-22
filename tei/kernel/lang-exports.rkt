#lang racket/base

;; This is structured like sans-lang.rkt,
;; but for the things from the lang.

(module+ private
  (require "lang/define.rkt"
           )
  (provide define-values/elements-specifications
           ))

(module+ doc
  ;; The doc submodule provides things for Scribble
  ;; use in the guidelines document to interoperate
  ;; with sections implemented in #lang ricoeur/tei/kernel.
  ;; It is not for runtime use.
  (require "lang/doc-support.rkt"
           )
  (provide tag
           attr
           make-other-doc-tag
           ))