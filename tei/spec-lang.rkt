#lang racket/base

;; There are no "public" exports from the lang.

(module reader ricoeur/core-doc-lang/module-reader
  ricoeur/tei/spec-lang/specification-lang)

(require adjutor)

(module+ private
  ;; The private submodule is for use only in
  ;; the implementation of ricoeur/tei/base.
  ;; It provides forms and functions to help make
  ;; use of #lang ricoeur/tei/spec-lang.
  (require-provide "spec-lang/link.rkt"))

(module+ doc
  ;; The doc submodule provides things for Scribble
  ;; use in the guidelines document to interoperate
  ;; with sections implemented in #lang ricoeur/tei/kernel.
  ;; It is not for runtime use.
  (require-provide "spec-lang/doc-support.rkt"))
