#lang racket/base

(require adjutor)

;; This is structured like sans-lang.rkt,
;; but for the things from the lang.
;; There are no "public" exports from the lang.

(module+ private
  (require-provide "lang/link.rkt"
                   ))

(module+ doc
  ;; The doc submodule provides things for Scribble
  ;; use in the guidelines document to interoperate
  ;; with sections implemented in #lang ricoeur/tei/kernel.
  ;; It is not for runtime use.
  (require-provide "lang/doc-support.rkt"
                   ))

