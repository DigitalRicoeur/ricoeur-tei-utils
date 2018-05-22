#lang racket/base

(require adjutor)

;; This module (and its submodules) re-export
;; everything from ricoeur/tei/kernel that
;; is ok to use outside of the kernel layer.
;; Modules outside of the kernel should only access
;; kernel functionality through this module and its submodules.

;; The exports from this module are truly public.

(require-provide "kernel/sans-lang.rkt"
                 )

(module+ private
  ;; The private submodule is for use only in
  ;; the implementation of ricoeur/tei/base.
  ;; It provides forms and functions to help make
  ;; use of #lang ricoeur/tei/kernel.
  (require-provide (submod "kernel/sans-lang.rkt" private)
                   (submod "kernel/lang-exports.rkt" private)
                   ))

(module+ private-plain-TEI-info
  ;; This submodule is for use only in the
  ;; implementation of the teiHeader struct.
  (require-provide (submod "kernel/sans-lang.rkt"
                           private-plain-TEI-info)))

(module+ doc
  ;; The doc submodule provides things for Scribble
  ;; use in the guidelines document to interoperate
  ;; with sections implemented in #lang ricoeur/tei/kernel.
  ;; It is not for runtime use.
  (require-provide (submod "kernel/lang-exports.rkt" doc)
                   ))

