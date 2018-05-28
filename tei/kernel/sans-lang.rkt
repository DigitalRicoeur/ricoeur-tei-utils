#lang racket/base

(require "pre-kernel-lib.rkt")

;; This module (and its submodules) re-export
;; everything from ricoeur/tei/kernel that
;; is ok to use outside of the kernel layer,
;; except for the lang related things.

;; The exports from this module are truly public.

(require-provide "base-structs.rkt"
                 "tei-info.rkt"
                 "interfaces.rkt"
                 "xmllint.rkt"
                 "xexpr/plain-contracts.rkt"
                 "xexpr/normalize.rkt"
                 (provide-only "pre-kernel-lib.rkt")
                 )

(module+ private
  ;; The private submodule is for use only in
  ;; the implementation of ricoeur/tei/base.
  ;; It provides forms and functions to help make
  ;; use of #lang ricoeur/tei/kernel.
  (require-provide "xexpr/contract-utils.rkt"
                   ))

(module+ private-plain-TEI-info
  ;; This submodule is for use only in the
  ;; implementation of the teiHeader struct.
  (require-provide (submod "tei-info.rkt" private)
                   ))

