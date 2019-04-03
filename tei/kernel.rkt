#lang racket/base

(require ricoeur/kernel/pre-kernel-lib)

;; This module (and its submodules) re-export
;; everything from ricoeur/tei/kernel that
;; is ok to use outside of the kernel layer.
;; Modules outside of the kernel should only access
;; kernel functionality through this module and its submodules.

;; The exports from this module are truly public.

(require-provide "kernel/base-structs.rkt"
                 "kernel/instance-info.rkt"
                 "kernel/instance-set.rkt"
                 "kernel/interfaces.rkt"
                 "kernel/xmllint.rkt"
                 "kernel/xexpr/plain-contracts.rkt"
                 "kernel/xexpr/normalize.rkt"
                 (provide-only ricoeur/kernel/pre-kernel-lib))

(module+ private
  ;; The private submodule is for use only in
  ;; the implementation of ricoeur/tei/base.
  ;; It provides forms and functions to help make
  ;; use of #lang ricoeur/tei/spec-lang.
  (require-provide "kernel/xexpr/contract-utils.rkt"
                   (submod "kernel/interfaces.rkt" private)))

(module+ private-plain-instance-info
  ;; This submodule is for use only in the
  ;; implementation of the teiHeader struct.
  (require-provide (submod "kernel/instance-info.rkt" private)))

