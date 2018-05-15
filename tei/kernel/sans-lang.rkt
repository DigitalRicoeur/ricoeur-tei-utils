#lang racket/base

(require adjutor)

;; This module (and its submodules) re-export
;; everything from ricoeur/tei/kernel that
;; is ok to use outside of the kernel layer,
;; except for the lang related things.

;; The exports from this module are truly public.

(require-provide "base-structs.rkt"
                 "tei-info.rkt"
                 "xexpr/plain-contracts.rkt"
                 )

(module+ private
  ;; The private submodule is for use only in
  ;; the implementation of ricoeur/tei/base.
  ;; It provides forms and functions to help make
  ;; use of #lang ricoeur/tei/kernel.
  (require-provide "xexpr/contract-utils.rkt"
                   (submod "tei-info.rkt" private)
                   ))
