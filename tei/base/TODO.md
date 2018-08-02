#lang racket

#|

check invariants about note transl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Should there be an abstraction for pb<%>-like sophistication of
;; n attributes to be shared by note<%> and div<%>?
;; Or do we ever really care about handling those numerically?
;; Maybe for TEI Lint?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; things that should set prop:element->plain-text but don't:
  - list and item
  - sp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The transition to expressing contracts in the DSL
;; might support generation of more than just the whole-element
;; x-expression contracts we have right now: for instance,
;; the body portion could be a separately usable contract.
;; More ambitiously, basically the same checks could probably
;; be compiled for element structs as for xexprs, potentially
;; enabling functional update without converting all the
;; unchanged portions to xexprs and back.

|#