#lang racket

#|

;; TODO: I think it might make sense in connection with 
;; this ongoing round of breaking API changes
;; to split the TEI-info interface into a bibliographic information part
;; and a document-specific part, so that the bibliographic part
;; might eventually come from our bibliographic database rather than
;; the TEI XML files.
;; The document-specific part might eventually contain the
;; value from tei-document-md5 as well as tei-document-paragraphs-status.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Should there be an abstraction for pb<%>-like sophistication of
;; n attributes to be shared by note<%> and div<%>?
;; Or do we ever really care about handling those numerically?
;; Maybe for TEI Lint?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Condition currently needed for xmllint to avoid a warning:
(and pages-ok?
     (not (eq? 'todo (send val get-guess-paragraphs-status)))
     (date<=? (send val get-original-publication-date)
              (send val get-publication-date))
     (force promise:ricoeur-xml:id-ok?))

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