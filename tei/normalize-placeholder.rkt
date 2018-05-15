#lang racket/base

(provide non-element-body->plain-text
         non-element-xexpr->plain-text
         nondestructive-normalize-xexpr-body-once
         )

(define (non-element-body->plain-text . args)
  ;; used by:
  ;;  - ricoeur/tei/base/specification/specification
  ;;  - ricoeur/tei/oop/classes
  (error 'non-element-body->plain-text
         "TODO (ricoeur/tei/normalize-placeholder)"))

(define (non-element-xexpr->plain-text . args)
  ;; used by:
  ;;  - ricoeur/tei/oop/interfaces/element
  (error 'non-element-xexpr->plain-text
         "TODO (ricoeur/tei/normalize-placeholder)"))

(define (nondestructive-normalize-xexpr-body-once . args)
  ;; used by:
  ;;  - ricoeur/tei/oop/objects
  (error 'nondestructive-normalize-xexpr-body-once
         "TODO (ricoeur/tei/normalize-placeholder)"))
