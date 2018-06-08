#lang racket/base

;; Are maybe-date<? and maybe-date>? still used?
;; If so, add them here.
;; If not, they should probably be dropped, since
;; a future use might want different semantics for (nothing).

(require racket/match
         racket/contract
         )

(module+ test
  (require rackunit
           (submod "..")))

(provide (contract-out
          [title<?
           (-> string? string? any/c)]
          [attributes-ref
           (-> (listof (list/c symbol? string?))
               symbol?
               (or/c #f string?))]
          ))

(define (attributes-ref attrs k)
  (define rslt
    (assq k attrs))
  (and rslt (cadr rslt)))

(define (title<? a b)
  (string-ci<? (normalize-title a) (normalize-title b)))

(define normalize-title
  (match-lambda
    [(pregexp #px"^(?i:an?|the)\\s+(\\S.*)$" (list _ trimmed))
     trimmed]
    [full full]))

(module+ test
  (check-equal? (normalize-title "The Rain in Spain")
                "Rain in Spain"
                "normalize-title: remove \"The\"")
  (check-equal? (normalize-title "A Night to Remember")
                "Night to Remember"
                "normalize-title: remove \"A\"")
  (check-equal? (normalize-title "An Old Book")
                "Old Book"
                "normalize-title: remove \"An\"")
  (check-equal? (normalize-title "Journals")
                "Journals"
                "normalize-title: preserve normal titles")
  (check-equal? (normalize-title "The      ")
                "The      "
                "normalize-title: don't produce empty strings")
  (check-equal? (normalize-title "Theories")
                "Theories"
                "normalize-title: require word break")
  (check-equal? (normalize-title "Another Day")
                "Another Day"
                "normalize-title: require word break"))
  


