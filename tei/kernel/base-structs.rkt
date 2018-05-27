#lang racket/base

;; Note blame issue:
;; https://github.com/racket/racket/issues/2093
;; Will be fixed in Racket 7

(require racket/contract
         racket/match
         racket/string
         "xexpr/plain-contracts.rkt"
         "xexpr/normalize.rkt"
         )

(provide tei-element?
         elements-only-element?
         content-containing-element?
         (contract-out
          [rename tei-element-name
                  tei-element-get-name
                  (-> tei-element? any)]
          [rename tei-element-attributes
                  tei-element-get-attributes
                  (-> tei-element? any)]
          [tei-element-get-body
           (-> tei-element? any)]
          [rename elements-only-element-body/elements-only
                  tei-get-body/elements-only
                  (-> elements-only-element? any)]
          [tei-element->xexpr
           (-> tei-element? normalized-xexpr-element/c)]
          [element-or-xexpr->plain-text
           (-> (or/c tei-element? raw-xexpr-atom/c) string?)]
          [prop:element->plain-text ;; should this be semi-private?
           (struct-type-property/c
            (-> any/c string?))]
          ))



(module* private* #f
  (provide (contract-out
            [struct (elements-only-element tei-element)
              ([name symbol?]
               [attributes
                (listof (list/c symbol?
                                string?))]
               [body (listof (or/c tei-element?
                                   comment?
                                   p-i?))]
               [body/elements-only (listof tei-element?)])
              #:omit-constructor]
            [struct (content-containing-element tei-element)
              ([name symbol?]
               [attributes
                (listof (list/c symbol?
                                string?))]
               [body (listof (or/c tei-element?
                                   normalized-xexpr-atom/c))])
              #:omit-constructor])))
(module* private racket/base
  (require (submod ".." private*))
  (provide elements-only-element
           content-containing-element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values {prop:element->plain-text
                has-prop:element->plain-text?
                get:element->plain-text}
  (make-struct-type-property 'prop:element->plain-text))

(define (element->plain-text e)
  ((get:element->plain-text e) e))

(define (element-or-xexpr->plain-text e/xs)
  (if (has-prop:element->plain-text? e/xs)
      (element->plain-text e/xs)
      (non-element-xexpr->plain-text e/xs)))



(struct tei-element (name attributes)
  #:transparent)


(struct elements-only-element tei-element (body body/elements-only)
  #:transparent
  #:property prop:element->plain-text
  (λ (this)
    (string-join (map element->plain-text
                      (elements-only-element-body/elements-only this))
                 "")))

(struct content-containing-element tei-element (body)
  #:transparent
  #:property prop:element->plain-text
  (λ (this)
    (string-join (map element-or-xexpr->plain-text
                      (content-containing-element-body this))
                 "")))

(define tei-element-get-body
  (match-lambda
    [(or (elements-only-element _ _ body _)
         (content-containing-element _ _ body))
     body]))

    
(define tei-element->xexpr
  (match-lambda
    [(or (elements-only-element name attrs body _)
         (content-containing-element name attrs body))
     (list* name attrs (map tei-element-to-xexpr* body))]))

(define (tei-element-to-xexpr* x)
  (if (tei-element? x)
      (tei-element->xexpr x)
      x))

