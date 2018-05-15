#lang racket/base

(require racket/contract
         racket/match
         "xexpr/plain-contracts.rkt"
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
                  elements-only-element-get-body/elements-only
                  (-> elements-only-element? any)]
          [tei-element-to-xexpr
           (-> tei-element? normalized-xexpr-element/c)]
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
               [body/elements-only (listof tei-element?)])]
            [struct (content-containing-element tei-element)
              ([name symbol?]
               [attributes
                (listof (list/c symbol?
                                string?))]
               [body (listof (or/c tei-element?
                                   comment?
                                   p-i?
                                   string?))])])))
(module* private racket/base
  (require (submod ".." private*))
  (provide elements-only-element
           content-containing-element))


(struct tei-element (name attributes)
  #:transparent)


(struct elements-only-element tei-element (body body/elements-only)
  #:transparent)


(struct content-containing-element tei-element (body)
  #:transparent)


(define tei-element-get-body
  (match-lambda
    [(or (elements-only-element _ _ body _)
         (content-containing-element _ _ body))
     body]))

    
(define tei-element-to-xexpr
  (match-lambda
    [(or (elements-only-element name attrs body _)
         (content-containing-element name attrs body))
     (list* name attrs body)]))



