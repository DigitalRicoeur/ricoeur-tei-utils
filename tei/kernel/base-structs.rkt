#lang racket/base

(require racket/contract
         racket/match
         racket/string
         racket/struct
         "pre-kernel-lib.rkt"
         "xexpr/plain-contracts.rkt"
         "xexpr/normalize.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide tei-element?
         elements-only-element?
         content-containing-element?
         (rename-out
          [match:tei-element tei-element]
          [match:elements-only-element elements-only-element]
          [match:content-containing-element content-containing-element])
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
          [tei-element->xexpr*
           (-> (or/c tei-element? normalized-xexpr-atom/c)
               normalized-xexpr/c)]
          [element-or-xexpr->plain-text
           (->* {(or/c tei-element? raw-xexpr-atom/c)}
                {#:include-header? any/c}
                string-immutable/c)]
          ))

(module+ for-lang
  (provide (contract-out
            [prop:element->plain-text 
             (struct-type-property/c
              (or/c (-> any/c string?)
                    (-> any/c boolean? string?)))]
            )))

(module* private* #f
  (provide (contract-out
            [prop:element-fields-to-print
             (struct-type-property/c
              (listof boolean?))]
            [struct (elements-only-element tei-element)
              ([name symbol?]
               [attributes
                (listof (list/c symbol?
                                string-immutable/c))]
               [body (listof (or/c tei-element?
                                   normalized-comment/c
                                   normalized-p-i/c))]
               [body/elements-only (listof tei-element?)])
              #:omit-constructor]
            [struct (content-containing-element tei-element)
              ([name symbol?]
               [attributes
                (listof (list/c symbol?
                                string-immutable/c))]
               [body (listof (or/c tei-element?
                                   normalized-xexpr-atom/c))])
              #:omit-constructor])))
(module* private racket/base
  (require (submod ".." private*))
  (provide (rename-out
            [elements-only-element super:elements-only-element]
            [content-containing-element super:content-containing-element])
           prop:element-fields-to-print
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values {prop:element->plain-text
                has-prop:element->plain-text?
                get:element->plain-text}
  (make-struct-type-property
   'prop:element->plain-text
   (λ (proc info)
     (if (procedure-arity-includes? proc 2)
         proc
         (λ (e _)
           (proc e))))))

(define (element->plain-text e include-header?)
  ((get:element->plain-text e) e include-header?))

(define (element-or-xexpr->plain-text e/xs
                                      #:include-header? [include-header? #t])
  (string->immutable-string
   (if (has-prop:element->plain-text? e/xs)
       (element->plain-text e/xs (any->boolean include-header?))
       (non-element-xexpr->plain-text e/xs))))



(struct tei-element (name attributes)
  #:property prop:custom-print-quotable 'never
  #:transparent)

(define (make-element-write-proc ctor-name field-count)
  (make-constructor-style-printer
   (λ (_) ctor-name)
   (λ (this)
     (match-define (or (elements-only-element name attrs body _)
                       (content-containing-element name attrs body))
       this)
     (define specific-fields
       (for/list ([f (in-list (list-tail (struct->list this) field-count))]
                  [print? (in-list (element-get-fields-to-print this))]
                  #:when print?)
         f))
     (list* name
            attrs
            (if (null? specific-fields)
                (list body)
                (list (unquoted-printing-string "#:fields")
                      specific-fields
                      (unquoted-printing-string "#:body")
                      body))))))

(struct elements-only-element tei-element (body body/elements-only)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-element-write-proc 'elements-only-element 4))]
  #:property prop:element->plain-text
  (λ (this)
    (string-join (map element->plain-text
                      (elements-only-element-body/elements-only this))
                 "")))

(struct content-containing-element tei-element (body)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-element-write-proc 'content-containing-element 3))]
  #:property prop:element->plain-text
  (λ (this)
    (string-join (map element-or-xexpr->plain-text
                      (content-containing-element-body this))
                 "")))

(define-match-expander match:content-containing-element
  (syntax-parser
    [(_ name-pat attr-pat body-pat)
     #'(content-containing-element name-pat attr-pat body-pat)]))

(define-match-expander match:elements-only-element
  (syntax-parser
    [(_ name-pat attr-pat body-pat
        (~optional (~seq #:elements-only e-o-pat)
                   #:defaults ([e-o-pat #'_])))
     #'(elements-only-element name-pat attr-pat body-pat e-o-pat)]))

(define-match-expander match:tei-element
  (syntax-parser
    [(_ name-pat attr-pat body-pat)
     #'(or (match:elements-only-element name-pat attr-pat body-pat)
           (content-containing-element name-pat attr-pat body-pat))]))
           

(define tei-element-get-body
  (match-lambda
    [(or (elements-only-element _ _ body _)
         (content-containing-element _ _ body))
     body]))

    
(define tei-element->xexpr
  (match-lambda
    [(or (elements-only-element name attrs body _)
         (content-containing-element name attrs body))
     (list* name attrs (map tei-element->xexpr* body))]))

(define (tei-element->xexpr* x)
  (if (tei-element? x)
      (tei-element->xexpr x)
      x))

(define-values {prop:element-fields-to-print
                has-prop:element-fields-to-print?
                element-get-fields-to-print}
  (make-struct-type-property 'prop:element-fields-to-print))





