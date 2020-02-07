#lang racket/base

(require racket/contract
         racket/match
         racket/string
         racket/struct
         ricoeur/kernel/pre-kernel-lib
         "xexpr/plain-contracts.rkt"
         "xexpr/normalize.rkt"
         (only-in adjutor/unstable TODO/void)
         (for-syntax racket/base
                     syntax/parse))

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
           (-> (or/c tei-element? raw-xexpr-atom/c)
               string-immutable/c)]
          ))

(module+ for-lang
  (provide (contract-out
            [prop:element->plain-text 
             (struct-type-property/c
              (-> any/c string?))]
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

(TODO/void Add macro to codify invariants
           #: This module has lots of invariants
           about subtypes of tei-element
           that are enforced by the "DSL," but
           their logic is spread across multiple files.
           Add a macro in the style of define-struct/derived
           to codify those invariants locally.)
           
(define-values {prop:element->plain-text
                has-prop:element->plain-text?
                get:element->plain-text}
  (make-struct-type-property
   'prop:element->plain-text))

(define (element->plain-text e)
  (string->immutable-string
   ((get:element->plain-text e) e)))

(define (element-or-xexpr->plain-text e/xs)
  (if (has-prop:element->plain-text? e/xs)
      (element->plain-text e/xs)
      (non-element-xexpr->plain-text e/xs)))

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

(define-syntax-rule
  (make-default-to-plain-text-proc get-children
                                   child->plain-text)
  ;; macro to avoid use-before-definition of accessor
  (λ (this)
    (string-join
     (for/list ([child (in-list (get-children this))])
       (child->plain-text child))
     "")))

(struct elements-only-element tei-element (body body/elements-only)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-element-write-proc 'elements-only-element 4))]
  #:property prop:element->plain-text
  (make-default-to-plain-text-proc elements-only-element-body/elements-only
                                   element->plain-text))

(struct content-containing-element tei-element (body)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-element-write-proc 'content-containing-element 3))]
  #:property prop:element->plain-text
  (make-default-to-plain-text-proc content-containing-element-body
                                   element-or-xexpr->plain-text))

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





