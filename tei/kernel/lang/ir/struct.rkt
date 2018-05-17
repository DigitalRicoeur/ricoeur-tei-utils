#lang racket/base

(require racket/contract
         )

(provide (contract-out
          [struct attr-contract-info
            ([name-sym symbol?]
             [name-stx identifier?]
             [plain-contract-stx syntax?]
             [protected-contract-stx syntax?])]
          [struct extra-check
            ([plain-stx syntax?]
             [protected-stx syntax?])]
          [struct child-spec
            ([repeat-datum (or/c 1 '1+ '0-1 '0+)]
             [repeat-stx syntax?]
             [name-sym symbol?]
             [name-stx identifier?])]
          [struct element-options
            ([children (or/c #f (listof child-spec?))]
             [required-order (or/c #f (listof identifier?))]
             [attr-contracts (or/c #f (listof attr-contract-info?))]
             [required-attrs (or/c #f (listof identifier?))]
             [extra-check (or/c #f extra-check?)]
             [text? boolean?])]
          [struct element-info
            ([name-sym symbol?]
             [name-stx identifier?]
             [wrapped-constructor-name identifier?]
             [options element-options?])]
          [struct element-definition-group
            ([elements (listof element-info?)]
             [inset?-expr syntax?]
             [body (listof syntax?)])]
          ))

(struct attr-contract-info (name-sym
                            name-stx
                            plain-contract-stx
                            protected-contract-stx)
  #:transparent)

(struct extra-check (plain-stx protected-stx)
  #:transparent)

(struct child-spec (repeat-datum repeat-stx name-sym name-stx)
  #:transparent)

(struct element-options (children
                         required-order
                         attr-contracts
                         required-attrs
                         extra-check
                         text?)
  #:transparent)

(struct element-info (name-sym name-stx wrapped-constructor-name options)
  ;; TODO: datatype definition
  #:transparent)

(struct element-definition-group (elements
                                  inset?-expr
                                  body)
  ;; TODO: support local definitions
  #:transparent)
  







