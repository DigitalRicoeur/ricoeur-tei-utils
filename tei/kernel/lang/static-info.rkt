#lang racket/base

(require syntax/parse
         racket/contract
         )

(provide elements-specification-transformer
         (contract-out
          [struct specification-group-info
           ([elements (hash/c symbol? element-static-info?)]
            [elements-present-assocs
             (listof (cons/c symbol? identifier?))]
            [elements-needed-assocs
             (listof (cons/c symbol? identifier?))])]
          [struct element-static-info
            ([name-sym symbol?]
             [name-stx identifier?]
             [text? boolean?]
             [children-stx syntax?]
             [required-order-stx syntax?]
             [attr-contracts-stx syntax?]
             [required-attrs-stx syntax?]
             [extra-check-stx syntax?])]
          ))
            

(struct specification-group-info (elements
                                  elements-present-assocs
                                  elements-needed-assocs
                                  )
  #:transparent
  #:property prop:procedure
  (λ (this stx)
    (raise-syntax-error
     #f
     "elements specification transformer used out of contect"
     stx)))

(struct element-static-info (name-sym
                             name-stx
                             text?
                             children-stx
                             required-order-stx
                             attr-contracts-stx
                             required-attrs-stx
                             extra-check-stx)
  #:transparent)


(define-syntax-class elements-specification-transformer
  #:description "elements specification transformer"
  #:attributes {parsed}
  (pattern it:id
           #:do [(define val
                   (syntax-local-value #'it))]
           #:fail-unless (specification-group-info? val)
           "not an elements specification transformer"
           #:attr parsed val))





