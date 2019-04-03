#lang racket/base

(require syntax/parse
         racket/contract
         racket/match
         racket/hash
         racket/list
         )

(provide elements-specification-transformer
         (contract-out
          [specification-group-info-union
           (-> specification-group-info? ...
               specification-group-info?)]
          [specification-group-info->stx
           (-> specification-group-info? syntax?)]
          [struct specification-group-info
            ([elements (hash/c symbol? element-static-info?)]
             [elements-present-assocs
              (listof (cons/c symbol? identifier?))]
             [elements-needed-assocs
              (listof (cons/c symbol? identifier?))])]
          [struct element-static-info
            ([name-sym symbol?]
             [name-stx identifier?]
             [wrapped-constructor-name identifier?]
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

(define (specification-group-info-union . s)
  (define present-assocs
    (apply append
           (map specification-group-info-elements-present-assocs
                s)))
  (define present-syms
    (map car present-assocs))
  (specification-group-info
   (apply hash-union
          (map specification-group-info-elements
               s))
   present-assocs
   (filter (λ (pr)
             (not (memq (car pr) present-syms)))
           (remove-duplicates
            (apply append
                   (map specification-group-info-elements-needed-assocs
                        s))
            eq?
            #:key car))))

(define specification-group-info->stx
  (match-lambda
    [(specification-group-info elements
                               elements-present-assocs
                               elements-needed-assocs)
     #`(specification-group-info
        (hasheq
         #,@(apply
             append
             (for/list ([e (in-hash-values elements)])
               (match-define (element-static-info
                              _
                              name-stx
                              wrapped-constructor-name
                              text?
                              children-stx
                              required-order-stx
                              attr-contracts-stx
                              required-attrs-stx
                              extra-check-stx)
                 e)
               (list
                #`'#,name-stx
                #`(element-static-info
                   '#,name-stx
                   #'#,name-stx
                   #'#,wrapped-constructor-name
                   #,text?
                   #'#,children-stx
                   #'#,required-order-stx
                   #'#,attr-contracts-stx
                   #'#,required-attrs-stx
                   #'#,extra-check-stx)))))
        #,(assocs->stx elements-present-assocs)
        #,(assocs->stx elements-needed-assocs))]))

(define (assocs->stx assocs)
  (define/syntax-parse (name ...)
    (map cdr assocs))
  #'(list (cons 'name #'name) ...))

(struct element-static-info (name-sym
                             name-stx
                             wrapped-constructor-name
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
                   (syntax-local-value #'it (λ () #f)))]
           #:fail-unless (specification-group-info? val)
           "not an elements specification transformer"
           #:attr parsed val))





