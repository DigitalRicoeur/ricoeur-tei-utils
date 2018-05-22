#lang racket/base

(require ricoeur/tei/kernel/sans-lang
         (submod "stxparam.rkt" private)
         racket/splicing
         syntax/parse/define
         (only-in "define.rkt"
                  define-combined-elements-specification)
         (for-syntax racket/base
                     racket/list
                     racket/sequence
                     syntax/parse
                     syntax/flatten-begin
                     racket/match
                     "ir/struct.rkt"
                     "ir/syntax-class.rkt"
                     "static-info.rkt"
                     ))

(require-provide (provide-only ricoeur/tei/kernel/sans-lang)
                 racket/contract
                 racket/match
                 racket/string
                 racket/list
                 "stxparam.rkt"
                 "adt.rkt"
                 )

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out
          [module-begin #%module-begin]
          ))


(begin-for-syntax
  (define-syntax-class spec-name-declaration
    #:description "spec name declaration"
    #:attributes {name to-extend}
    (pattern [#:spec name:id]
             #:with to-extend #'())
    (pattern [#:spec name:id
              (~alt (~once (~seq #:with-local local-name:id)
                           #:name "local name declaration")
                    (~once (~seq #:extends e:id ...+)
                           #:name "#:extends declaration"))
              ...]
             #:with to-extend #'(local-name [e ...]))))

(define-syntax-parser module-begin
  [(_ (~optional (~seq _:whitespace-str ...
                       decl:spec-name-declaration)
                 #:defaults
                 ([decl.name (datum->syntax this-syntax 'spec)]
                  [decl.to-extend #'()]))
      body ...)
   #:with doc (datum->syntax this-syntax 'doc)
   #:with for-doc-lang
   (datum->syntax this-syntax 'ricoeur/tei/kernel/lang/doc-lang)
   #`(#%module-begin
      (provide decl.name)
      (module* doc for-doc-lang
        doc body ...)
      (module+ test
        (require (submod ".." doc)))
      (splicing-syntax-parameterize
          ([define-element plain-d-element]
           [define-elements-together
             plain-d-elements-together])
        (stratify-body decl.name
                       decl.to-extend
                       ()
                       ()
                       (body ...))))])

(define-syntax-parser stratify-body
  [(_ name:id
      to-extend
      (for-spec-body:expr ...)
      (spec-form:plain-element-definition-stx ...)
      ())
   #:fail-unless (eq? (syntax-local-context) 'module)
   "only allowed in a module context"
   #`(unwrap-stratified name
                        to-extend
                        (for-spec-body ...)
                        (spec-form ...))]
  [(_ name:id
      to-extend
      (for-spec-body:expr ...)
      (spec-form:plain-element-definition-stx ...)
      (this to-go:expr ...))
   #:fail-unless (eq? (syntax-local-context) 'module)
   "only allowed in a module context"
   (syntax-parse (local-expand #'this
                               'module
                               (list #'begin-for-runtime
                                     #'plain-element-definition
                                     #'begin ;; it's implicitly added, but let's be clear
                                     ;; Need to not try to expand these:
                                     #'#%require
                                     #'#%provide
                                     ))
     #:literals {begin begin-for-runtime}
     [(begin body:expr ...)
      #:with (flattened ...) (flatten-all-begins
                              #'(begin body ...))
      #`(stratify-body name
                       to-extend
                       (for-spec-body ...)
                       (spec-form ...)
                       (flattened ... to-go ...))]
     [(begin-for-runtime body:expr ...)
      #`(stratify-body name
                       to-extend
                       (for-spec-body ... body ...)
                       (spec-form ...)
                       (to-go ...))]
     [new-spec:plain-element-definition-stx
      #`(stratify-body name
                       to-extend
                       (for-spec-body ...)
                       (spec-form ... new-spec)
                       (to-go ...))]
     [_
      #`(stratify-body name
                       to-extend
                       (for-spec-body ...)
                       (spec-form ...)
                       (to-go ...))])])

(define-for-syntax (error-inside-begin-for-runtime stx)
  (raise-syntax-error
   #f "not allowed inside begin-for-runtime" stx))

(define-syntax-parser unwrap-stratified
  [(_ name:id
      to-extend
      (for-spec-body:expr ...)
      (spec-form:plain-element-definition-stx ...))
   #`(splicing-syntax-parameterize
         ([begin-for-runtime nested-begin-for-runtime-transformer]
          [define-element error-inside-begin-for-runtime]
          [define-elements-together error-inside-begin-for-runtime])
       for-spec-body ...
       (prepare-spec name
                     to-extend
                     spec-form ...))])


(define-for-syntax ir->static-info-splice
  (match-lambda
    [(element-info
      _ name-stx wrapped-constructor-name
      (element-options children required-order
                       attr-contracts required-attrs
                       extra-check/false text?))
     (list
      #`'#,name-stx
      #`(element-static-info
         '#,name-stx
         #'#,name-stx
         #'#,wrapped-constructor-name
         #,text?
         #,(if children
               #`#''(#,@(map (match-lambda
                               [(child-spec _ repeat _ name)
                                #`[#,repeat #,name]])
                             children))
               #'#''())
         #,(if required-order
               #`#''(#,@required-order)
               #'#''())
         #,(if attr-contracts
               #`#'(list
                    #,@(map (match-lambda
                              [(attr-contract-info _ name _ protected)
                               #`(cons '#,name
                                       #,(syntax-local-lift-expression
                                          protected))])
                            attr-contracts))
               #'#''())
         #,(if required-attrs
               #`#''(#,@required-attrs)
               #'#''())
         #,(match extra-check/false
             [(extra-check _ protected)
              #`#'#,(syntax-local-lift-expression
                     protected)]
             [#f
              #'#'#f])))]))
                  
(define-for-syntax (ir->needed-elements-stxes ir)
  (define children?
    (element-options-children
     (element-info-options ir)))
  (if children?
      (map child-spec-name-stx 
           children?)
      null))




(define-syntax-parser prepare-spec
  [(_ name:id
      (local-name:id [e:id ...])
      spec-form:plain-element-definition-stx ...)
   (for ([extend-id (in-syntax #'(e ...))])
     (unless (specification-group-info?
              (syntax-local-value extend-id (λ () #f)))
       (raise-syntax-error
        #f "not an elements specification transformer"
        extend-id)))
   #`(begin (prepare-spec local-name () spec-form ...)
            (define-combined-elements-specification name
              [local-name e ...]))]
  [(_ name:id () spec-form:plain-element-definition-stx ...)
   #:fail-when (check-duplicate-identifier
                (syntax->list #'(spec-form.name ...)))
   "duplicate element name"
   #:with (raw-needed-name ...)
   (apply append (map ir->needed-elements-stxes
                      (attribute spec-form.parsed)))
   #`(define-syntax name
       (let ([present-assocs (list (cons 'spec-form.name
                                         #'spec-form.name)
                                   ...)]
             [present-syms '(spec-form.name ...)]
             [raw-needed-assocs (list (cons 'raw-needed-name
                                            #'raw-needed-name)
                                      ...)])
             
         (specification-group-info
          (hasheq #,@(apply append
                            (map ir->static-info-splice
                                 (attribute spec-form.parsed))))
          present-assocs
          (filter (λ (pr)
                    (not (memq (car pr) present-syms)))
                  (remove-duplicates raw-needed-assocs
                                     eq?
                                     #:key car)))))])









