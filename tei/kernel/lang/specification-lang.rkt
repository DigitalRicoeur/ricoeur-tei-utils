#lang racket/base

(require "stxparam.rkt"
         (submod "stxparam.rkt" private)
         racket/contract
         racket/match
         racket/splicing
         syntax/parse/define
         ricoeur/tei/kernel/sans-lang
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     syntax/flatten-begin
                     racket/match
                     "ir/struct.rkt"
                     "ir/syntax-class.rkt"
                     "static-info.rkt"
                     ))

(provide (except-out (all-from-out racket/base)
                     provide
                     #%module-begin)
         (all-from-out ricoeur/tei/kernel/sans-lang)
         (all-from-out "stxparam.rkt")
         (all-from-out racket/contract)
         (all-from-out racket/match)
         (rename-out
          [module-begin #%module-begin]
          ))

(module+ for-doc
  (require scribble/manual
           (only-in scribble/manual/lang
                    [#%top-interaction scribble-top-interaction]
                    [#%module-begin scribble-module-begin])
           "doc-support.rkt"
           (submod "doc-support.rkt" private)
           "stxparam.rkt"
           (except-in racket
                      link
                      tag
                      ))
  (provide (except-out (all-from-out racket)
                       #%top-interaction
                       #%module-begin)
           (all-from-out scribble/manual)
           (all-from-out "stxparam.rkt")
           (all-from-out "doc-support.rkt")
           (rename-out
            [for-doc-module-begin #%module-begin]
            [scribble-top-interaction #%top-interaction]
            ))
  #|END module+ for-doc|#)


(define-syntax-parser module-begin
  [(_ (~optional (~seq _:whitespace-str ...
                       (~describe "spec name declaration"
                                  [#:spec name:id]))
                 #:defaults
                 ([name (datum->syntax this-syntax 'spec)]))
      body ...)
   #:with doc (datum->syntax this-syntax 'doc)
   #:with for-doc-lang
   (datum->syntax this-syntax
                  '(submod ricoeur/tei/kernel/lang/specification-lang
                           for-doc))
   #`(#%module-begin
      (provide name)
      (module doc for-doc-lang
        doc body ...)
      (module+ test
        (require (submod ".." doc)))
      (splicing-syntax-parameterize
          ([define-element plain-d-element]
           [define-elements-together
             plain-d-elements-together])
        (stratify-body name
                       ()
                       ()
                       (body ...))))])

  

(define-syntax-parser stratify-body
  [(_ name:id
      (for-spec-body:expr ...)
      (spec-form:plain-element-definition-stx ...)
      ())
   #:fail-unless (eq? (syntax-local-context) 'module)
   "only allowed in a module context"
   #`(unwrap-stratified name
                        (for-spec-body ...)
                        (spec-form ...))]
  [(_ name:id
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
                                     ;; need to not try to expand these
                                     #'#%require
                                     #'#%provide
                                     ))
     #:literals {begin begin-for-runtime}
     [(begin body:expr ...)
      #:with (flattened ...) (flatten-all-begins
                              #'(begin body ...))
      #`(stratify-body name
                       (for-spec-body ...)
                       (spec-form ...)
                       (flattened ... to-go ...))]
     [(begin-for-runtime body:expr ...)
      #`(stratify-body name
                       (for-spec-body ... body ...)
                       (spec-form ...)
                       (to-go ...))]
     [new-spec:plain-element-definition-stx
      #`(stratify-body name
                       (for-spec-body ...)
                       (spec-form ... new-spec)
                       (to-go ...))]
     [_
      #`(stratify-body name
                       (for-spec-body ...)
                       (spec-form ...)
                       (to-go ...))])])


(define-syntax-parser unwrap-stratified
  [(_ name:id
      (for-spec-body:expr ...)
      (spec-form:plain-element-definition-stx ...))
   #`(splicing-syntax-parameterize
         ([begin-for-runtime nested-begin-for-runtime-transformer])
       for-spec-body ...
       (prepare-spec name spec-form ...))])


(define-for-syntax ir->static-info-splice
  (match-lambda
    [(element-info
      _ name-stx
      (element-options children required-order
                       attr-contracts required-attrs
                       extra-check/false text?))
     (list
      #`'#,name-stx
      #`(element-static-info
         '#,name-stx
         #'#,name-stx
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
  [(_ name:id spec-form:plain-element-definition-stx ...)
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

   
;                                                          
;                                                          
;                                                          
;                                                          
;       ;;;                             ;;                 
;     ;;                                ;;                 
;   ;;;;;;;  ;;;    ;; ;;;           ;;;;;   ;;;       ;;; 
;     ;;    ;   ;   ;;;             ;   ;;  ;   ;    ;;   ;
;     ;;    ;   ;   ;;      ;;;;;;  ;   ;;  ;   ;    ;     
;     ;;   ;;   ;;  ;;             ;;   ;; ;;   ;;  ;;     
;     ;;    ;   ;   ;;              ;   ;;  ;   ;    ;     
;     ;;    ;   ;   ;;              ;   ;;  ;   ;    ;;   ;
;     ;;     ;;;    ;;               ;;; ;   ;;;       ;;; 
;                                                          
;                                                          
;                                                          
;                                                          

(module+ for-doc

  (define-for-syntax element-info->stx
    (match-lambda
      [(element-info _ name-stx 
                     (element-options children
                                      required-order
                                      attr-contracts
                                      required-attrs
                                      _
                                      text?))
       #`(make-element-rows
          #:name '#,name-stx
          #:children
          #,(if children
                #`'(#,@(map (match-lambda
                              [(child-spec _ repeat-stx
                                           _ name-stx)
                               #`[#,name-stx . #,repeat-stx]])
                            children))
                #'#f)
          #:required-order #,(if required-order
                                 #`'(#,@required-order)
                                 #'#f)
          #:attr-contracts
          #,(if attr-contracts
                #`(list #,@(map (match-lambda
                                  [(attr-contract-info
                                    _ name-stx
                                    plain-contract-stx _)
                                   #`(cons '#,name-stx
                                           (racketblock0
                                            #,plain-contract-stx))])
                                attr-contracts))
                #'#f)
          #:required-attrs #,(if required-attrs
                                 #`'(#,@required-attrs)
                                 #'#f)
          #:text? #,(if text? #'#t #'#f))]))
  
  
  (define-for-syntax element-definition-group->scribble
    (match-lambda
      [(element-definition-group elements
                                 inset?-expr
                                 body)
       #`(make-defelement-flow
          #:inset? #,inset?-expr
          #:elements (list #,@(map element-info->stx
                                   elements))
          #,@body)]))

  (define-for-syntax scribble-d-element
    (syntax-parser
      [(_ it:define-element/rest)
       (element-definition-group->scribble
        (attribute it.parsed))]))
  
  (define-for-syntax scribble-d-elements-together
    (syntax-parser
      [(_ it:define-elements-together/rest)
       (element-definition-group->scribble
        (attribute it.parsed))]))

  (define-syntax-parser for-doc-module-begin
    [(_ doc:id body:expr ...)
     #`(scribble-module-begin
        doc
        (splicing-syntax-parameterize
            ([define-element scribble-d-element]
             [define-elements-together
               scribble-d-elements-together])
          body ...))])
  
  #|END module+ for-doc|#)






