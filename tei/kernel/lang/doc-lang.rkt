#lang racket

;; This intentionally uses #lang racket as the base lang
;; for documentation-time.

(require "doc-support.rkt"
         (only-in scribble/manual/lang
                  [#%top-interaction scribble-top-interaction]
                  [#%module-begin scribble-module-begin])
         (submod "doc-support.rkt" private)
         racket/splicing
         (for-syntax "ir/struct.rkt"
                     "ir/syntax-class.rkt"
                     ))

(require-provide (provide-only "doc-support.rkt")
                 scribble/manual
                 "stxparam.rkt"
                 syntax/parse/define
                 (for-syntax racket/base
                             syntax/parse
                             racket/match
                             ))

(provide (except-out (all-from-out racket)
                     #%top-interaction
                     #%module-begin)
         (rename-out
          [for-doc-module-begin #%module-begin]
          [scribble-top-interaction #%top-interaction]
          ))

(define-for-syntax element-info->stx
  (match-lambda
    [(element-info _ name-stx _
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



