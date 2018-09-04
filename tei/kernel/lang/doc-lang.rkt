#lang racket

;; This intentionally uses #lang racket as the base lang
;; for documentation-time.

(require "doc-support.rkt"
         (submod "doc-support.rkt" private)
         (except-in ricoeur-doc-lang/doc-time
                    tag)
         ricoeur-doc-lang/prose-body
         scribble/decode
         (for-syntax "ir/struct.rkt"
                     "ir/syntax-class.rkt"
                     ))

(require-provide (for-syntax syntax/flatten-begin
                             racket/match
                             racket/sequence
                             ))

(provide (all-from-out ricoeur-doc-lang/doc-time)
         (all-from-out "doc-support.rkt")
         define-element
         define-elements-together
         )
     


(begin-for-syntax
  (define-splicing-syntax-class inset-clause
    #:description #f
    #:attributes {inset?-expr}
    (pattern (~describe "#:inset? clause"
                        (~seq #:inset? (~describe "inset expression"
                                                  inset?-expr:expr))))
    (pattern (~seq)
             #:with inset?-expr #'#f))
  (define-splicing-syntax-class element-declaration-start
    #:description #f
    #:attributes {name options}
    (pattern (~seq (~describe "element name"
                              name:id)
                   opts:element-contract-options)
             #:attr options (attribute opts.parsed))))

(define-syntax-parser define-elements-together
  [(_ inset:inset-clause
      ([decl:element-declaration-start
        (~describe "element struct definition form"
                   struct-clause)
        ...]
       ...+)
      prose-body:expr ...)
   #:with (name ...) #'(decl.name ...)
   #:with (opts ...) (map element-options->stx
                          (attribute decl.options))
   #`(define-elements-together/private
       #,this-syntax
       #:inset? inset.inset?-expr
       ([name opts struct-clause ...]
        ...)
       prose-body ...)])


(define-syntax-parser define-element
  [(_ (~describe "element name"
                 name:id)
      inset:inset-clause
      opts:element-contract-options
      (~or*
       (~seq (~describe "element struct definition form"
                        struct-clause)
             ...
             #:prose [prose-body:expr ...]) 
       (~seq #:prose [prose-body:expr ...]
             (~describe "element struct definition form"
                        struct-clause)
             ...)))
   #`(define-elements-together/private
       #,this-syntax
       #:inset? inset.inset?-expr
       ([name
         #,(element-options->stx (attribute opts.parsed))
         struct-clause ...])
       prose-body ...)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax element-info->rows-stx
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
                                  _ name-stx c)
                                 #`(cons '#,name-stx
                                         (racketblock0 #,c))])
                              attr-contracts))
              #'#f)
        #:required-attrs #,(if required-attrs
                               #`'(#,@required-attrs)
                               #'#f)
        #:text? #,(if text? #'#t #'#f))]))


(begin-for-syntax
  (define-syntax-class element-definition-prose
    #:description "element definition prose body expression"
    #:attributes {c}
    (pattern raw
             #:declare raw (expr/c #'pre-flow?
                                   #:name "element definition prose body expression")
             #:with c (parameterize ([currently-expanding-prose-body? #t])
                        (local-expand #'raw.c 'expression null)))))
             

(define-syntax-parser define-elements-together/private
  #:context (syntax-parse this-syntax
              [(_ orig-datum _ ...)
               #'orig-datum])
  [(_ orig-datum
      inset:inset-clause
      (elem:normalized-element-definition ...+)
      prose-body:element-definition-prose ...)
   #`(begin-for-runtime/derived
       orig-datum
       #:expr (make-defelement-flow
               #:inset? inset.inset?-expr
               #:elements (list #,@(map element-info->rows-stx
                                        (attribute elem.parsed)))
               prose-body.c ...)
       (define-element/runtime orig-datum elem) ...)])




