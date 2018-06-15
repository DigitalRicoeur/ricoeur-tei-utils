#lang racket

;; This intentionally uses #lang racket as the base lang
;; for documentation-time.

(require "doc-support.rkt"
         (submod "doc-support.rkt" private)
         "begin-for-runtime.rkt"
         (submod "begin-for-runtime.rkt" private)
         (only-in scribble/manual/lang
                  [#%top-interaction scribble-top-interaction]
                  [#%module-begin scribble-module-begin])
         scribble/decode
         (for-syntax "ir/struct.rkt"
                     "ir/syntax-class.rkt"
                     ))

(require-provide (provide-only "doc-support.rkt"
                               "begin-for-runtime.rkt")
                 scribble/manual
                 syntax/parse/define
                 (for-syntax racket/base
                             syntax/parse
                             syntax/flatten-begin
                             racket/match
                             racket/sequence
                             ))

(provide (except-out (all-from-out racket)
                     #%top-interaction
                     #%module-begin)
         define-element
         define-elements-together
         (rename-out
          [module-begin #%module-begin]
          [scribble-top-interaction #%top-interaction]
          ))

(define-syntax-parser module-begin
  [(_ doc body:expr ...)
   ;; scribble-module-begin would expand things twice
   #:do [(define lift-ctx
           (gensym 'lifts))
         (define stop-list
           (list #'begin ;; it's implicitly added, but let's be clear
                 ;; Need to not try to expand these:
                 #'#%require
                 #'#%provide
                 #'define-values
                 #'define-syntaxes
                 #'module #'module* #'module+))]
   #:with (expanded-body:expr ...)
   (let loop ([to-go (syntax->list #'(body ...))]
              [lifted-so-far null]
              [expanded-body-so-far null])
     (match to-go
       ['()
        (append lifted-so-far expanded-body-so-far)]
       [(cons body-stx to-go)
        (syntax-parse (local-expand/capture-lifts body-stx
                                                  'module
                                                  stop-list
                                                  #f
                                                  lift-ctx)
          #:literals {begin}
          [(begin lifted:expr ... (begin nested:expr ...))
           (loop (append (flatten-all-begins
                          #'(begin nested ...))
                         to-go)
                 lifted-so-far
                 expanded-body-so-far)]
          [(begin lifted:expr ... expanded-body:expr)
           (loop to-go
                 (append lifted-so-far
                         (syntax->list #'(lifted ...)))
                 (append expanded-body-so-far
                         (list #'expanded-body)))])]))
   #'(scribble-module-begin doc expanded-body ...)])
      

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




