#lang racket/base

(require ricoeur/tei/kernel/sans-lang
         racket/contract
         racket/match
         racket/string
         racket/list
         syntax/parse/define
         "stxparam.rkt"
         (submod "stxparam.rkt" private)
         "adt.rkt"
         (submod "adt.rkt" private)
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/sequence
                     racket/list
                     racket/match
                     "ir/struct.rkt"
                     "ir/syntax-class.rkt"
                     "static-info.rkt"
                     ))

(provide define-element/runtime
         define-elements-specification-transformer
         elements-specification-transformer-part-id
         (for-syntax elements-specification-transformer-part
                     ))


(define-syntax-parser define-element/runtime
  #:context (syntax-parse this-syntax
              [(_ orig-datum _)
               #'orig-datum])
  [(_ orig-datum
      (~var elem (normalized-element-definition
                  #:contract? #t)))
   #:with wrapped-constructor-name
   (generate-temporary (format-symbol "make-~a-struct" #'elem.name))
   #:do [(define info
           (attribute elem.parsed))
         (define opts
           (element-info-options info))]
   #:with struct-def
   #`(define-element-struct/derived
       orig-datum
       [#:wrapped-constructor-name wrapped-constructor-name
        #:element-name elem.name
        #,(if (element-options-text? opts)
              #'#:contains-text
              #'#:elements-only)]
       elem.struct-clause ...)
   #:with (needed-name:id ...) (opts->needed-elements-stxes opts)
   #`(begin
       struct-def
       (elements-specification-transformer-part-id
        elem.name
        #:static-info-stx #,(make-static-element-info-stx info
                                                          #'wrapped-constructor-name)
        #:needed needed-name ...))])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(begin-for-syntax
  (define-syntax-class elements-specification-transformer-part
    #:attributes {name static-info-splice present-assoc [needed-assoc 1]}
    #:literals {elements-specification-transformer-part-id}
    (pattern
     (elements-specification-transformer-part-id
      name:id
      (~alt (~once (~seq #:static-info-stx static-info-stx:expr))
            (~once (~seq #:needed needed-name:id ...)))
      ...)
     #:attr static-info-splice (list #''name #'static-info-stx)
     #:with present-assoc #'(cons 'name (quote-syntax name))
     #:with (needed-assoc ...) #'((cons 'needed-name
                                        (quote-syntax needed-name))
                                  ...))))


(define-syntax-parser define-elements-specification-transformer
  [(_ name:id part:elements-specification-transformer-part ...)
   #:fail-when (check-duplicate-identifier
                (syntax->list #'(part.name ...)))
   "duplicate element name"
   #`(define-syntax name
       (make-elements-specification-transformer
        (list part.present-assoc ...)
        (list part.needed-assoc ... ...)
        (hasheq #,@(apply append
                          (attribute part.static-info-splice)))))])



(define-for-syntax (make-elements-specification-transformer present-assocs
                                                            raw-needed-assocs
                                                            hsh)
  (define present-syms
    (map car present-assocs))
  (specification-group-info
   hsh
   present-assocs
   (filter (λ (pr)
             (not (memq (car pr) present-syms)))
           (remove-duplicates raw-needed-assocs
                              eq?
                              #:key car))))


(define-syntax (elements-specification-transformer-part-id stx)
  (raise-syntax-error #f "not allowed in an expanded program" stx))

       
(define-for-syntax (opts->needed-elements-stxes opts)
  (define children?
    (element-options-children opts))
  (if children?
      (map child-spec-name-stx 
           children?)
      null))



(define-for-syntax (make-static-element-info-stx elem-info
                                                 wrapped-constructor-name)
  (match elem-info
    [(element-info
      _ name-stx 
      (element-options children required-order
                       attr-contracts required-attrs
                       extra-check/false text?))=
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
                             [(attr-contract-info _ name c)
                              #`(cons '#,name #,c)])
                           attr-contracts))
              #'#''())
        #,(if required-attrs
              #`#''(#,@required-attrs)
              #'#''())
        #,(if extra-check/false
              #`#'#,extra-check/false
              #'#'#f))]))


