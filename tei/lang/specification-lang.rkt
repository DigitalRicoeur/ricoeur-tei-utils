#lang racket

(require (only-in scribble/manual/lang
                  [#%top-interaction scribble-top-interaction]
                  [#%module-begin scribble-module-begin])
         syntax/parse/define
         racket/stxparam
         racket/splicing
         xml/xexpr
         (for-syntax racket/base
                     syntax/parse
                     syntax/strip-context
                     syntax/flatten-begin
                     racket/match
                     adjutor
                     ))

(provide (except-out (all-from-out racket)
                     tag
                     provide
                     #%top-interaction
                     #%module-begin)
         begin-for-spec
         define-element
         define-elements-together
         (rename-out
          [module-begin #%module-begin]
          [scribble-top-interaction #%top-interaction]
          ))

(module+ for-doc
  (require scribble/manual
           "manual-helpers.rkt"
           (except-in racket
                      link
                      tag
                      ))
  (provide (except-out (all-from-out racket)
                       #%top-interaction
                       #%module-begin)
           (all-from-out scribble/manual)
           begin-for-spec
           define-element
           define-elements-together
           tag
           attr
           (rename-out
            [for-doc-module-begin #%module-begin]
            [scribble-top-interaction #%top-interaction]
            ))
  #|END module+ for-doc|#)


(define-syntax-parser module-begin
  [(_ body ...)
   #:with name (datum->syntax this-syntax 'spec)
   #`(#%module-begin
      (module 
          #,@(map strip-context
                  (syntax->list
                   #'(doc
                      (submod ricoeur/tei/lang/specification-lang
                              for-doc)
                      doc
                      body ...))))
      (splicing-syntax-parameterize
          ([define-element plain-d-element]
           [define-elements-together
             plain-d-elements-together])
        (stratify-body name
                       ()
                       ()
                       (body ...))))])

(define-syntax-parameter begin-for-spec ; begin-for-runtime ?
  (syntax-parser
    [(_ body:expr ...)
     #'(void)]))

(define-for-syntax nested-begin-for-spec-transformer
  (syntax-parser
    [(_ body:expr ...)
     #'(begin body ...)]))

(define-for-syntax (element-definition-syntax-error stx)
  (raise-syntax-error
   #f "element definition form used out of context" stx))

(define-syntax-parameter define-element
  element-definition-syntax-error)

(define-syntax-parameter define-elements-together
  element-definition-syntax-error)

(define-syntax-parameter plain-element-definition
  element-definition-syntax-error)












(begin-for-syntax
  (struct attr-contract-info (name-sym
                              name-stx
                              plain-contract-stx
                              protected-contract-stx)
    #:transparent)
  (struct extra-check (plain-stx protected-stx)
    #:transparent)
  (struct child-spec (repeat-datum repeat-stx name-sym name-stx)
    #:transparent)
  (define-syntax-class extra-check-spec
    #:description "extra check expression"
    #:attributes {parsed}
    (pattern #f
             #:attr parsed (extra-check this-syntax this-syntax))
    (pattern check
             #:declare check (expr/c #'(or/c #f
                                             (-> (and/c list? xexpr/c)
                                                 (or/c blame? #f)
                                                 any/c
                                                 any/c))
                                     #:name "extra check expression")
             #:attr parsed (extra-check this-syntax #'check.c)))
  ;; attributes
  (define-syntax-class single-attr-contract-spec
    #:description "attribute name / contract expression pair"
    #:attributes {parsed}
    (pattern [(~describe "attribute name" name:id) val]
             #:declare val
             (expr/c #'flat-contract?
                     #:name (format "~v attribute contract expression"
                                    (syntax->datum #'name)))
             #:attr parsed (attr-contract-info (syntax->datum #'name)
                                               #'name
                                               #'val
                                               #'val.c)))
  (define-syntax-class attr-contracts-spec
    #:description "a parenthesized sequence of attribute/contract pairs"
    #:attributes {parsed}
    (pattern (spec:single-attr-contract-spec ...)
             #:attr parsed (attribute spec.parsed)
             #:fail-when (check-duplicate-identifier
                          (map attr-contract-info-name-stx
                               (attribute spec.parsed)))
             "duplicate attribute contract spec"))
  (define-syntax-class required-attr-spec
    #:description "a parenthesized sequence of attribute names"
    #:attributes {lst}
    (pattern ((~describe "attribute name" name:id) ...)
             #:attr lst (syntax->list #'(name ...))
             #:fail-when (check-duplicate-identifier
                          (attribute lst))
             "duplicate required attribute"))
  ;; children
  (define-syntax-class single-child-spec
    #:description "repetition constraint / child element name pair"
    #:attributes {parsed}
    #:datum-literals {1+ 0-1 0+}
    (pattern [(~describe "repetition constraint"
                         (~and repeat (~or* 1 1+ 0-1 0+)))
              (~describe "child element name"
                         name:id)]
             #:attr parsed (child-spec (syntax->datum #'repeat)
                                       #'repeat
                                       (syntax->datum #'name)
                                       #'name)))
  (define-syntax-class children-spec
    #:description "a parenthesized sequence of repetition/name pairs"
    #:attributes {parsed}
    (pattern (child:single-child-spec ...)
             #:fail-when (check-duplicate-identifier
                          (map child-spec-name-stx
                               (attribute child.parsed)))
             "duplicate child element name"
             #:attr parsed (attribute child.parsed)))
  (define-syntax-class child-order-spec
    ;; TODO: should this check-duplicate-identifier ?
    #:description "a parenthesized sequence of child element names"
    #:attributes {lst}
    (pattern ((~describe "child element name" name:id) ...)
             #:attr lst (syntax->list #'(name ...))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (struct element-options (children
                           required-order
                           attr-contracts
                           required-attrs
                           extra-check
                           text?)
    #:transparent)
  (define-splicing-syntax-class define-element/options
    #:description #f
    #:attributes {parsed}
    (pattern
     (~seq (~alt (~optional (~seq (~and text-kw #:contains-text))
                            #:name "#:contains-text option")
                 (~optional (~seq #:extra-check check:extra-check-spec)
                            #:name "#:extra-check clause"
                            #:defaults ([check.parsed #f]))
                 ;; attributes
                 (~optional (~seq #:attr-contracts
                                  attr-contracts:attr-contracts-spec)
                            #:name "#:attr-contracts clause"
                            #:defaults ([attr-contracts.parsed #f]))
                 (~optional (~seq #:required-attrs
                                  required-attrs:required-attr-spec)
                            #:name "#:required-attrs clause"
                            #:defaults ([required-attrs.lst #f]))
                 ;; children
                 (~optional (~seq #:children children:children-spec)
                            #:name "#:children clause"
                            #:defaults ([children.parsed #f]))
                 (~optional (~seq #:required-order order:child-order-spec)
                            #:name "#:required-order clause"
                            #:defaults ([order.lst #f]))
                 )
           ...)
     #:do [(def
             [text? (any->boolean (attribute text-kw))]
             [extra-check/false (attribute check.parsed)]
             ;; attributes
             [l-attr-contract-info/false (attribute attr-contracts.parsed)]
             [l-required-attributes/false  (attribute required-attrs.lst)]
             ;; children
             [l-child-specs/false (attribute children.parsed)]
             [l-required-order/false (attribute order.lst)])]
     ;; check children
     #:fail-when (and l-required-order/false
                      (not l-child-specs/false)
                      #'order)
     "#:required-order clause not allowed without #:children clause"
     #:fail-when (and l-required-order/false
                      l-child-specs/false
                      (let ([children-syms
                             (map child-spec-name-sym
                                  l-child-specs/false)])
                        (for/first ([sym (in-list (map syntax->datum
                                                       l-required-order/false))]
                                    [stx (in-list l-required-order/false)]
                                    #:unless (memq sym children-syms))
                          stx)))
     "child element is missing from the #:children clause"
     ;; check attributes
     #:fail-when (and l-required-attributes/false
                      (not l-attr-contract-info/false)
                      #'required-attrs)
     "#:required-attrs clause not allowed without #:attr-contracts clause"
     #:fail-when (and l-attr-contract-info/false
                      l-required-attributes/false
                      (let ([contracted-attr-syms
                             (map attr-contract-info-name-sym
                                  l-attr-contract-info/false)])
                        (for/first ([sym (in-list (map syntax->datum
                                                       l-required-attributes/false))]
                                    [stx (in-list l-required-attributes/false)]
                                    #:unless (memq sym contracted-attr-syms))
                          stx)))
     "missing contract for required attribute"
     #:attr parsed
     (element-options l-child-specs/false
                      l-required-order/false
                      l-attr-contract-info/false
                      l-required-attributes/false
                      extra-check/false
                      text?)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (struct element-info (name-sym name-stx options)
    ;; TODO: datatype definition
    #:transparent)
  (struct element-definition-group (elements
                                    inset?-expr
                                    body)
    ;; TODO: support local definitions
    #:transparent)
  (define-splicing-syntax-class inset-clause
    #:description #f
    #:attributes {inset?-expr}
    (pattern (~describe "#:inset? clause"
                        (~seq #:inset? (~describe "inset expression"
                                                  inset?-expr:expr))))
    (pattern (~seq)
             #:with inset?-expr #'#f))
  (define-splicing-syntax-class define-element/rest
    #:description #f
    #:attributes {parsed name opts [body 1]}
    (pattern (~seq (~describe "element name"
                              name:id)
                   inset:inset-clause
                   opts:define-element/options
                   (~or*
                    (~seq body:expr ...) ;; datatype before body
                    (~seq #:prose [body:expr ...]))) ;; datatype after body
             #:attr parsed
             (element-definition-group
              (list (element-info (syntax->datum #'name)
                                  #'name
                                  (attribute opts.parsed)))
              #'inset.inset?-expr
              (syntax->list #'(body ...)))))
  (define-splicing-syntax-class element-declaration-start
    #:description #f
    #:attributes {name opts options}
    (pattern (~seq (~describe "element name"
                              name:id)
                   opts:define-element/options)
             #:attr options (attribute opts.parsed)))
  (define-splicing-syntax-class define-elements-together/rest
    #:description #f
    #:attributes {parsed [name 1] [opts 1] [body 1]}
    (pattern (~seq inset:inset-clause
                   (~or* (~seq ([decl:element-declaration-start]
                                ;; datatype after decl
                                ...+)
                               body:expr ...)
                         (~seq ([decl:element-declaration-start]
                                ...+)
                               #:prose [body:expr ...]))) ;; datatypes after body
             #:with (name ...) #'(decl.name ...)
             #:with (opts ...) #'(decl.opts ...)
             #:attr parsed
             (element-definition-group
              (for/list ([name-stx (in-list (syntax->list
                                             #'(decl.name ...)))]
                         [opts (in-list (attribute decl.options))])
                (element-info (syntax->datum name-stx)
                              name-stx
                              opts))
              #'inset.inset?-expr
              (syntax->list #'(body ...)))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-syntax-class explicit-element-info
    #:datum-literals {1+ 0-1 0+}
    #:attributes {parsed}
    (pattern
     [name:id
      [#:children
       (~optional (~and children-present
                        ([(~and repeat
                                (~or* 1 1+ 0-1 0+))
                          child-name:id]
                         ...)))]
      [#:required-order
       (~optional (~and required-order-present
                        (order-name:id ...)))]
      [#:attr-contracts
       (~optional (~and attr-contracts-present
                        ([attr-name:id
                          plain-contract:expr
                          protected-contract:expr]
                         ...)))]
      [#:required-attrs
       (~optional (~and required-attrs-present
                        (required-attr-name:id ...)))]
      [#:extra-check
       (~optional (~and extra-check-present
                        [plain-check:expr
                         protected-check:expr]))]
      (~or* (~and text? #:contains-text)
            #:no-text)]
     #:attr parsed
     (element-info
      (syntax->datum #'name)
      #'name
      (element-options
       (and (attribute children-present)
            (for/list ([r (in-list (syntax->list
                                    #'(repeat ...)))]
                       [n (in-list (syntax->list
                                    #'(child-name ...)))])
              (child-spec (syntax->datum r)
                          r
                          (syntax->datum n)
                          n)))
       (and (attribute required-order-present)
            (syntax->list #'(order-name ...)))
       (and (attribute attr-contracts-present)
            (for/list ([n (in-list (syntax->list
                                    #'(attr-name ...)))]
                       [plain (in-list (syntax->list
                                        #'(plain-contract ...)))]
                       [protected (in-list
                                   (syntax->list
                                    #'(protected-contract ...)))])
              (attr-contract-info (syntax->datum n)
                                  n
                                  plain
                                  protected)))
       (and (attribute required-attrs-present)
            (syntax->list #'(required-attr-name ...)))
       (and (attribute extra-check-present)
            (extra-check #'plain-check #'protected-check))
       (any->boolean (attribute text?))))))
  (define element-info->plain-element-definition
    (match-lambda
      [(element-info
        _ name
        (element-options children
                         required-order
                         attr-contracts
                         required-attrs
                         extra-check/false
                         text?))
       #`(plain-element-definition
          [#,name
           #,(if children
                 #`[#:children
                    (#,@(map (match-lambda
                               [(child-spec _ repeat _ n)
                                #`[#,repeat #,n]])
                             children))]
                 #`[#:children])
           #,(if required-order
                 #`[#:required-order (#,@required-order)]
                 #`[#:required-order])
           #,(if attr-contracts
                 #`[#:attr-contracts
                    (#,@(map (match-lambda
                               [(attr-contract-info
                                 _ n plain protected)
                                #`[#,n #,plain #,protected]])
                             attr-contracts))]
                 #`[#:attr-contracts])
           #,(if required-attrs
                 #`[#:required-attrs (#,@required-attrs)]
                  #`[#:required-attrs])
           #,(match extra-check/false
               [(extra-check plain protected)
                #`[#:extra-check [#,plain #,protected]]]
               [#f
                #`[#:extra-check]])
           #,(if text?
                 #'#:contains-text
                 #'#:no-text)])]))
  (define plain-d-element
    (syntax-parser
      [(_ it:define-element/rest)
       #`(begin #,(element-info->plain-element-definition
                   (car
                    (element-definition-group-elements
                     (attribute it.parsed))))
                it.body ...)]))
  (define plain-d-elements-together
    (syntax-parser
      [(_ it:define-elements-together/rest)
       #`(begin #,@(map element-info->plain-element-definition
                        (element-definition-group-elements
                         (attribute it.parsed)))
                it.body ...)]))
  (define-syntax-class plain-element-definition-stx
    #:description "plain-element-definition"
    #:attributes {parsed}
    #:literals {plain-element-definition}
    (pattern (plain-element-definition
              info:explicit-element-info)
             #:attr parsed
             (attribute info.parsed)))
  #|END begin-for-syntax|#)











  

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
                               (list #'begin-for-spec
                                     #'plain-element-definition
                                     ;; need to not try to expand these
                                     #'#%require
                                     #'#%provide
                                     ))
     #:literals {begin begin-for-spec}
     [(begin body:expr ...)
      #:with (flattened ...) (flatten-all-begins
                              #'(begin body ...))
      #`(stratify-body name
                       (for-spec-body ...)
                       (spec-form ...)
                       (flattened ... to-go ...))]
     [(begin-for-spec body:expr ...)
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
         ([begin-for-spec nested-begin-for-spec-transformer])
       for-spec-body ...
       (prepare-spec name spec-form ...))])


(define-syntax-parser prepare-spec
  [(_ name:id spec-form:plain-element-definition-stx ...)
   #`(begin (define name
              '(spec-form ...))
            (provide name))])













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
       #`(make-define-element-flow
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






