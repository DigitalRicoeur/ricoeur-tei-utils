#lang racket/base

(require syntax/parse
         racket/contract
         racket/match
         adjutor
         "struct.rkt"
         (for-template racket/base
                       racket/contract
                       ricoeur/tei/kernel/xexpr/plain-contracts
                       ))

(provide element-contract-options
         element-contract-options/term
         element-options->stx
         normalized-element-definition
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extra check

(define-syntax-class (extra-check-spec #:contract? [contract? #f])
  #:description "extra check expression"
  #:attributes {parsed}
  (pattern #f
           #:with parsed this-syntax)
  (pattern check
           #:declare check (expr/c #'(or/c #f
                                           (-> raw-xexpr-element/c
                                               (or/c blame? #f)
                                               any/c
                                               any/c))
                                   #:name "extra check expression")
           #:with parsed (if contract?
                             (syntax-local-lift-expression #'check.c)
                             #'check)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; attributes

(define-syntax-class (single-attr-contract-spec #:contract? [contract? #f])
  #:description "attribute name / contract expression pair"
  #:attributes {parsed}
  #:literals {string? any/c}
  (pattern [(~describe "attribute name" name:id)
            (~or* val:str
                  (~and val (~or* string? any/c)))]
           #:attr parsed
           (attr-contract-info (syntax->datum #'name)
                               #'name
                               #'val))
  (pattern [(~describe "attribute name" name:id) val]
           #:declare val
           (expr/c #'flat-contract?
                   #:name (format "~v attribute contract expression"
                                  (syntax->datum #'name)))
           #:attr parsed
           (attr-contract-info (syntax->datum #'name)
                               #'name
                               (if contract?
                                   (syntax-local-lift-expression #'val.c)
                                   #'val))))


(define-syntax-class (attr-contracts-spec #:contract? [contract? #f])
  #:description "a parenthesized sequence of attribute/contract pairs"
  #:attributes {parsed}
  (pattern ((~var spec (single-attr-contract-spec #:contract? contract?))
            ...)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  #:description "a parenthesized sequence of child element names"
  #:attributes {lst}
  (pattern ((~describe "child element name" name:id) ...)
           #:attr lst (syntax->list #'(name ...))
           #:fail-when (check-duplicate-identifier
                        (attribute lst))
           "duplicate child element name"))


;                                                          
;                                                          
;                                                          
;                                                          
;                    ;;        ;                           
;                    ;;        ;;                          
;    ;;;    ; ;;   ;;;;;;;  ;;;;;    ;;;    ;; ;      ;;   
;   ;   ;   ;;  ;    ;;        ;;   ;   ;   ;;; ;   ;;  ;  
;   ;   ;   ;;  ;    ;;        ;;   ;   ;   ;;  ;;   ;     
;  ;;   ;;  ;;  ;;   ;;        ;;  ;;   ;;  ;;  ;;    ;;   
;   ;   ;   ;;  ;    ;;        ;;   ;   ;   ;;  ;;      ;; 
;   ;   ;   ;;  ;     ;        ;;   ;   ;   ;;  ;;  ;   ;  
;    ;;;    ;;;;       ;;;     ;;    ;;;    ;;  ;;   ;;;   
;           ;;                                             
;           ;;                                             
;           ;;                                             
;                                                          

(define-splicing-syntax-class (element-contract-options #:contract? [contract? #f])
  #:description #f
  #:attributes {parsed}
  (pattern
   (~seq (~alt (~optional (~seq (~and text-kw #:contains-text))
                          #:name "#:contains-text option")
               (~optional (~seq #:extra-check
                                (~var check (extra-check-spec
                                             #:contract? contract?)))
                          #:name "#:extra-check clause"
                          #:defaults ([check.parsed #f]))
               ;; attributes
               (~optional (~seq #:attr-contracts
                                (~var attr-contracts (attr-contracts-spec
                                                      #:contract? contract?)))
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



(define-splicing-syntax-class (element-contract-options/term #:contract? [contract? #f])
  #:description #f
  #:attributes {parsed}
  (pattern [(~var opts (element-contract-options #:contract? contract?))]
           #:attr parsed (attribute opts.parsed)))


(define element-options->stx
  (match-lambda
    [(element-options children
                      required-order
                      attr-contracts
                      required-attrs
                      extra-check/false
                      text?)
     #`[#,@(list-when children
             (list #'#:children
                   (map (match-lambda
                          [(child-spec _ repeat _ n)
                           #`[#,repeat #,n]])
                        children)))
        #,@(list-when required-order
             (list #'#:required-order
                   #`(#,@required-order)))
        #,@(list-when attr-contracts
             (list #'#:attr-contracts
                   #`(#,@(map (match-lambda
                                [(attr-contract-info
                                  _ n c)
                                 #`[#,n #,c]])
                              attr-contracts))))
        #,@(list-when required-attrs
             (list #'#:required-attrs
                   #`(#,@required-attrs)))
        #,@(list-when extra-check/false
             (list #'#:extra-check extra-check/false))
        #,@(list-when text?
             (list #'#:contains-text))]]))


;                                  
;                                  
;                                  
;                                  
;      ;                ;;;        
;      ;;             ;;           
;   ;;;;;   ;; ;    ;;;;;;;  ;;;   
;      ;;   ;;; ;     ;;    ;   ;  
;      ;;   ;;  ;;    ;;    ;   ;  
;      ;;   ;;  ;;    ;;   ;;   ;; 
;      ;;   ;;  ;;    ;;    ;   ;  
;      ;;   ;;  ;;    ;;    ;   ;  
;      ;;   ;;  ;;    ;;     ;;;   
;                                  
;                                  
;                                  
;                                  


(define-syntax-class (normalized-element-definition #:contract? [contract? #f])
  #:description "normalized element definition"
  #:attributes {name parsed [struct-clause 1]}
  (pattern [name:id
            (~var opts (element-contract-options/term
                        #:contract? contract?))
            struct-clause ...]
           #:attr parsed (element-info (syntax->datum #'name)
                                       #'name
                                       (attribute opts.parsed))))






