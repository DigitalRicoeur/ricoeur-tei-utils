#lang racket/base

(require syntax/parse/define
         racket/contract
         racket/match
         ricoeur/tei/kernel
         (submod ricoeur/tei/kernel/xexpr/contract-utils
                 private)
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/match
                     racket/list
                     "static-info.rkt"
                     ))

(provide define-values/elements-specifications
         define-combined-elements-specification)

(module+ private
  (provide show-elements-specification-transformer))


(begin-for-syntax
  (struct elem-name+ (sym stx info-val transf-stx)
    #:transparent)

  (define (assocs-to-name+-list accessor l-vals l-stx)
    (flatten
     (for/list ([info-val (in-list l-vals)]
                [transf-stx (in-list l-stx)])
       (map (match-lambda
              [(cons sym stx)
               (elem-name+ sym stx info-val transf-stx)])
            (accessor info-val)))))

  (define (check-no-missing/duplicate-elems this-syntax
                                            l-vals
                                            l-stx
                                            #:missing-ok? [missing-ok? #f])
    (define all-present-name+
      (assocs-to-name+-list 
       specification-group-info-elements-present-assocs
       l-vals
       l-stx))
    (for ([grp (in-list (group-by elem-name+-sym
                                  all-present-name+
                                  eq?))]
          #:unless (= 1 (length grp)))
      (match-define (list-rest
                     (elem-name+ sym _ _ tranf-stx-a)
                     (elem-name+ _ _ _  tranf-stx-b)
                     _)
        grp)
      (raise-syntax-error
       #f
       (format "duplicate definitions for element ~v"
               sym)
       this-syntax
       tranf-stx-b
       (remove tranf-stx-b
               (map elem-name+-transf-stx grp))))
    (unless missing-ok?
      (define all-present-syms
        (map elem-name+-sym all-present-name+))
      (define all-needed-name+
        (assocs-to-name+-list
         specification-group-info-elements-needed-assocs
         l-vals
         l-stx))
      (for ([needed (in-list all-needed-name+)]
            #:unless (memq (elem-name+-sym needed)
                           all-present-syms))
        (match-define (elem-name+ sym stx info-val transf-stx)
          needed)
        (raise-syntax-error
         #f
         (format "no definition for required element ~v"
                 sym)
         this-syntax
         transf-stx))))
  #|END begin-for-syntax|#)





(define-syntax-parser define-values/elements-specifications
  [(_ [spec:elements-specification-transformer
       ...]
      (~alt (~once (~seq #:tei-xexpr/c tei-xexpr/c:id)
                   #:name "#:tei-xexpr/c clause")
            (~once (~seq #:dynamic-tei-xexpr/c
                         dynamic-tei-xexpr/c:id)
                   #:name "#:dynamic-tei-xexpr/c clause")
            (~once (~seq #:any-tei-xexpr/c any-tei-xexpr/c:id)
                   #:name "#:any-tei-xexpr/c clause")
            (~once (~seq #:xexpr->tei-element xexpr->tei-element:id
                         (~optional (~and use-contract? #:define/contract)))
                   #:name "#:xexpr->tei-element clause")
            (~once (~seq #:tei-element-name/c tei-element-name/c:id)
                   #:name "#:tei-element-name/c clause"))
      ...)
   #:do [(check-no-missing/duplicate-elems
          this-syntax
          (attribute spec.parsed)
          (syntax->list #'(spec ...)))
         (define l-info
           (flatten (map (compose1 hash-values
                                   specification-group-info-elements)
                         (attribute spec.parsed))))
         (define l-element-names
           (map element-static-info-name-stx l-info))]
   #:with (element-name ...) l-element-names
   #:with (contract-name ...) (generate-temporaries
                               l-element-names)
   #:with (contract-expr ...)
   (map (->element-static-info->contract-expr
         #'make-element-contract)
        l-info)
   #:with (wrapped-constructor-name ...)
   (map element-static-info-wrapped-constructor-name
        l-info)
   (with-disappeared-uses
    (record-disappeared-uses
     (syntax->list #'(spec ...)))
    #`(begin
        (define/final-prop tei-element-name/c
          (or/c 'element-name ...))
        
        (define-make-element-contract make-element-contract
          #:tei-xexpr/c dynamic-tei-xexpr/c
          #:any-tei-xexpr/c any-tei-xexpr/c)

        (define contract-name contract-expr) ...
        
        (define-dynamic-tei-xexpr/c dynamic-tei-xexpr/c 
          [element-name ...]
          [contract-name ...])

        (define-static-tei-xexpr/c tei-xexpr/c 
          [element-name ...]
          [contract-name ...])
          
        (define/final-prop any-tei-xexpr/c
          (make-any-tei-xexpr/c
           #:tei-element-name/c tei-element-name/c
           #:tei-xexpr/c dynamic-tei-xexpr/c))

        (define-xexpr->element xexpr->tei-element
          #:contract #,(if (attribute use-contract?)
                           #'any-tei-xexpr/c
                           #'#f)
          [element-name ...]
          [wrapped-constructor-name ...])
        
        #|END define-values/elements-specifications|#))])

(define-syntax-parser define-static-tei-xexpr/c
  [(_ static-tei-xexpr/c:id
      [element-name:id ...]
      [contract-name:id ...])
   #`(define-syntax-parser static-tei-xexpr/c
       [(_ (~datum element-name))
        #'contract-name]
       ...)])
  
(define-syntax-parser define-dynamic-tei-xexpr/c
  [(_ tei-xexpr/c:id
      [element-name:id ...]
      [contract-name:id ...])
   #'(define/final-prop (tei-xexpr/c sym)
       (case sym
         [(element-name) contract-name]
         ...
         [else
          (raise-argument-error
           'tei-xexpr/c
           "tei-element-name/c"
           sym)]))])

(define-for-syntax (->element-static-info->contract-expr
                    make-element-contract-stx)
  (match-lambda
    [(element-static-info _
                          name-stx
                          wrapped-constructor-name
                          text?
                          children-stx
                          required-order-stx
                          attr-contracts-stx
                          required-attrs-stx
                          extra-check-stx)
     #`(#,make-element-contract-stx
        '#,name-stx
        #:children #,children-stx
        #:text? #,(if text? #'#t #'#f)
        #:required-order #,required-order-stx
        #:extra-check #,extra-check-stx
        #:attr-contracts #,attr-contracts-stx
        #:required-attrs #,required-attrs-stx)]))


(define-syntax-parser define-xexpr->element
  [(_ xexpr->element:id #:contract any-tei-xexpr/c:id
      [element-name:id ...]
      [wrapped-constructor-name:id ...])
   #`(define/contract xexpr->element
       (-> any-tei-xexpr/c tei-element?)
       (make-xexpr->element
        xexpr->element
        [element-name ...]
        [wrapped-constructor-name ...]))]
  [(_ xexpr->element:id #:contract #f
      [element-name:id ...]
      [wrapped-constructor-name:id ...])
   #`(define xexpr->element
       (make-xexpr->element
        xexpr->element
        [element-name ...]
        [wrapped-constructor-name ...]))])

(define-syntax-parser make-xexpr->element
  [(_ xexpr->element:id
      [element-name:id ...]
      [wrapped-constructor-name:id ...])
   #`(λ (xs)
       #|(define/contract norm
         normalized-xexpr-element/c
         (normalize-xexpr xs))
       (check-immutable-strings norm)
       (let xexpr->element ([xs norm])|#
       (let xexpr->element ([xs (normalize-xexpr xs)])
         (match-define (list-rest name attrs body)
           xs)
         (define make
           (case name
             [(element-name) wrapped-constructor-name]
             ...
             [else
              (raise-argument-error
               'xexpr->element
               "any-tei-xexpr/c"
               xs)]))
         (make name
               attrs
               (for/list ([child (in-list body)])
                 (if (list? child)
                     (xexpr->element child)
                     child)))))])

#|(require racket/list)
(define (check-immutable-strings xs)
  (for/first ([v (in-list (flatten xs))]
              #:when (string? v)
              #:unless (immutable? v))
    (error 'check-immutable-strings
           "mutable string: ~e\n  in...:\n   ~e"
           v xs)))|#


(define-syntax-parser define-combined-elements-specification
  [(_ name:id [spec:elements-specification-transformer ...+])
   #:do [(check-no-missing/duplicate-elems
          #:missing-ok? #t
          this-syntax
          (attribute spec.parsed)
          (syntax->list #'(spec ...)))]
   (with-disappeared-uses
    (record-disappeared-uses
     (syntax->list #'(spec ...)))
    #`(define-syntax name
        #,(specification-group-info->stx
           (apply specification-group-info-union
                  (attribute spec.parsed)))))])




(define-syntax-parser show-elements-specification-transformer
  [(_ spec:elements-specification-transformer)
   #:do [(match-define
           (specification-group-info elems
                                     present-assocs
                                     needed-assocs)
           (attribute spec.parsed))]
   #:with (present-name ...) (map cdr present-assocs)
   #:with (needed-name ...) (map cdr needed-assocs)
   (with-disappeared-uses
    (record-disappeared-uses (list #'spec))
    #`(vector
       '(present-name ...)
       '(needed-name ...)
       (hasheq
        #,@(apply append
                  (for/list ([v (in-hash-values elems)])
                    (match v
                      [(element-static-info
                        _
                        name-stx
                        wrapped-constructor-name
                        text?
                        children-stx
                        required-order-stx
                        attr-contracts-stx
                        required-attrs-stx
                        extra-check-stx)
                       (list #`'#,name-stx
                             #`(vector
                                #,wrapped-constructor-name
                                #,(if text? #'#t #'#f)
                                #,children-stx
                                #,required-order-stx
                                #,attr-contracts-stx
                                #,required-attrs-stx
                                #,extra-check-stx))]))))))])







