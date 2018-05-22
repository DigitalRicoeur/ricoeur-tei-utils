#lang racket/base

;; An attempt to work around
;; https://github.com/racket/racket/issues/2093

(require racket/contract
         racket/match
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide substruct-of/c)

(define (blame-add-field-context blame field)
  (blame-add-context blame (format "the ~a field of" field)
                     #:swap? #t))

(define-syntax-parser make:blame->val+neg-party->guard-proc
  [(_ [field-name:id contract-expr:expr] ...)
   #:with (field-late-neg ...)
   (generate-temporaries (syntax->list #'(field-name ...)))
   #:with (field-proj+blame ...)
   (generate-temporaries (syntax->list #'(field-name ...)))
   #`(let ([field-late-neg
            (get/build-late-neg-projection contract-expr)]
           ...)
       (λ (blame)
         (let ([field-proj+blame
                (field-late-neg (blame-add-field-context blame
                                                         'field-name))]
               ...)
           (λ (val neg-party)
             (λ (field-name ... inst-name)
               (values (field-proj+blame field-name neg-party)
                       ...))))))])

(struct struct-type-contract (name blame->val+neg-party->guard-proc)
  #:transparent
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name (λ (this) (struct-type-contract-name this))
   #:first-order (λ (this) struct-type?)
   #:late-neg-projection
   (match-lambda
     [(struct-type-contract _ blame->val+neg-party->guard-proc)
      (λ (blame)
        (define val+neg-party->guard-proc
          (blame->val+neg-party->guard-proc blame))
        (λ (val neg-party)
          (unless (struct-type? val)
            (raise-blame-error
             blame #:missing-party neg-party
             val
             '(expected: "struct-type?" given: "~e")
             val))
          (chaperone-struct-type
           val
           values ;struct-info-proc (should there be filtering?)
           values ;make-constructor-proc (or error?)
           (val+neg-party->guard-proc val neg-party))))])))


(define-syntax (substruct-of/c stx)
  (define-syntax-class field-spec
    #:description "field specification"
    #:attributes {field-name contract}
    (pattern [(~describe "field name" field-name:id)
              contract-expr]
             #:declare contract-expr
             (expr/c #'chaperone-contract?
                     #:name (format "contract for field ~a"
                                    (syntax->datum #'field-name)))
             #:with contract #'(coerce-chaperone-contract
                                'substruct-of/c
                                contract-expr.c)))
  (syntax-parse stx
    [(_ (~describe "name" name:id) f:field-spec ...)
     #`(struct-type-contract
        '(substruct-of/c name)
        (make:blame->val+neg-party->guard-proc
         [f.field-name f.contract] ...))]))



   
