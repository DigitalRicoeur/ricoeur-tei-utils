#lang racket/base

(require racket/stxparam
         racket/splicing
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide begin-for-runtime
         define-element
         define-elements-together
         begin-for-test
         (for-syntax local-element-name
                     ))

(module+ private
  (provide plain-element-definition
           syntax-parameterize-local-element-name
           splicing-syntax-parameterize-local-element-name
           (for-syntax nested-begin-for-runtime-transformer
                       )))

(define-syntax-parameter begin-for-runtime 
  (syntax-parser
    [(_ body:expr ...)
     #'(void)]))

(define-for-syntax nested-begin-for-runtime-transformer
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

(define-syntax begin-for-test
  (syntax-parser
    [(_ body:expr ...)
     #'(begin-for-runtime
         (module+ test
           body ...))]))

#|(begin-for-syntax
  (struct local-element-name-id-box (id-stx)
    #:transparent
    #:property prop:procedure
    (λ (this stx)|#
      

(define-syntax-parameter local-element-name-stxparam
  #f)

(define-syntaxes {syntax-parameterize-local-element-name
                  splicing-syntax-parameterize-local-element-name}
  (let ([make (λ (stx-parameterize-id)
                (syntax-parser
                  [(_ name:id body:expr ...+)
                   #:with stx-parameterize stx-parameterize-id
                   #`(stx-parameterize 
                      ([local-element-name-stxparam #'name])
                      body ...)]))])
    (values (make #'syntax-parameterize)
            (make #'splicing-syntax-parameterize))))

(define-for-syntax (local-element-name)
  (syntax-parameter-value #'local-element-name-stxparam))



