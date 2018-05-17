#lang racket/base

(require racket/stxparam
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide begin-for-runtime
         define-element
         define-elements-together
         begin-for-test
         extend-specifications
         )

(module+ private
  (provide plain-element-definition
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

(define-syntax-parameter extend-specifications
  (syntax-parser
    [(_ e:id ...)
     #'(void)]))

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

