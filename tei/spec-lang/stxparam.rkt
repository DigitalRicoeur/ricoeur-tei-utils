#lang racket/base

(require racket/stxparam
         racket/splicing
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))

(provide (for-syntax local-element-name))

(module+ private
  (provide syntax-parameterize-local-element-name))

(define-syntax-parameter local-element-name-stxparam
  #f)

(define-syntax-parser syntax-parameterize-local-element-name
  [(_ name:id
      (~optional (~and splicing? #:splicing))
      body:expr ...)
   #:with stx-parameterize (if (attribute splicing?)
                               #'splicing-syntax-parameterize
                               #'syntax-parameterize)
   #`(stx-parameterize 
      ([local-element-name-stxparam #'name])
      body ...)])

(define-for-syntax (local-element-name)
  (syntax-parameter-value #'local-element-name-stxparam))

