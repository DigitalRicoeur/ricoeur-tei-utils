#lang racket/base

(require syntax/parse
         syntax/strip-context
         (for-template racket/base))

(provide lift-for-runtime!
         lifted-for-runtime)

(define lift-for-runtime-sym
  '|the top secret runtime lift symbol|)

(define lift-for-runtime-id
  (datum->syntax #f lift-for-runtime-sym))

(define (lift-for-runtime! body-lst)
  (syntax-local-lift-expression
   #`(quote-syntax (#,lift-for-runtime-id
                    ;; Using strip-context here prevents check-syntax
                    ;; from drawing extra, spurious binding arrows.
                    ;; I think strip-context isn't quite right:
                    ;; we want to keep internal scope differences, 
                    ;; just remove the other module's scope.
                    ;; But we already do it on the other side in
                    ;; make-module-begin.
                    #,@(map strip-context body-lst)))))

(define-syntax-class lifted-for-runtime
  #:description "lifted runtime forms"
  #:attributes {[body 1]}
  #:literals {define-values quote-syntax}
  (pattern (define-values (_) (quote-syntax (key:id body:expr ...)))
           #:fail-unless (eq? (syntax-e #'key) lift-for-runtime-sym)
           "lifted definition uses wrong identifier"))
