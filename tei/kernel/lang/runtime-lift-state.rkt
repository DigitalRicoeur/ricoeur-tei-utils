#lang racket/base

(require racket/contract)

(provide make-runtime-lift-target
         runtime-lift-target?
         (contract-out
          [rename runtime-lift-target-list
                  runtime-lift-target->list
                  (-> runtime-lift-target?
                      (listof syntax?))]
          [current-runtime-lift-target
           (parameter/c runtime-lift-target?)]
          [lift-for-runtime!
           (-> (listof syntax?) any)]
          ))

(struct runtime-lift-target ([list #:mutable])
  #:transparent)

(define (make-runtime-lift-target)
  (runtime-lift-target null))

(define current-runtime-lift-target
  (make-parameter (make-runtime-lift-target)))

(define (lift-for-runtime! l-stx)
  (define target
    (current-runtime-lift-target))
  (set-runtime-lift-target-list! target
                                 (append (runtime-lift-target-list target)
                                         l-stx)))
