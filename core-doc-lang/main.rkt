#lang racket/base

(require "make-module-begin.rkt"
         (for-syntax racket/base))

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         module+
         module*
         (rename-out
          [module-begin #%module-begin]
          ))

(module reader "module-reader.rkt"
   ricoeur-doc-lang)

(define-syntax module-begin
  (make-module-begin))
