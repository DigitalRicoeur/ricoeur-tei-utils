#lang racket/base

(require "kernel/topic-model.rkt"
         "preprocessor/interactive.rkt"
         syntax/parse/define
         (for-syntax racket/base))

(provide (all-from-out "kernel/topic-model.rkt")
         (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [preprocessor-modbegin
                      #%module-begin]))

(module* reader syntax/module-reader
  ;; FIXME: this doesn't work: #:language '(submod ".."))
  ricoeur/secondary-lit/preprocessor)

(define-syntax-parser preprocessor-modbegin
  #:literals {topic-model}
  [(_ (~describe "topic-model expression"
                 (~and model-expr (topic-model _ ...))))
   #:with model (datum->syntax #'model-expr 'model)
   #'(#%plain-module-begin
      (provide model)
      (define model model-expr)
      (module* main #f
        (#%plain-module-begin
         (start-interactive-preprocessor
          model))))]
  [(_ (~optional form0) forms ...)
   (raise-syntax-error
    #f "bad syntax;\n expected a single topic-model expression"
    this-syntax
    (attribute form0))])
