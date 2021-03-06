#lang racket/base

(require "untangle.rkt"
         syntax/parse/define
         (for-syntax racket/base))

(provide cross-phase doctime-string
         module+ module*
         (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [demo-module-begin #%module-begin]))

(module reader syntax/module-reader
  ricoeur/new-core-tangled-lang/demo-lang)

(define-simple-macro (demo-module-begin raw-form ...)
  #:do [(define introducer (make-syntax-introducer #t))]
  #:with (form ...) (introducer #'(raw-form ...))
  #:with racket/base (introducer
                      (datum->syntax this-syntax 'racket/base
                                     (vector (syntax-source this-syntax) 1 0 1 1)))
  (#%module-begin
   (... (define-syntax-parser wrap-untangled
          [(_ doc-form ...)
           #`(module* doc #,(syntax-local-introduce #'racket/base)
               doc-form ...)]))
   (untangle #:wrap wrap-untangled form ...)
   (module+ test
     (require (submod ".." doc)))))

(define-simple-macro (cross-phase [runtime ...] [doctime ...])
  (begin runtime ... (begin-for-doc doctime ...)))


(define-simple-macro (doctime-string s:str ...)
  (begin-for-doc (require racket/string)
                 (string-join '(s ...))))
