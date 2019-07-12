#lang racket/base

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/flatten-begin))

(provide begin-for-doc
         untangle
         (rename-out
          [child-module+ module+]
          [child-module* module*])
         (for-syntax currently-expanding-tangled?))

(define-syntax-parser child-module+
  ;; inspired by https://groups.google.com/d/msg/racket-users/H7vilh3KcD4/WoKcRUXe-PAJ
  ;; This will use the normal #%module-begin
  [(_ . stuff)
   #'(module+ . stuff)])

(define-syntax-parser child-module*
  [(_ . stuff)
   #'(module* . stuff)])

(define-syntax (#%begin-for-doc stx)
  (raise-syntax-error #f "internal error" stx))

(define-for-syntax currently-expanding-tangled?-param
  (make-parameter #f))
(define-for-syntax (currently-expanding-tangled?)
  (currently-expanding-tangled?-param))

(define-simple-macro (begin-for-doc form ...)
  #:fail-unless (currently-expanding-tangled?)
  "only allowed at untangle-time"
  (#%begin-for-doc form ...))


(define-for-syntax stop-list
  (list #'#%begin-for-doc
        #'begin ;; it's implicitly added, but let's be clear
        ;; Need to not try to expand these:
        #'#%require #'require
        #'#%provide #'provide
        #'define-values
        #'define-syntaxes
        #'module #'module* #'module+))

(define-simple-macro (untangle #:wrap done-form body ...)
  #:fail-unless (eq? 'module (syntax-local-context))
  "only allowed at module level"
  (#%untangle #:wrap done-form
              #:collected ()
              #:body (body ...)))

(define-syntax-parser #%untangle
  #:literals {begin #%begin-for-doc}
  #:track-literals
  [(_ #:wrap done-form
      #:collected (collected ...)
      #:body ())
   #'(done-form collected ...)]
  [(_ #:wrap done-form
      #:collected (collected ...)
      #:body (this to-go ...))
   #:with expanded
   (parameterize ([currently-expanding-tangled?-param #t])
     (local-expand #'this 'module stop-list))
   #'(#%untangle #:wrap done-form
                 #:collected (collected ...)
                 #:this expanded
                 #:body (to-go ...))]
  [(_ #:wrap done-form
      #:collected (collected ...)
      #:this (#%begin-for-doc form ...)
      #:body (to-go ...))
   #'(#%untangle #:wrap done-form
                 #:collected (collected ... form ...)
                 #:body (to-go ...))]
  [(_ #:wrap done-form
      #:collected (collected ...)
      #:this (~and this (begin raw ...))
      #:body (to-go ...))
   #:with (flattened ...) (flatten-all-begins #'this)
   #'(#%untangle #:wrap done-form
                 #:collected (collected ...)
                 #:body (flattened ... to-go ...))]
  [(_ #:wrap done-form
      #:collected (collected ...)
      #:this other
      #:body (to-go ...))
   #'(begin other
            (#%untangle #:wrap done-form
                        #:collected (collected ...)
                        #:body (to-go ...)))])
