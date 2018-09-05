#lang racket/base

(require scribble/reader
         (only-in scribble/base/reader
                  ;; not documented, but used by #lang scribble/lp2
                  scribble-base-info)
         (only-in syntax/module-reader
                  [#%module-begin smr-module-begin])
         racket/runtime-path
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin]))

(module reader syntax/module-reader
  ricoeur/tei/kernel/lang/core/module-reader)

(define-syntax-parser module-begin
  [(_ (~optional (~describe #:opaque "language module path"
                            language:expr))
      (~alt (~optional (~seq #:info
                             (~describe #:opaque "info function expression"
                                        custom-info-proc:expr))
                       #:name "#:info clause")
            (~optional (~seq #:language
                             (~describe #:opaque "language expression"
                                        lang-expr:expr))
                       #:name "#:language clause"))
      ...
      form:expr ...)
   #:fail-unless (or (attribute language)
                     (attribute lang-expr))
   "must specify either a module language, or #:language"
   #:fail-when (and (attribute language)
                    (attribute lang-expr))
   "must specify either a module language, or #:language, not both"
   #`(smr-module-begin
      (~? language
          (~@ #:language lang-expr))
      #:whole-body-readers? #t
      #:read read*
      #:read-syntax read-syntax*
      #:info
      (~? (λ (key default-val default-filter-proc)
            (custom-info-proc key
                              default-val
                              (λ (key default-val)
                                (my-info-proc key
                                              default-val
                                              default-filter-proc))))
          my-info-proc)
      form ...)])


(define read-syntax*
  (make-at-reader #:syntax? #t
                  #:inside? #t
                  #:command-char #\ƒ))

(define (read* in)
  (syntax->datum 
   (read-syntax* #f in)))

(define-runtime-module-path color-lexer-module
  "color-lexer.rkt")

(define (my-info-proc key default-val default-filter-proc)
  (define (default)
    ((scribble-base-info) key default-val default-filter-proc))
  (define (try-dynamic-require lib export)
    (dynamic-require lib export default))
  (case key
    [(color-lexer)
     (try-dynamic-require color-lexer-module 'color-lexer)]
    [(drracket:default-extension) "rkt"]
    [else
     (default)]))

