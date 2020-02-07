#lang racket/base

(require ricoeur/tei/kernel
         syntax/parse/define
         racket/require
         "elem-for-runtime.rkt"
         (only-in "link.rkt"
                  define-combined-elements-specification)
         ricoeur/core-doc-lang/make-module-begin
         (prefix-in racket: (only-in racket/base
                                     module+
                                     module*))
         (only-in adjutor/unstable TODO/void)
         (for-syntax racket/base
                     racket/list
                     racket/sequence
                     racket/bool
                     syntax/parse
                     syntax/flatten-begin
                     syntax/strip-context
                     "ir/struct.rkt"
                     "ir/syntax-class.rkt"
                     "static-info.rkt"))

(require-provide (provide-only ricoeur/tei/kernel)
                 racket/contract
                 racket/match
                 racket/string
                 racket/list
                 "stxparam.rkt"
                 "adt.rkt"
                 "interface-macros.rkt")

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         define-element/runtime ;; TODO: something better w/ syntax-local-require
         module+
         module*
         (rename-out
          [module-begin #%module-begin]
          ))

(TODO/void spec declaration: give good errors without "#:with-local")

(begin-for-syntax
  (define-syntax-class whitespace-str
    #:description "whitespace string"
    (pattern v:str
             #:fail-unless
             (regexp-match? #px"^\\s*$"
                            (syntax->datum #'v))
             "not exclusively whitespace"))
  (define-syntax-class spec-name-declaration
    #:description "spec name declaration"
    #:attributes {name to-extend}
    #:no-delimit-cut
    (pattern [#:spec ~! name:id
              (~alt (~optional (~seq #:with-local local-name:id)
                               #:name "local name declaration")
                    (~optional (~seq #:extends e:id ...+)
                               #:name "#:extends declaration"))
              ...]
             #:cut
             #:fail-when (xor (attribute local-name)
                              (attribute e))
             "expected either both #:extends and #:with-local, or neither"
             #:with to-extend #'((~? (~@ local-name [e ...]))))))

(define-syntax shifting-module-begin
  (make-module-begin #:doc-lang 'ricoeur/tei/spec-lang/doc-lang
                     #:module-begin #'#%plain-module-begin))

(define-for-syntax the-stop-list
  (list #'begin ;; it's implicitly added, but let's be clear
        ;; Need to not try to expand these:
        #'#%require 
        #'#%provide 
        #'define-values
        #'define-syntaxes
        #'module #'racket:module* #'racket:module+
        ))

(define-syntax-parser module-begin
  [(_ (~optional (~seq _:whitespace-str ...
                       decl:spec-name-declaration))
      body-for-doc ...)
   #:with spec-name (or (attribute decl.name)
                        (syntax-local-introduce
                         (datum->syntax #'define 'spec)))
   #:with to-extend (or (attribute decl.to-extend)
                        #'())
   #:with (runtime-body ...)
   (syntax-parse
       (local-expand (datum->syntax
                      this-syntax
                      (cons #'shifting-module-begin
                            (syntax->list #'(body-for-doc ...)))
                      this-syntax)
                     'module-begin
                     (cons #'#%plain-module-begin
                           the-stop-list))
     [(_ runtime-body ...)
      #'(runtime-body ...)])
   #`(#%module-begin
      (provide spec-name)
      (collect-spec-parts spec-name
                          to-extend
                          ()
                          (runtime-body ...)))])




(define-syntax-parser collect-spec-parts
  [(_ name:id
      to-extend
      (part:elements-specification-transformer-part ...)
      ())
   #:fail-unless (eq? (syntax-local-context) 'module)
   "only allowed in a module context"
   #'(prepare-spec name
                   to-extend
                   part ...)]
  [(_ name:id
      to-extend
      (part:elements-specification-transformer-part ...)
      (this:expr to-go:expr ...))
   #:fail-unless (eq? (syntax-local-context) 'module)
   "only allowed in a module context"
   (syntax-parse (local-expand #'this
                               'module
                               (cons #'elements-specification-transformer-part-id
                                     the-stop-list))
     #:literals {begin}
     [(begin body:expr ...)
      #:with (flattened ...) (flatten-all-begins
                              #'(begin body ...))
      #`(collect-spec-parts name
                            to-extend
                            (part ...)
                            (flattened ... to-go ...))]
     [new-part:elements-specification-transformer-part
      #`(collect-spec-parts name
                            to-extend
                            (part ... new-part)
                            (to-go ...))]
     [other
      #`(begin other
               (collect-spec-parts name
                                   to-extend
                                   (part ...)
                                   (to-go ...)))])])




(define-syntax-parser prepare-spec
  [(_ name:id
      (local-name:id [e:id ...])
      part:elements-specification-transformer-part ...)
   (for ([extend-id (in-syntax #'(e ...))])
     (unless (specification-group-info?
              (syntax-local-value extend-id (λ () #f)))
       (println (syntax-local-value extend-id (λ () #f)))
       (raise-syntax-error
        #f "not an elements specification transformer"
        extend-id)))
   #`(begin (prepare-spec local-name () part ...)
            (define-combined-elements-specification name
              [local-name e ...]))]
  [(_ name:id () part:elements-specification-transformer-part ...)
   #'(define-elements-specification-transformer name
       part ...)])









