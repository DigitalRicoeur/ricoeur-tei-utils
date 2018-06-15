#lang racket/base

(require ricoeur/tei/kernel/sans-lang
         syntax/parse/define
         racket/require
         (multi-in ricoeur/tei/kernel/lang
                   (elem-for-runtime 
                    begin-for-runtime))
         (only-in ricoeur/tei/kernel/lang/link
                  define-combined-elements-specification)
         (for-syntax racket/base
                     racket/list
                     racket/sequence
                     syntax/parse
                     syntax/flatten-begin
                     syntax/strip-context
                     (multi-in ricoeur/tei/kernel/lang
                               (ir/struct
                                ir/syntax-class
                                static-info
                                runtime-lift-state
                                ))))

(require-provide (provide-only ricoeur/tei/kernel/sans-lang)
                 racket/contract
                 racket/match
                 racket/string
                 racket/list
                 (multi ricoeur/tei/kernel/lang
                        (stxparam
                         adt
                         interface-macros
                         )))

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         define-element/runtime ;; TODO: something better w/ syntax-local-require
         (rename-out
          [module-begin #%module-begin]
          ))


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
    (pattern [#:spec name:id]
             #:with to-extend #'())
    (pattern [#:spec name:id
              (~alt (~once (~seq #:with-local local-name:id)
                           #:name "local name declaration")
                    (~once (~seq #:extends e:id ...+)
                           #:name "#:extends declaration"))
              ...]
             #:with to-extend #'(local-name [e ...]))))

(define-syntax-parser module-begin
  [(_ (~optional (~seq _:whitespace-str ...
                       decl:spec-name-declaration)
                 #:defaults
                 ([decl.name (datum->syntax this-syntax 'spec)]
                  [decl.to-extend #'()]))
      body-for-doc ...)
   #:with doc (datum->syntax this-syntax 'doc)
   #:with for-doc-lang
   (datum->syntax #;this-syntax
                  (car (syntax->list #'(body-for-doc ...)))
                  'ricoeur/tei/kernel/lang/doc-lang
                  (vector (syntax-source this-syntax) 1 0 1 1)) ;????
   #:do [(define doctime-introduce
           (make-syntax-introducer #t))
         (define runtime-introduce
           (make-syntax-introducer #t))
         (define target
           (make-runtime-lift-target))
         (define runtime-lifts
           (parameterize ([current-runtime-lift-target target])
             ;; Thanks https://groups.google.com/d/msg/racket-users/zpe27qAdHG0/iWWdxpuZEAAJ
             (local-expand #`(#%plain-module-begin
                              (require (rename-in #,(doctime-introduce
                                                     (strip-context #'for-doc-lang))
                                                  [#,(doctime-introduce
                                                      (datum->syntax #f
                                                                     '#%module-begin))
                                                   doc-module-begin]))
                              (expand-for-effect doc-module-begin
                                                 doc
                                                 #,@(syntax->list
                                                     (doctime-introduce
                                                      (strip-context #'(body-for-doc ...))))))
                           'module-begin
                           null)
             (runtime-lift-target->list target)))]
   #:with (runtime-body ...)
   (map (compose1 make-check-syntax-original
                  runtime-introduce
                  doctime-introduce
                  (make-syntax-delta-introducer #'for-doc-lang
                                                #f))
        runtime-lifts)
   #`(#%module-begin
      (provide decl.name)
      (module* doc for-doc-lang
        doc body-for-doc ...)
      (module+ test
        (require (submod ".." doc)))
      (collect-spec-parts decl.name
                          #,(runtime-introduce #'decl.to-extend)
                          ()
                          (runtime-body ...)))])


(define-syntax-parser expand-for-effect
  [(_ doc-module-begin:id body ...)
   (local-expand #`(doc-module-begin body ...)
                 'module-begin
                 null)
   #'(void)])


(define-for-syntax (make-check-syntax-original stx)
  (syntax-property (let ([lst (syntax->list stx)])
                     (if lst
                         #`(#,@(map make-check-syntax-original
                                    lst))
                         stx))
                   'original-for-check-syntax #t))



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
                               (list #'elements-specification-transformer-part-id
                                     #'begin ;; it's implicitly added, but let's be clear
                                     ;; Need to not try to expand these:
                                     #'#%require
                                     #'#%provide
                                     #'define-values
                                     #'define-syntaxes
                                     #'module #'module* #'module+
                                     ))
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









