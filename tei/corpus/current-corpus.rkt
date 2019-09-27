#lang racket/base

(require racket/contract
         racket/class
         ricoeur/tei/base
         "plain-corpus.rkt"
         syntax/parse/define
         (for-syntax racket/base
                     "exptime-common.rkt"
                     (only-in syntax/parse/lib/function-header
                              formal
                              formals)
                     racket/syntax))

(provide empty-corpus
         corpus-mixin+interface
         define-corpus-mixin+interface
         (contract-out
          [current-corpus
           (parameter/c (is-a?/c corpus%))]
          [get-instance-info-set
           (-> (instance-set/c))]
          [corpus-get-instance-info-set
           (-> (is-a?/c corpus%)
               (instance-set/c))]
          [get-checksum-table
           (-> checksum-table/c)]
          [corpus-get-checksum-table
           (-> (is-a?/c corpus%)
               checksum-table/c)]
          ))

 
(define empty-corpus
  (new corpus% [docs (instance-set)]))

(define current-corpus
  (make-parameter empty-corpus))


(define-simple-macro (define-base-corpus-getters
                       [method:id proc:id]
                       ...)
  #:with (gen ...) (generate-temporaries #'(method ...))
  (begin
    (~@ (define gen (generic corpus<%> method))
        (define (proc c) (send-generic c gen))
        (define (method) (proc (current-corpus))))
    ...))

(define-base-corpus-getters
  [get-instance-info-set corpus-get-instance-info-set]
  [get-checksum-table corpus-get-checksum-table])








(begin-for-syntax
  (define-syntax-class super-list
    #:description "parenthesized sequence of super-interface expressions"
    #:attributes {[super-interface 1]}
    (pattern (super-interface:expr ...)))

  
  (define-syntax-class plain-interface-method-clause
    #:description "interface method clause"
    ;; this variant won't support extra definitions
    (pattern :id)
    (pattern [:id :expr]))
  
  (define-syntax-class name-spec
    #:description #f
    #:attributes {name-mixin name<%>}
    (pattern [name-mixin:id name<%>:id])
    (pattern name:id
             #:do [(define name-len
                     (string-length (symbol->string (syntax-e #'name))))
                   (define (mk rhs)
                     (define made
                       (format-id #'name #:source #'name
                                  "~a~a" #'name rhs))
                     (syntax-property
                      made
                      'sub-range-binders
                      (vector-immutable (syntax-local-introduce made)
                                        0 name-len 0.5 0.5
                                        (syntax-local-introduce #'name)
                                        0 name-len 0.5 0.5)))]
             #:with name-mixin (mk "-mixin")
             #:with name<%> (mk "<%>")))


  
  (define-syntax-class define/method-id
    #:description #f
    (pattern (~or* (~literal define/public)
                   (~literal define/pubment)
                   (~literal define/public-final))))
    
  (define-syntax-class method-definition-form
    #:description "method definition form"
    #:attributes {method-definition method kw-formals send-args}
    (pattern (define/method:define/method-id
               (method:id
                . (~and (~or* (arg:formal ...)
                              (arg:formal ... . (~describe "rest argument"
                                                           rest-arg:id)))
                        ;; does great duplicate checking,
                        ;; but doesn't expose nested attributes
                        kw-formals:formals))
               body:expr ...)
             #:with method-definition this-syntax
             #:with (arg/kw ...)
             #'((~? (~@ arg.kw arg.name) arg.name) ...)
             #:with send-args
             #'(~? (arg/kw ... . rest-arg)
                   (arg/kw ...))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-syntax-class (interface-method*-clause name<%>-stx)
    #:description #f
    #:attributes {parsed method-definition [splice-after 1]}
    (pattern parsed:plain-interface-method-clause
             #:attr method-definition #f
             #:with (splice-after ...) #'())
    (pattern (~describe
              "extended interface method clause"
              [(~alt (~once :method-definition-form)
                     (~optional (~seq #:contract contract:expr))
                     (~optional (~seq #:proc proc-name:id))
                     (~optional
                      (~seq (~or* #:with-current/infer
                                  (~seq #:with-current explicit-with-current:id))
                            #:else
                            (~describe "parenthesized sequence of expressions"
                                       [else-body:expr ...]))))
               ...])
             #:with parsed #'(~? [method contract] method)
             #:with (gen-id) (generate-temporaries (list #'method))
             #:with (splice-after ...)
             (if (not (or (attribute proc-name) (attribute else-body)))
                 #'()
                 #`((define gen-id (generic #,name<%>-stx method))
                    (~? (define (proc-name corpus . kw-formals)
                          (send-generic corpus gen-id . send-args)))
                    (~? (define ((~? explicit-with-current method) . kw-formals)
                          (define corpus (current-corpus))
                          (cond
                            [(is-a? corpus #,name<%>-stx)
                             (send-generic corpus gen-id . send-args)]
                            [else
                             else-body ...]))))))))







(define-syntax-parser define-corpus-mixin+interface
  #:track-literals
  #:literals {interface interface*}
  [(_ :name-spec
      from:from-interfaces to:to-interfaces
      (~or* (interface :super-list
              if-clause
              ...)
            (interface* :super-list
                        ([prop:expr val:expr] ...)
              if-clause
              ...))
      mixin-clause:class-clause
      ...+)
   #:declare if-clause (interface-method*-clause #'name<%>)
   #:with name-mixin-tag-method
   ((make-syntax-introducer)
    (format-id #f #:source #'name-mixin
               "~a-tag-method" #'name-mixin))
   #'(begin
       (define-member-name name-mixin-tag-method
         (generate-member-key))
       (define name<%>
         (interface* (corpus<%> super-interface ...)
                     (~? ([prop val] ...) ())
           name-mixin-tag-method
           if-clause.parsed ...))
       (define name-mixin
         (corpus-mixin (from.<%> ...) (to.<%> ... name<%>)
           (define/public-final (name-mixin-tag-method)
             (void))
           (~? if-clause.method-definition) ...
           mixin-clause ...))
       (~? (~@ if-clause.splice-after ...))
       ...)])








(define-syntax-parser corpus-mixin+interface
  #:track-literals
  #:literals {interface interface*}
  [(_ from:from-interfaces to:to-interfaces
      (~or* (interface supers:super-list
              if-clause:plain-interface-method-clause
              ...)
            (interface* supers:super-list
                        ([prop:expr val:expr] ...)
              if-clause:plain-interface-method-clause
              ...))
      mixin-clause:class-clause
      ...+)
   #'(let ()
       (define-corpus-mixin+interface
         [anonymous-corpus-mixin anonymous-corpus-mixin<%>]
         (from.<%> ...) (to.<%> ...)
         (interface* supers
                     (~? ([prop val] ...) ())
           if-clause ...)
         mixin-clause ...)
       (values anonymous-corpus-mixin anonymous-corpus-mixin<%>))])






