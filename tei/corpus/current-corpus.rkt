#lang racket/base

(require racket/contract
         racket/class
         ricoeur/tei/base
         "plain-corpus.rkt"
         syntax/parse/define
         (for-syntax racket/base
                     "exptime-common.rkt"
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


(define-simple-macro (define-corpus-getters
                       [method:id proc:id]
                       ...)
  #:with (gen ...) (generate-temporaries #'(method ...))
  (begin
    (~@ (define gen (generic corpus<%> method))
        (define (proc c) (send-generic c gen))
        (define (method) (proc (current-corpus))))
    ...))

(define-corpus-getters
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

  
  (define-syntax-class interface-method*-clause
    #:description "interface method clause"
    #:attributes {parsed}
    ;; TODO: extend this to define functions,
    ;; with local macros to expand to send-generic in their
    ;; implementations, and handle when (current-corpus)
    ;; doesn't implement it.
    (pattern parsed:id)
    (pattern [method:id contract:expr]
             #:with parsed this-syntax)
    (pattern [method:id (~optional contract:expr)]
             #:with parsed #'(~? [method contract] method))))







(define-syntax-parser define-corpus-mixin+interface
  #:track-literals
  #:literals {interface interface*}
  [(_ :name-spec
      from:from-interfaces to:to-interfaces
      (~or* (interface :super-list
              if-clause:interface-method*-clause
              ...)
            (interface* :super-list
                        ([prop:expr val:expr] ...)
              if-clause:interface-method*-clause
              ...))
      mixin-clause:class-clause
      ...+)
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
           mixin-clause ...)))])








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






