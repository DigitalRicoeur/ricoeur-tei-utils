#lang racket/base

(require racket/contract
         racket/class
         gregor
         adjutor
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide instance-info?
         plain-instance-info?
         instance-info<%>
         instance-title 
         instance-citation 
         instance-orig-publication-date 
         instance-publication-date 
         instance-publication-original? 
         instance-book/article 
         (contract-out
          [instance-get-resp-string
           (-> instance-info? symbol? string?)]
          [get-plain-instance-info
           (-> instance-info?
               plain-instance-info?)]
          [prop:instance-info
           (struct-type-property/c
            (-> any/c plain-instance-info?))]
          [instance-info-mixin 
           (and/c mixin-contract
                  (-> instance-info-mixin-arg/c
                      (class/c
                       (init [TEI-info instance-info?]))))]
          ))

(module+ private
  (provide (contract-out
            [make-plain-instance-info
             (-> #:title string?
                 #:resp-table (hash/c symbol? string?
                                      #:immutable #t)
                 #:citation string?
                 #:orig-publication-date date?
                 #:publication-date date?
                 #:publication-original? any/c
                 #:book/article (or/c 'book 'article)
                 plain-instance-info?)]
            )))

(define-values {prop:instance-info instance-info? get-get-plain}
  (make-struct-type-property 'prop:instance-info))
        

(struct plain-instance-info (title
                             resp-table
                             citation
                             orig-publ-date
                             this-publ-date
                             publication-original?
                             book/article
                             )
  #:transparent
  #:property prop:instance-info values)

(define (make-plain-instance-info #:title title
                                  #:resp-table resp-table
                                  #:citation citation
                                  #:orig-publication-date orig-publ-date
                                  #:publication-date this-publ-date
                                  #:publication-original? publication-original?
                                  #:book/article book/article)
  (plain-instance-info title
                       resp-table
                       citation
                       orig-publ-date
                       this-publ-date
                       publication-original?
                       book/article)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-plain-instance-info v)
  ((get-get-plain v) v))

(define-syntax-rule (define-simple/plain [name simple-proc] ...)
  (begin (define (name this)
           (simple-proc (get-plain-instance-info this)))
         ...))

(define-simple/plain
  [instance-title plain-instance-info-title]
  [instance-citation plain-instance-info-citation]
  [instance-orig-publication-date plain-instance-info-orig-publ-date]
  [instance-publication-date plain-instance-info-this-publ-date]
  [instance-publication-original? plain-instance-info-publication-original?]
  [instance-book/article plain-instance-info-book/article])

   
(define (plain-instance-info-get-resp-string plain
                                             resp
                                             [who 'plain-instance-info-get-resp-string])
  (or (hash-ref (plain-instance-info-resp-table plain) resp #f)
      (raise-arguments-error who
                             "given resp symbol not found"
                             "given" resp)))
  
(define (instance-get-resp-string info resp)
  (plain-instance-info-get-resp-string (get-plain-instance-info info)
                                       resp 
                                       'instance-get-resp-string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values {instance-info<%> instance-info-mixin-arg/c instance-info-mixin}
  (let ()
    (define-member-name get-plain (generate-member-key))
    (define-values {instance-info<%> absent/c}
      (let* ([instance-info<%> (interface () get-plain)]
             [gen:get-plain
              (generic instance-info<%> get-plain)])
        (define-syntax (interface*+absent/c stx)
          (define-syntax-class name-clause
            #:attributes {method-name}
            (pattern method-name:id)
            (pattern [method-name:id _:expr]))
          (syntax-parse stx
            [(_ supers:expr props:expr
                method:name-clause ...)
             #`(let ([instance-info<%>
                      (interface* supers props method ...)])
                 (values instance-info<%>
                         (class/c
                          (absent method.method-name ...))))]))
        (interface*+absent/c
         (instance-info<%>)
         ([prop:instance-info 
           (λ (this) 
             (send-generic this gen:get-plain))])
         get-title 
         get-citation 
         get-orig-publication-date 
         get-publication-date 
         get-publication-original? 
         get-book/article 
         [get-resp-string (->m symbol? string?)])))
    (values
     instance-info<%>
     absent/c
     (mixin {} {instance-info<%>}
       (super-new)
       (init [(raw instance-info)])
       (define plain
         (get-plain-instance-info raw))
       (define/public-final (get-plain)
         plain)
       (define/public-final (get-resp-string resp)
         (plain-instance-info-get-resp-string plain resp 'get-resp-string))
       (define-syntax-rule (define-simple-methods/plain [name simple-proc] ...)
         (begin (define/public-final (name)
                  (simple-proc plain))
                ...))
       (define-simple-methods/plain
         [get-title plain-instance-info-title]
         [get-citation plain-instance-info-citation]
         [get-orig-publication-date plain-instance-info-orig-publ-date]
         [get-publication-date plain-instance-info-this-publ-date]
         [get-publication-original? plain-instance-info-publication-original?]
         [get-book/article plain-instance-info-book/article])
       #|END mixin|#))))





