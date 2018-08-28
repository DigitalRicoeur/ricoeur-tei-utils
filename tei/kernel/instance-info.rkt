#lang racket/base

(require racket/contract
         racket/class
         racket/set
         racket/stream
         racket/match
         "pre-kernel-lib.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide instance-info?
         plain-instance-info?
         instance-info
         instance-info<%>
         instance-title
         instance-title/symbol
         instance-citation 
         instance-orig-publication-date 
         instance-publication-date 
         instance-publication-original?
         instance-language
         instance-book/article
         (rename-out
          [immutable-instance-set? instance-set?])
         instance-set/c
         in-instance-set
         (contract-out
          [instance-get-resp-string
           (-> instance-info? symbol? string?)]
          [get-plain-instance-info
           (-> instance-info?
               plain-instance-info?)]
          [prop:instance-info
           (struct-type-property/c
            (-> any/c plain-instance-info?))]
          [rename make-immutable-instance-set
                  instance-set
                  (->* {}
                       {(stream/c instance-info?)}
                       instance-set?)]
          [instance-set*
           (-> instance-info? ... instance-set?)]
          [instance-info-mixin 
           (and/c mixin-contract
                  (-> instance-info-mixin-arg/c
                      (and/c
                       (implementation?/c instance-info<%>)
                       (class/c
                        (init [instance-info instance-info?])))))]
          ))

(module+ private
  (provide (contract-out
            [make-plain-instance-info
             (-> #:title string-immutable/c
                 #:resp-table (hash/c symbol?
                                      string-immutable/c
                                      #:immutable #t)
                 #:citation string-immutable/c
                 #:orig-publication-date date?
                 #:publication-date date?
                 #:publication-original? any/c
                 #:language (or/c 'en 'fr)
                 #:book/article (or/c 'book 'article)
                 plain-instance-info?)]
            )))

(define-values {prop:instance-info instance-info? get-get-plain}
  (make-struct-type-property 'prop:instance-info))
        

(struct plain-instance-info (title
                             title/symbol
                             resp-table
                             citation
                             orig-publ-date
                             this-publ-date
                             publication-original?
                             language
                             book/article
                             )
  #:transparent
  #:property prop:instance-info values)

(define-match-expander instance-info
  (syntax-parser
    [(_ (~alt
         (~optional (~seq #:title title-pat:expr)
                    #:defaults ([title-pat #'_]))
         (~optional (~seq #:title/symbol title/symbol-pat:expr)
                    #:defaults ([title/symbol-pat #'_]))
         (~optional (~seq #:citation citation-pat:expr)
                    #:defaults ([citation-pat #'_]))
         (~optional (~seq #:orig-publication-date orig-publication-date-pat:expr)
                    #:defaults ([orig-publication-date-pat #'_]))
         (~optional (~seq #:publication-date publication-date-pat:expr)
                    #:defaults ([publication-date-pat #'_]))
         (~optional (~seq #:publication-original? publication-original?-pat:expr)
                    #:defaults ([publication-original?-pat #'_]))
         (~optional (~seq #:language lang-pat:expr)
                    #:defaults ([lang-pat #'_]))
         (~optional (~seq #:book/article book/article-pat:expr)
                    #:defaults ([book/article-pat #'_])))
        ...)
     #:with plain-pat
     #'(plain-instance-info title-pat
                            title/symbol-pat
                            _
                            citation-pat
                            orig-publication-date-pat
                            publication-date-pat
                            publication-original?-pat
                            lang-pat
                            book/article-pat)
     #'(or plain-pat
           (? instance-info?
              (app get-plain-instance-info
                   plain-pat)))]))

(define (make-plain-instance-info #:title title
                                  #:resp-table resp-table
                                  #:citation citation
                                  #:orig-publication-date orig-publ-date
                                  #:publication-date this-publ-date
                                  #:publication-original? publication-original?
                                  #:language lang
                                  #:book/article book/article)
  (when publication-original?
    (unless (equal? orig-publ-date this-publ-date)
      (error 'make-plain-instance-info
             "unequal publication dates for thisIsOriginal")))
  (plain-instance-info title
                       (string->symbol title)
                       resp-table
                       citation
                       orig-publ-date
                       this-publ-date
                       (any->boolean publication-original?)
                       lang
                       book/article)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-custom-set-types instance-set
  #:elem? instance-info?
  (λ (a b)
    (eq? (instance-title/symbol a)
         (instance-title/symbol b)))
  (λ (this)
    (eq-hash-code
     (instance-title/symbol this))))

(define (instance-set* . args)
  (make-immutable-instance-set args))

(define/subexpression-pos-prop instance-set/c
  (let ([immutable-instance-set?
         (flat-named-contract 'instance-set?
                              immutable-instance-set?)])
    (case-lambda
      [() immutable-instance-set?]
      [(elem/c)
       (let ([elem/c (coerce-chaperone-contract 'instance-set/c elem/c)])
         (rename-contract
          (and/c immutable-instance-set?
                 (set/c elem/c #:kind 'immutable))
          (build-compound-type-name 'instance-set/c elem/c)))])))

(define (check-instance-set v)
  (unless (immutable-instance-set? v)
    (raise-argument-error 'in-instance-set "instance-set?" v))
  v)

(define (in-instance-set/first-class v)
  (in-immutable-set (check-instance-set v)))

(define-sequence-syntax in-instance-set
  (λ () #'in-instance-set/first-class)
  (syntax-parser
    [[(elem:id) (_ v:expr)]
     #'[(elem) (in-immutable-set (check-instance-set v))]]))

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
  [instance-title/symbol plain-instance-info-title/symbol]
  [instance-citation plain-instance-info-citation]
  [instance-orig-publication-date plain-instance-info-orig-publ-date]
  [instance-publication-date plain-instance-info-this-publ-date]
  [instance-publication-original? plain-instance-info-publication-original?]
  [instance-language plain-instance-info-language]
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
         get-title/symbol
         get-citation 
         get-orig-publication-date 
         get-publication-date 
         get-publication-original?
         get-language
         get-book/article 
         [get-resp-string (->m symbol? string-immutable/c)])))
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
         [get-title/symbol plain-instance-info-title/symbol]
         [get-citation plain-instance-info-citation]
         [get-orig-publication-date plain-instance-info-orig-publ-date]
         [get-publication-date plain-instance-info-this-publ-date]
         [get-publication-original? plain-instance-info-publication-original?]
         [get-language plain-instance-info-language]
         [get-book/article plain-instance-info-book/article])
       #|END mixin|#))))





