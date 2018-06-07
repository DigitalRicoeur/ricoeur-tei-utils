#lang racket/base

(require racket/contract
         racket/class
         gregor
         adjutor
         (for-syntax racket/base
                     syntax/parse
                     ))

;; With the removal of tei-document-paragraphs-status,
;; TEI-info is now exclusively bibliographic,
;; rather than about the state of the file.
;; It might be better to use a different name, since
;; the "info" now is about the work, not the TEI document.

(provide TEI-info?
         plain-TEI-info?
         TEI-info<%>
         tei-title 
         tei-citation 
         tei-orig-publication-date 
         tei-publication-date 
         tei-publication-original? 
         tei-book/article 
         (contract-out
          [tei-get-resp-string
           (-> TEI-info? symbol? string?)]
          [get-plain-TEI-info
           (-> TEI-info?
               plain-TEI-info?)]
          [prop:TEI-info
           (struct-type-property/c
            (-> any/c plain-TEI-info?))]
          [TEI-info-mixin 
           (and/c mixin-contract
                  (-> TEI-info-mixin-arg/c
                      (class/c
                       (init [TEI-info TEI-info?]))))]
          ))

(module+ private
  (provide (contract-out
            [make-plain-TEI-info
             (-> #:title string?
                 #:resp-table (hash/c symbol? string?
                                      #:immutable #t)
                 #:citation string?
                 #:orig-publication-date date?
                 #:publication-date date?
                 #:publication-original? any/c
                 #:book/article (or/c 'book 'article)
                 plain-TEI-info?)]
            )))

(define-values {prop:TEI-info TEI-info? get-get-plain}
  (make-struct-type-property 'prop:TEI-info))
        

(struct plain-TEI-info (title
                        resp-table
                        citation
                        orig-publ-date
                        this-publ-date
                        publication-original?
                        book/article
                        )
  #:transparent
  #:property prop:TEI-info values)

(define (make-plain-TEI-info #:title title
                             #:resp-table resp-table
                             #:citation citation
                             #:orig-publication-date orig-publ-date
                             #:publication-date this-publ-date
                             #:publication-original? publication-original?
                             #:book/article book/article)
  (plain-TEI-info title
                  resp-table
                  citation
                  orig-publ-date
                  this-publ-date
                  publication-original?
                  book/article)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-plain-TEI-info v)
  ((get-get-plain v) v))

(define-syntax-rule (define-simple/plain [name simple-proc] ...)
  (begin (define (name this)
           (simple-proc (get-plain-TEI-info this)))
         ...))

(define-simple/plain
  [tei-title plain-TEI-info-title]
  [tei-citation plain-TEI-info-citation]
  [tei-orig-publication-date plain-TEI-info-orig-publ-date]
  [tei-publication-date plain-TEI-info-this-publ-date]
  [tei-publication-original? plain-TEI-info-publication-original?]
  [tei-book/article plain-TEI-info-book/article])

   
(define (plain-TEI-info-get-resp-string plain
                                        resp
                                        [who 'plain-TEI-info-get-resp-string])
  (or (hash-ref (plain-TEI-info-resp-table plain) resp #f)
      (raise-arguments-error who
                             "given resp symbol not found"
                             "given" resp)))
  
(define (tei-get-resp-string info resp)
  (plain-TEI-info-get-resp-string (get-plain-TEI-info info)
                                  resp 
                                  'tei-get-resp-string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values {TEI-info<%> TEI-info-mixin-arg/c TEI-info-mixin}
  (let ()
    (define-member-name get-plain (generate-member-key))
    (define-values {TEI-info<%> absent/c}
      (let* ([TEI-info<%> (interface () get-plain)]
             [gen:get-plain
              (generic TEI-info<%> get-plain)])
        (define-syntax (interface*+absent/c stx)
          (define-syntax-class name-clause
            #:attributes {method-name}
            (pattern method-name:id)
            (pattern [method-name:id _:expr]))
          (syntax-parse stx
            [(_ supers:expr props:expr
                method:name-clause ...)
             #`(let ([TEI-info<%>
                      (interface* supers props method ...)])
                 (values TEI-info<%>
                         (class/c
                          (absent method.method-name ...))))]))
        (interface*+absent/c
         (TEI-info<%>)
         ([prop:TEI-info 
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
     TEI-info<%>
     absent/c
     (mixin {} {TEI-info<%>}
       (super-new)
       (init [(raw TEI-info)])
       (define plain
         (get-plain-TEI-info raw))
       (define/public-final (get-plain)
         plain)
       (define/public-final (get-resp-string resp)
         (plain-TEI-info-get-resp-string plain resp 'get-resp-string))
       (define-syntax-rule (define-simple-methods/plain [name simple-proc] ...)
         (begin (define/public-final (name)
                  (simple-proc plain))
                ...))
       (define-simple-methods/plain
         [get-title plain-TEI-info-title]
         [get-citation plain-TEI-info-citation]
         [get-orig-publication-date plain-TEI-info-orig-publ-date]
         [get-publication-date plain-TEI-info-this-publ-date]
         [get-publication-original? plain-TEI-info-publication-original?]
         [get-book/article plain-TEI-info-book/article])
       #|END mixin|#))))





