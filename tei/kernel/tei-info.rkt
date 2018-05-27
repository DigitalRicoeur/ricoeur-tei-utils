#lang racket/base

(require racket/contract
         racket/class
         gregor
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide TEI-info?
         plain-TEI-info?
         TEI-info<%>
         guess-paragraphs-status/c
         (contract-out
          [get-plain-TEI-info
           (-> TEI-info?
               plain-TEI-info?)]
          [prop:TEI-info
           (struct-type-property/c
            (-> any/c plain-TEI-info?))]
          [TEI-info-mixin ;needs contract
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
                 #:guess-paragraphs-status guess-paragraphs-status/c
                 plain-TEI-info?)]
            )))

(define-values {prop:TEI-info TEI-info? get-get-plain}
  (make-struct-type-property 'prop:TEI-info))

(define/final-prop guess-paragraphs-status/c
  (or/c 'todo
        'line-breaks
        'blank-lines
        'done
        'skip))
        
#|
get-title
get-publication-date
get-orig-publication-date
get-citation
get-book/article
get-resp-string
;; get-guess-paragraphs-status currently exists, but
;; may want to abstract over more details for public interface
get-guess-paragraphs-status
;;;;;;;;;;;
;; Things that probably should be added:
get-this-is-original?

|#

(struct plain-TEI-info (title
                        resp-table
                        citation
                        orig-publ-date
                        this-publ-date
                        publication-original?
                        book/article
                        guess-paragraphs-status
                        )
  #:transparent
  #:property prop:TEI-info values)

(define (make-plain-TEI-info #:title title
                             #:resp-table resp-table
                             #:citation citation
                             #:orig-publication-date orig-publ-date
                             #:publication-date this-publ-date
                             #:publication-original? publication-original?
                             #:book/article book/article
                             #:guess-paragraphs-status guess-paragraphs-status)
  (plain-TEI-info title
                  resp-table
                  citation
                  orig-publ-date
                  this-publ-date
                  publication-original?
                  book/article
                  guess-paragraphs-status)) 

(define (get-plain-TEI-info v)
  ((get-get-plain v) v))

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
             (send-generic this gen:get-plain))]))
        ))
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
       #|END mixin|#))))



