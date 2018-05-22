#lang racket/base

(require racket/contract
         racket/class
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide TEI-info?
         plain-TEI-info?
         TEI-info<%>
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

#|(module+ private
  (provide (contract-out
            [make-plain-TEI-info
             any/c]
            )))|#

(define-values {prop:TEI-info TEI-info? get-get-plain}
  (make-struct-type-property 'prop:TEI-info))

#|
get-title
get-publication-date
get-original-publication-date
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

(struct plain-TEI-info ()
  #:property prop:TEI-info values)

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



