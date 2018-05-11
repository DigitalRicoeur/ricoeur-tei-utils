#lang racket

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
          ;TEI-info-mixin
          ))

(module+ provate
  (provide (contract-out
            [plain-TEI-info
             any/c])))

(define-values {prop:TEI-info TEI-info? get-get-plain}
  (make-struct-type-property 'prop:TEI-info))

(struct plain-TEI-info ()
  #:property prop:TEI-info values)

(define (get-plain-TEI-info v)
  ((get-get-plain v) v))

(define-values {TEI-info<%> TEI-info-mixin}
  (let ()
    (define-member-name get-plain (generate-member-key))
    (define TEI-info<%>
      (let* ([TEI-info<%> (interface () get-plain)]
             [gen:get-plain
              (generic TEI-info<%> get-plain)])
        (interface* (TEI-info<%>)
                    ([prop:TEI-info 
                      (λ (this) 
                        (send-generic this gen:get-plain))]))))
    (values
     TEI-info<%>
     (mixin {} {TEI-info<%>}
       (super-new)
       (init [(raw TEI-info)])
       (define plain
         (get-plain-TEI-info raw))
       (define/public-final (get-plain)
         plain)
       #|END mixin|#))))



