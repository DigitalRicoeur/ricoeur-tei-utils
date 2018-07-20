#lang racket

(require racket/serialize
         racket/contract
         syntax/parse/define
         ricoeur/tei/kernel
         ricoeur/tei/base/def-from-spec
         "location-stack.rkt"
         )

(provide segment?
         segment-meta?
         page-spec/c
         (contract-out
          [prop:segment
           (struct-type-property/c
            (-> any/c segment-meta?))]
          [segment-get-meta
           (-> segment? segment-meta?)]
          ;;;;
          [segment-title/symbol
           (-> segment? symbol?)]
          [segment-counter
           (-> segment? natural-number/c)]
          [segment-resp-string
           (-> segment? string-immutable/c)]
          [segment-page-spec
           (-> segment? page-spec/c)]
          [segment-location-stack
           (-> segment? location-stack?)]
          ;;;;
          [segment-by-ricoeur?
           (-> segment? any/c)]
          [segment-meta=?
           (-> segment? segment? any/c)]
          ))

(module+ private
  (provide (contract-out
            [tei-document->make-segment-meta
             (-> tei-document?
                 (-> #:i natural-number/c
                     #:resp symbol?
                     #:page page-spec/c
                     #:location location-stack?
                     segment-meta?))]
            )))

   
(define-values {prop:segment segment? get-get-meta}
  (make-struct-type-property 'prop:segment))

(define (segment-get-meta s)
  ((get-get-meta s) s))

(define/final-prop page-spec/c
  (let ([maybe-string-immutable/c
         (maybe/c string-immutable/c)])
    (or/c maybe-string-immutable/c
          (list/c maybe-string-immutable/c
                  maybe-string-immutable/c))))

(serializable-struct segment-meta (title/symbol
                                   checksum
                                   counter
                                   resp-sym
                                   resp-string
                                   page-spec
                                   location-stack)
  #:transparent
  #:property prop:segment values)


(define (tei-document->make-segment-meta doc)
  (define checksum
    (tei-document-checksum doc))
  (define title/symbol
    (instance-title/symbol doc))
  (λ (#:i counter
      #:resp resp-sym
      #:page page-spec
      #:location location-stack)
    (segment-meta title/symbol
                  checksum
                  counter
                  resp-sym
                  (instance-get-resp-string doc resp-sym)
                  page-spec
                  location-stack)))


(define-syntax-rule (define-segment/meta-proc [name proc] ...)
  (begin (define (name it)
           (proc (segment-get-meta it))) ...))

(define-segment/meta-proc
  [segment-title/symbol segment-meta-title/symbol]
  [segment-counter segment-meta-counter]
  [segment-resp-string segment-meta-resp-string]
  [segment-page-spec segment-meta-page-spec]
  [segment-location-stack segment-meta-location-stack])


(define (segment-by-ricoeur? it)
  (eq? 'ricoeur (segment-meta-resp-sym
                 (segment-get-meta it))))

(define (segment-meta=? a b)
  (equal? (segment-get-meta a)
          (segment-get-meta b)))

