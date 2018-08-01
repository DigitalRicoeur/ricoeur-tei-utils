#lang racket/base

(require racket/contract
         racket/match
         racket/string
         adjutor
         xml
         "plain-contracts.rkt"
         "entity-utils.rkt"
         "../pre-kernel-lib.rkt"
         (submod "plain-contracts.rkt" private)
         )

(provide (contract-out
          [normalize-xexpr
           (-> raw-xexpr/c normalized-xexpr/c)]
          [non-element-xexpr->plain-text
           (-> raw-xexpr-atom/c string-immutable/c)]
          [non-element-body->plain-text
           (-> (listof raw-xexpr-atom/c) string-immutable/c)]
          ))

(define normalize-xexpr
  (match-lambda
    [(? normalized-xexpr? it)
     it]
    [(list-rest name (and attrs (list (? pair?) ...)) body)
     (list* name
            (normalize-attributes-list attrs)
            (map normalize-xexpr body))]
    [(cons name body)
     (list* name '() (map normalize-xexpr body))]
    [(? string? it)
     (string->immutable-string it)]
    [(? comment? it)
     (normalize-comment it)]
    [(? p-i? it)
     (normalize-p-i it)]
    [it
     (normalize-to-string it)]))

(define (normalize-attributes-list lst)
  (cond
    [(or (null? lst)
         (andmap (match-lambda
                   [(list _ str)
                    (immutable? str)])
                 lst))
     lst]
    [else
     (for/list ([it (in-list lst)])
       (match it
         [(list name (app string->immutable-string val))
          (list name val)]))]))

(define non-element-xexpr->plain-text
  (match-lambda
    [(? string? it)
     (string->immutable-string it)]
    [(or (? comment? it)
         (? p-i? it))
     ""]
    [it
     (normalize-to-string it)]))


(define (non-element-body->plain-text body)
  (string->immutable-string
   (string-join (map non-element-xexpr->plain-text
                     body)
                ;; should this be " " ???
                ""))) 


(define (normalize-to-string it)
  (string->immutable-string
   (match it
     [(? symbol? it)
      (entity-symbol->string it)]
     [(? valid-char? it)
      (string (integer->char it))]
     [(cdata _ _ (or (regexp #rx"^<!\\[CDATA\\[(.*)\\]\\]>$"
                             (list _ content))
                     content))
      (or content "")])))



