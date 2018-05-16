#lang racket/base

(require racket/contract
         racket/match
         racket/string
         xml
         "plain-contracts.rkt"
         "entity-utils.rkt"
         )

(provide (contract-out
          [normalize-xexpr
           (-> raw-xexpr/c normalized-xexpr/c)]
          [non-element-xexpr->plain-text
           (-> raw-xexpr-atom/c string?)]
          [non-element-body->plain-text
           (-> (listof raw-xexpr-atom/c) string?)]
          ))

(define normalize-xexpr
  (match-lambda
    [(list-rest name (and attrs (list (? pair?) ...)) body)
     (list* name attrs (map normalize-xexpr body))]
    [(cons name body)
     (list* name '() (map normalize-xexpr body))]
    [(or (? string? it)
         (? comment? it)
         (? p-i? it))
     it]
    [it
     (normalize-to-string it)]))


(define non-element-xexpr->plain-text
  (match-lambda
    [(? string? it)
     it]
    [(or (? comment? it)
         (? p-i? it))
     ""]
    [it
     (normalize-to-string it)]))


(define (non-element-body->plain-text body)
  (string-join (map non-element-xexpr->plain-text
                    body)
               ""))


(define normalize-to-string 
  (match-lambda
    [(? symbol? it)
     (entity-symbol->string it)]
    [(? valid-char? it)
     (string (integer->char it))]
    [(cdata _ _ (or (regexp #rx"^<!\\[CDATA\\[(.*)\\]\\]>$"
                            (list _ content))
                    content))
     (or content "")]))



