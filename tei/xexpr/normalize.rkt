#lang racket/base

(require "entity-utils.rkt"
         xml
         racket/match
         racket/contract
         racket/string
         )

(provide (contract-out
          [non-element-xexpr->plain-text
           (-> non-element-xexpr/c string?)]
          [non-element-body->plain-text
           (-> (listof non-element-xexpr/c) string?)]
          ))

(define (non-element-xexpr->plain-text child)
  (cond
    [(string? child)
     child]
    [(or (comment? child)
         (p-i? child))
     ""]
    [(cdata? child)
     (cdata->plain-text child)]
    [(valid-char? child)
     (valid-char->plain-text child)]
    [(symbol? child) 
     (string (entity-symbol->char child))]))

(define (non-element-body->plain-text body)
  (string-join (map non-element-xexpr->plain-text
                    body)
               ""))

(define non-element-xexpr/c
  (or/c string?
        symbol?
        valid-char?
        cdata?
        comment?
        p-i?))

(define (cdata->plain-text it)
  (match (cdata-string it)
    [(regexp #rx"<!\\[CDATA\\[(.*)\\]\\]>" (list _ content))
     content]
    [content content]))

(define (valid-char->plain-text num)
  (string (integer->char num)))



