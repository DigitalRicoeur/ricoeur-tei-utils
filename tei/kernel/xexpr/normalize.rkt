#lang racket/base

(require racket/contract
         racket/match
         racket/string
         xml
         "plain-contracts.rkt"
         "entity-utils.rkt"
         ricoeur/kernel/pre-kernel-lib
         (submod "plain-contracts.rkt" private))

(provide (contract-out
          [normalize-xexpr
           (-> raw-xexpr/c normalized-xexpr/c)]
          [non-element-xexpr->plain-text
           (-> raw-xexpr-atom/c string-immutable/c)]
          [non-element-body->plain-text
           (-> (listof raw-xexpr-atom/c) string-immutable/c)]
          [write-xexpr/standardized
           (->* {xexpr/c}
                {output-port?}
                any)]
          [read-xexpr/standardized
           (->* {input-port?}
                xexpr/c)]
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

(define (write-xexpr/standardized xs [out (current-output-port)])
  (parameterize ([empty-tag-shorthand 'always]
                 [collapse-whitespace #f])
    (write-xexpr xs out)))

(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))

(define (read-xexpr/standardized [in (current-input-port)])
  (parameterize ([collapse-whitespace #f]
                 [read-comments #t]
                 [xml-count-bytes #f]
                 [xexpr-drop-empty-attributes #f])
    (discard-bom in)
    (xml->xexpr
     (document-element
      (read-xml in)))))



