#lang racket

(require (except-in xml element?)
         ricoeur/tei/xexpr/tei-xexpr-contracts
         ricoeur/tei/oop/interfaces
         ricoeur/tei/oop/classes
         data/maybe
         data/order
         gregor
         )

(provide tag->element
         (contract-out
          [read-TEI
           (->* {} {input-port?} (is-a?/c TEI<%>))]
          [maybe-date<?
           (-> (maybe/c date?) (maybe/c date?) any/c)]
          [maybe-date>?
           (-> (maybe/c date?) (maybe/c date?) any/c)]
          ))

(define/contract (tag->element tag)
  (-> any-tei-xexpr/c (is-a?/c element<%>))
  (define-values {name attributes raw-body}
    (match tag
      [(list-rest name
                  (? (listof (list/c symbol? string?)) attributes)
                  raw-body)
       (values name attributes raw-body)]
      [(cons name raw-body)
       (values name null raw-body)]))
  (new (case name
         [(TEI) TEI%]
         [(teiHeader) teiHeader%] 
         [(fileDesc) fileDesc%]
         [(titleStmt) titleStmt%]
         [(title) title%]
         [(author) author%]
         [(editor) editor%]
         [(publicationStmt) publicationStmt%]
         [(authority) authority%]
         [(availability) availability%]
         [(sourceDesc) sourceDesc%]
         [(bibl) bibl%]
         [(date) date%]
         ;;;;
         [(text) text%]
         [(body) body%]
         [(front) front%]
         [(back) back%]
         [(div) div%]
         [(pb) pb%]
         [(list) list%]
         [(sp) sp%]
         [(ab) ab%]
         [(p) p%]
         [(head) head%]
         [(note) note%]
         [(item) item%])
       [name name]
       [attributes attributes]
       [body (for/list ([child (in-list raw-body)])
               (if (list? child)
                   (tag->element child)
                   child))]))

(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))

(define (read-TEI [in (current-input-port)])
  (discard-bom in)
  ;TODO: improve error message
  (tag->element
   (contract (tei-xexpr/c 'TEI)
             (xml->xexpr
              (document-element
               (read-xml in)))
             in
             '(definition read-TEI)
             (or (object-name in) in)
             #f)))
   

(define maybe-date-order
  (order 'maybe-date-order
         (maybe/c date?)
         (match-lambda** 
             [{(nothing) (nothing)}
              '=]
           [{(nothing) (just _)}
            '<]
           [{(just _) (nothing)}
            '>]
           [{(just a) (just b)}
            (date-order a b)])))

(define (maybe-date<? a b)
  (eq? '< (maybe-date-order a b)))
             
(define (maybe-date>? a b)
  (eq? '> (maybe-date-order a b)))
