#lang racket

(require ricoeur/tei/oop-kernel
         (only-in xml
                  xml->xexpr
                  document-element
                  read-xml
                  )
         ricoeur/tei/oop/interfaces
         ricoeur/tei/oop/classes
         data/order
         )

(module+ test
  (require rackunit
           (submod "..")))

(provide title<?
         (contract-out
          [read-TEI
           (->* {}
                {input-port?
                 #:filename (or/c #f path-string?)}
                (is-a?/c TEI<%>))]
          [file->TEI
           (-> (and/c path-string? file-exists?)
               (is-a?/c TEI<%>))]
          [tag->element
           (->* {any-tei-xexpr/c}
                {#:filename (or/c #f path-string?)}
                (is-a?/c element<%>))]
          [maybe-date<?
           (-> (maybe/c date?) (maybe/c date?) any/c)]
          [maybe-date>?
           (-> (maybe/c date?) (maybe/c date?) any/c)]
          ))

(define/contract (tag->element tag #:filename [filename #f])
  (->* {any-tei-xexpr/c}
       {#:filename (or/c #f path-string?)}
       (is-a?/c element<%>))
  (parameterize ([current-filename (cond
                                     [filename
                                      (define-values (base name dir?)    
                                        (split-path filename))
                                      (path->string name)]
                                     [else #f])]
                 [current-full-path (and filename
                                         (simplify-path filename))])
    (tag->element* (normalize-xexpr tag))))

(define (tag->element* tag)
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
         [(profileDesc) profileDesc%]
         [(textClass) textClass%]
         [(catRef) catRef%]
         [(keywords) keywords%]
         [(term) term%]
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
                   (tag->element* child)
                   child))]))
  

(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))

(define (read-TEI [in (current-input-port)] #:filename [filename #f])
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
             #f)
   #:filename filename))

(define (file->TEI pth-str)
  (parameterize ([current-object-modify-seconds
                  (file-or-directory-modify-seconds pth-str)])
    (call-with-input-file  pth-str
      #:mode 'text
      (λ (in) (read-TEI in #:filename pth-str)))))


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








