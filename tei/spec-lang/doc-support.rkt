#lang _-exp racket/base

(require scribble/manual
         scribble/base
         scribble/core
         scribble/decode
         scribble/html-properties
         scribble/bnf
         racket/contract
         racket/match
         racket/list
         adjutor
         ricoeur/tei/kernel/schema)

(provide (all-from-out adjutor)
         DR-TEI_doc.html
         (contract-out
          [tag
           (-> (or/c symbol? string?) element?)]
          [attr
           (-> (or/c symbol? string?) element?)]
          [make-other-doc-tag
           (-> (or/c #f module-path?)
               (-> (or/c symbol? string?) element?))]
          ))

(module+ private
  (provide (contract-out
            [make-element-rows
             (->* (#:name symbol?)
                  (#:children
                   (or/c #f (listof (cons/c symbol?
                                            (or/c 1 '1+ '0-1 '0+))))
                   #:required-order (or/c #f (listof symbol?))
                   #:attr-contracts
                   (or/c #f (listof (cons/c symbol?
                                            (or/c block? content?))))
                   #:required-attrs (or/c #f (listof symbol?))
                   #:text? any/c)
                  (listof (listof (or/c block? content? 'cont))))]
            [make-defelement-flow
             (->* ()
                  (#:inset? any/c
                   #:elements (listof (listof (listof (or/c block?
                                                            content?
                                                            'cont)))))
                  #:rest (listof pre-flow?)
                  nested-flow?)]
            )))

(TODO/void tagfont: #:
           should it resemble the TEI Lint syntax colorer?)
(define tagfont
  litchar)

(define (deftag tag-str)
  (toc-target2-element #f
                       (tagfont tag-str)
                       (list 'tei (list 'prefixable tag-str))
                       (tt tag-str)))

(define ((make-other-doc-tag mod-path) raw)
  (define tag-str
    (if (symbol? raw)
        (symbol->string raw)
        raw))
  (link-element #f
                (tagfont tag-str)
                (list 'tei (cons 'prefixable
                                 (doc-prefix mod-path (list tag-str))))))

(define tag
  (make-other-doc-tag #f))
  

(define (attr raw)
  (tt (if (symbol? raw)
          (symbol->string raw)
          raw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (make-background-labeled it label)
  ; Based on scribble/private/manual-vars
  #:with [(def
            [RBackgroundLabel
             (style "RBackgroundLabel"
                    (list 'decorative 'command (alt-tag "div")
                          (attributes '((class . "SIEHidden")))))]
            [RBackgroundLabelInner
             (style "RBackgroundLabelInner" (list (alt-tag "div")))]
            [RForeground
             (style #f (list (attributes '((class . "RForeground")))))]
            [omitable
             (style #f '('omitable))]
            [noop-style (style #f null)])]
  (nested-flow
   noop-style
   (list
    (nested-flow RBackgroundLabel
                 (list (nested-flow RBackgroundLabelInner
                                    (list (paragraph omitable label)))))
    (paragraph RForeground it))))

(define (make-header-row sym)
  (list (make-background-labeled (deftag (symbol->string sym))
                                 "TEI element")))

(define empty-line
  ; Based on scribble/private/manual-utils
  (list (paragraph (style #f '(omitable))
                   (element #f (list (element 'tt '(nbsp)))))))

(define (make-contract-line lhs rhs)
  (list (tabular
         #:style (style "argcontract" '())
         (list (list (hspace 2)
                     lhs
                     (hspace 1)
                     (elem ":")
                     (hspace 1)
                     rhs)))))

(define (make-attributes-rows maybe-l-sym+ctc maybe-l-required)
  (list-when maybe-l-sym+ctc
    (append (list empty-line
                  (list (elem "Attributes:")))
            (for/list ([sym+ctc (in-list maybe-l-sym+ctc)])
              (match-define (cons sym ctc)
                sym+ctc)
              (make-contract-line (attr (symbol->string sym))
                                  ctc))
            (list-when maybe-l-required
              (list (list (elem "Required attributes:"))
                    (list (cons (hspace 2)
                                (match maybe-l-required
                                  [(list sym)
                                   (list (attr sym))]
                                  [(list a b)
                                   (list (attr a)
                                         (elem " and ")
                                         (attr b))]
                                  [many
                                   (add-between (map attr many)
                                                (elem ", ")
                                                #:before-last
                                                (elem ", and "))]))))))))

(define (make-children-rows maybe-assocs maybe-l-required)
  (define (repeat->typeset r)
    (case r
      [(1) ƒracket[1]]
      [(1+) ƒracket['1+]]
      [(0-1) ƒracket['0-1]]
      [(0+) ƒracket['0+]]))
  (list-when maybe-assocs
    (append (list empty-line
                  (list (elem "Children:"))
                  (list (tabular
                         #:column-properties '(left right left left)
                         (map (match-lambda
                                [(cons sym rep)
                                 (list (hspace 2) 
                                       (repeat->typeset rep)
                                       (hspace 1)
                                       (tag (symbol->string sym)))])
                              maybe-assocs))))
            (list-when maybe-l-required
              (list (list (elem "Required order:"))
                    (list (cons (hspace 2)
                                (add-between
                                 (for/list ([sym (in-list maybe-l-required)])
                                   (define base (tag (symbol->string sym)))
                                   (case (cdr (assq sym maybe-assocs))
                                     [(1) base]
                                     [(1+) (kleeneplus base)]
                                     [(0-1) (optional base)]
                                     [(0+) (kleenestar base)]))
                                 (elem ", ")))))))))


(define (make-has-text-rows text?)
  (list-when text?
    (list empty-line
          (list (elem "This element may contain free-form text.")))))


(define (make-element-rows #:name name
                           #:children [children-assocs #f]
                           #:required-order [required-order #f]
                           #:attr-contracts [attr-contracts #f]
                           #:required-attrs [required-attrs #f]
                           #:text? [text? #f])
  (cons (make-header-row name)
        (append (make-attributes-rows attr-contracts required-attrs)
                (make-children-rows children-assocs required-order)
                (make-has-text-rows text?))))


(define (make-boxed-table rows)
  (nested-flow
   (style 'vertical-inset null)
   (list (tabular
          #:style (style 'boxed (list (attributes '([class . "RBoxed"]))))
          rows))))

(define (make-together-table rows)
  (tabular
   #:style (style "together" '())
   rows))


(define (make-defelement-flow
         #:elements [listof-listsof-rows null]
         #:inset? [inset? #f]
         . body)
  (let ([listof-listsof-rows
         (let loop ([listof-listsof-rows listof-listsof-rows])
           (match listof-listsof-rows
             [(list _)
              listof-listsof-rows]
             [(cons this listof-listsof-rows)
              (cons (append this (list empty-line))
                    (loop listof-listsof-rows))]))])
    (apply nested
           #:style (and inset? 'inset)
           (make-boxed-table
            (match listof-listsof-rows
              [(list single-element-rows)
               single-element-rows]
              [multi
               (map (λ (rows)
                      (list (make-together-table rows)))
                    multi)]))
           body)))
   

 

