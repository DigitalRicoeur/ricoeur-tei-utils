#lang racket/base

(require adjutor
         racket/contract
         )

(require-provide ricoeur/tei/tei-xexpr-contracts
                 ricoeur/tei/classes
                 ricoeur/tei/xmllint
                 )

(provide make-ricoeur-teiHeader
         )

(define/contract (make-ricoeur-teiHeader
                  #:title title
                  #:authors [authors '((author "Paul Ricoeur"))]
                  #:editors [editors null]
                  . bibl-body)
  (->* (string?
        #:title string?)
       (#:authors (non-empty-listof (tei-xexpr/c 'author))
        #:editors (listof (tei-xexpr/c 'editor)))
       #:rest (listof string?)
       (tei-xexpr/c 'teiHeader))
  `(teiHeader
    (fileDesc
     (titleStmt (title ,title)
                ,@authors
                ,@editors)
     (publicationStmt (authority "Digital Ricoeur")
                      (availability ([status "restricted"])
                                    (p "Not for distribution.")))
     (sourceDesc (bibl ,@bibl-body)))))
