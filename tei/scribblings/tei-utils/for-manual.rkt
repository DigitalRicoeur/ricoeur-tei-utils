#lang at-exp racket

(require scribble/manual
         (except-in (submod ricoeur/tei/kernel doc)
                    tag)
         )

(require-provide (provide-only (submod ricoeur/tei/kernel doc))
                 scribble/core
                 syntax/parse/define
                 (for-syntax racket/base
                             syntax/parse
                             )
                 (for-label (except-in racket
                                       tag
                                       field
                                       get-field
                                       date
                                       date?)
                            ricoeur/tei
                            ricoeur/tei/base
                            ricoeur/tei/kernel
                            xml
                            db
                            json
                            setup/matching-platform
                            ))

(provide guidelines-doc
         guidelines-secref
         Ricoeur
         defpredicate
         submodlink
         (contract-out
          [tag
           (-> (or/c symbol? string?) element?)]
          ))

(define Ricoeur
  "Ricœur")

(define guidelines
  '(lib "ricoeur/tei/scribblings/guidelines/guidelines.scrbl"))

(define (guidelines-doc)
  @other-doc[guidelines])

(define (guidelines-secref sec)
  @secref[sec #:doc guidelines])

(define tag
  (make-other-doc-tag guidelines))



(define-syntax-parser defpredicate
  [(_ name:id v:id body:expr ...)
   #'(defproc (name [v any/c]) any/c body ...)]
  [(_ name:id body:expr ...)
   #'(defpredicate name v body ...)])


(define-syntax-parser submodlink
  [(_ m:expr)
   #'(racketmodlink m (racket m))])


