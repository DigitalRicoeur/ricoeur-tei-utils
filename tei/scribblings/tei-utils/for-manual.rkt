#lang at-exp racket

(require scribble/manual
         setup/getinfo
         pkg/lib
         (except-in (submod ricoeur/tei/kernel doc)
                    DR-TEI_doc.html
                    tag))

(require-provide (provide-only (submod ricoeur/tei/kernel doc))
                 scribble/core
                 scribble/example
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
                            xml
                            db
                            json
                            setup/matching-platform
                            ))

(provide guidelines-doc
         guidelines-secref
         Ricoeur
         tei-utils-version
         defpredicate
         submodlink
         make-tei-eval
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


(define tei-utils-version
  (let ([dir (pkg-directory "ricoeur-tei-utils")])
    (or (and dir
             (let ([ref (get-info/full dir)])
               (and ref
                    (ref 'version (λ () #f)))))
        "")))


(define-syntax-parser defpredicate
  [(_ name:id v:id body:expr ...)
   #'(defproc (name [v any/c]) any/c body ...)]
  [(_ name:id body:expr ...)
   #'(defpredicate name v body ...)])


(define-syntax-parser submodlink
  [(_ m:expr)
   #'(racketmodlink m (racket m))])


(define make-tei-eval
  (make-eval-factory '(ricoeur/tei
                       racket/contract
                       racket/match
                       racket/set
                       racket/class
                       racket/unit)))


