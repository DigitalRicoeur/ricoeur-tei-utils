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
                 scribble/html-properties
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
                            setup/matching-platform))

(provide guidelines-doc
         guidelines-secref
         Ricoeur
         tei-utils-version
         min-racket-version
         defpredicate
         submodlink
         make-tei-eval
         style:scale-down-to-fit
         (rename-out [tei-utils:title title])
         (contract-out
          [tag
           (-> (or/c symbol? string?) element?)]
          ))

(define Ricoeur
  "Ricœur")

(define guidelines
  '(lib "ricoeur/tei/scribblings/guidelines/ricoeur-tei-guidelines.scrbl"))

(define (guidelines-doc)
  @other-doc[guidelines])

(define (guidelines-secref sec)
  @secref[sec #:doc guidelines])

(define tag
  (make-other-doc-tag guidelines))

(define info-ref
  (get-info/full (pkg-directory "ricoeur-tei-utils")))

(define min-racket-version
  (cadr (memq '#:version (assoc "base" (info-ref 'deps)))))

(define tei-utils-version
  (info-ref 'version (λ () "")))

(define tei-utils:title
  (curry title #:version tei-utils-version))

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

(define style:scale-down-to-fit
  (style "RicoeurScaleToFit"
         (list (css-addition
                (string->bytes/utf-8 @~a{
 .RicoeurScaleToFit {
  width: auto;
  height: auto;
  max-width: 100%;
  }})))))

