#lang at-exp racket/base

(require adjutor
         scribble/manual)

(require-provide (provide-only adjutor)
                 (except-out (submod ricoeur/tei/spec-lang doc)
                             DR-TEI_doc.html)
                 scribble/core
                 scribble/html-properties
                 (for-label ricoeur/tei
                            (except-in racket
                                       date
                                       date?)))
         
(provide DR-TEI-link
         lib-tech)

(define DR-TEI-link
  @elem[#:style (style #f (list (link-resource DR-TEI_doc.html)))]{DR-TEI_doc.html})

(define (lib-tech . args)
  (TODO/void lib-tech: avoid duplicating in teiHeader.rkt)
  (apply tech #:doc '(lib "ricoeur/tei/scribblings/tei-utils/ricoeur-tei-utils.scrbl") args))

