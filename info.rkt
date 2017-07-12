#lang info

(define collection "ricoeur")
(define deps '("base"
               "adjutor"
               "functional-lib"
               "roman-numeral"
               "gregor-lib"
    "gui-lib"
    "pict-lib"
  "scribble-lib"
               ))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "at-exp-lib"
                     "functional-doc"
                     "gregor-doc"
                     "rackunit-lib"
                     ))
(define pkg-desc "TEI utilities for Digital Ricoeur")
(define version "0.0")
(define pkg-authors '(philip))
(define scribblings '(("tei/scribblings/tei-utils.scrbl"
                       ()
                       ("Digital Ricœur" 0)
                       )
                      ("tei/scribblings/guidelines.scrbl"
                       ()
                       ("Digital Ricœur" 0)
                       )))
(define raco-commands '(("tei-to-plain-text"
                         (submod ricoeur/tei/tools/tei-to-plain-text main)
                         "convert TEI XML file to plain text"
                         #f)))
(define gracket-launcher-names (list "TEI Lint"))
(define gracket-launcher-libraries (list "tei/tei-lint/tei-lint.rkt"))
