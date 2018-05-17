#lang info

(define collection "ricoeur")
(define pkg-desc "TEI utilities for Digital Ricoeur")
(define version "0.4")
(define pkg-authors '(philip))
;; Executables to build are listed in "tei/tools/info.rkt"
;; Dependencies:
(define deps '(("base" #:version "6.12")
               ("adjutor" #:version "0.2")
               "functional-lib"
               "roman-numeral"
               "gregor-lib"
               "gui-lib"
               "pict-lib"
               "scribble-lib"
               "data-lib"
               "db-lib"
               "draw-lib"
               "parser-tools-lib"
               "pict-snip-lib"
               ("xmllint-win32-x86_64" #:platform "win32\\x86_64"
                                       #:version "0.1") 
               ))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "at-exp-lib"
                     "functional-doc"
                     "gregor-doc"
                     "rackunit-lib"
                     ("_-exp" #:version "0.1")
                     "db-doc"
                     ))
;; Documentation:
(define scribblings '(("tei/scribblings/tei-utils/tei-utils.scrbl"
                       (multi-page)
                       ("Digital Ricœur" 0)
                       )
                      ("tei/scribblings/guidelines/guidelines.scrbl"
                       (multi-page)
                       ("Digital Ricœur" 0)
                       )))

