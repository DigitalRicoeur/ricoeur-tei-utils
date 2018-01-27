#lang info

(define collection "ricoeur")
(define deps '(("base" #:version "6.11")
               "adjutor"
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
(define pkg-desc "TEI utilities for Digital Ricoeur")
(define version "0.2.1")
(define pkg-authors '(philip))
(define scribblings '(("tei/scribblings/tei-utils.scrbl"
                       (multi-page)
                       ("Digital Ricœur" 0)
                       )
                      ("tei/scribblings/guidelines.scrbl"
                       (multi-page)
                       ("Digital Ricœur" 0)
                       )))
(define raco-commands '(("tei"
                         (submod ricoeur/tei/tools/raco-tei main)
                         "run Digital Ricoeur TEI commands"
                         #f)))
(define gracket-launcher-names
  '("TEI Lint"
    ;"DR Migration Assistant" ; now managed by tei/tools/migration/info.rkt
    ))
(define gracket-launcher-libraries
  '("tei/tools/tei-lint/tei-lint.rkt"
    ;"tei/tools/migration/migration-assistant.rkt"
    ))
(define racket-launcher-names
  (list "tei-guess-paragraphs"
        "encode-xml-entities"))
(define racket-launcher-libraries
  (list "tei/tools/tei-guess-paragraphs.rkt"
        "tei/tools/encode-entities.rkt"))
