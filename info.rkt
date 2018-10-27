#lang info

(define collection "ricoeur")
(define pkg-desc "TEI utilities for Digital Ricoeur")
(define version "0.5.4")
(define pkg-authors '(philip))

;; Executables to build are listed in "tei/tools/info.rkt".
;; Documentation to build is listed in "tei/info.rkt".

;; Dependencies:
(define deps '(("base" #:version "7.1")
               ("adjutor" #:version "0.2.5")
               "functional-lib"
               "roman-numeral"
               "gregor-lib"
               "gui-lib"
               "pict-lib"
               "scribble-lib"
               "data-lib"
               "db-lib"
               ("sql" #:version "1.3")
               "draw-lib"
               "icns"
               "parser-tools-lib"
               "pict-snip-lib"
               "typed-racket-lib"
               ("xmllint-win32-x86_64" #:platform "win32\\x86_64"
                                       #:version "0.1")
               ;; The following are for the documentation time
               ;; of #lang ricoeur/tei/kernel.
               ;; Can they be made build-deps somehow?
               "at-exp-lib"
               "syntax-color-lib"
               ))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "at-exp-lib"
                     "functional-doc"
                     "gregor-doc"
                     "rackunit-lib"
                     ("_-exp" #:version "0.1")
                     "db-doc"
                     "data-doc"
                     "gui-doc"
                     "scribble-doc"
                     "todo-list"
                     "racket-index"
                     ))

