#lang info

(define pkg-name "ricoeur-tei-utils")
(define collection "ricoeur")
(define pkg-desc "TEI utilities for Digital Ricoeur")
(define version "0.5.9")
(define pkg-authors '(philip))

;; Executables to build are listed in "tei/tools/info.rkt".


;; Documentation to build had been listed in "tei/info.rkt"
;; so that `raco setup --doc-index ricoeur/tei` would work.
;; Sould reconsider ...
(define scribblings
  '(("scribblings/tei-utils/ricoeur-tei-utils.scrbl"
     (multi-page)
     ("Digital Ricœur" 0)
     )
    ("scribblings/guidelines/ricoeur-tei-guidelines.scrbl"
     (multi-page)
     ("Digital Ricœur" 0)
     )))


;; Dependencies:
(define deps
  '(["base" #:version "7.4"]
    ["adjutor" #:version "0.2.5"]
    ["ricoeur-kernel" #:version "0.0.1"]
    "functional-lib"
    "roman-numeral"
    "gregor-lib"
    "gui-lib"
    "pict-lib"
    "scribble-lib"
    "data-lib"
    ["db-lib" #:version "1.4"]
    ["sql" #:version "1.5"]
    "draw-lib"
    "icns"
    "parser-tools-lib"
    "pict-snip-lib"
    "typed-racket-lib"
    ["xmllint-win32-x86_64" #:platform "win32\\x86_64"
                            #:version "0.1"]
    ;; The following are for the documentation time
    ;; of #lang ricoeur/tei/spec-lang.
    ;; Can they be made build-deps somehow?
    "at-exp-lib"
    "syntax-color-lib"))

(define build-deps
  '("scribble-lib"
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
    "racket-index"))

