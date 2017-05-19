#lang info

(define collection "ricoeur")
(define deps '("base"
               "adjutor"
               ))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     ))
(define pkg-desc "TEI utilities for Digital Ricoeur")
(define version "0.0")
(define pkg-authors '(philip))
(define scribblings '(("tei/scribblings/tei-utils.scrbl"
                       ()
                       ("Digital Ricœur" 0)
                       )))
