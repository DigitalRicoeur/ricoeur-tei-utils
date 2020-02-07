#lang racket/base

(require adjutor
         (only-in adjutor/unstable multi))

(require-provide (multi "toolkit"
                        ("interfaces.rkt"
                         "proto-frame.rkt"
                         "make-doc-class.rkt"
                         "error.rkt"
                         "status-dot.rkt"
                         "menu.rkt"
                         )))
