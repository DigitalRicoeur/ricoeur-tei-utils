#!/usr/bin/env racket
#lang racket/base

(module+ main
  (require racket/cmdline
           racket/lazy-require
           )

  (lazy-require
   ["tei-to-plain-text.rkt" (to-plain-text)]
   ["validate-directory.rkt" (validate-directory)]
   )

  (command-line
   #:program "raco tei"
   #:usage-help
   "Available Commands:"
   "  to-plain-text"
   "  validate-directory"
   #:args (command . option/arg)
   (case command
     [("to-plain-text")
      (to-plain-text option/arg)]
     [("validate-directory")
      (validate-directory option/arg)]
     [else
      (error '|raco tei|
             "unknown command: ~e"
             command)])))


