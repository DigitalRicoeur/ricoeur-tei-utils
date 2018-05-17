#!/usr/bin/env racket
#lang racket/base

(module+ main
  (require racket/cmdline
           racket/lazy-require
           )

  (lazy-require
   ["to-plain-text.rkt" (to-plain-text-command)]
   ["validate-directory.rkt" (validate-directory-command)]
   [(submod "directory-clean-filenames.rkt" cmd)
    (directory-clean-filenames-command)]
   )

  (command-line
   #:program "raco tei"
   #:usage-help
   "Available Commands:"
   "  to-plain-text"
   "  validate-directory"
   "  directory-clean-filenames"
   #:args (command . option/arg)
   (case command
     [("to-plain-text")
      (to-plain-text-command option/arg)]
     [("validate-directory")
      (validate-directory-command option/arg)]
     [("directory-clean-filenames")
      (directory-clean-filenames-command option/arg)]
     [else
      (error '|raco tei|
             "unknown command: ~e"
             command)])))


