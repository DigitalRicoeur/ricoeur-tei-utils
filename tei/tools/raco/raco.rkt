#!/usr/bin/env racket
#lang racket/base

(module+ main
  (require racket/cmdline
           racket/lazy-require
           )

  (lazy-require
   ["to-plain-text.rkt" (to-plain-text-command)]
   ["validate-directory.rkt" (validate-directory-command)]
   ["encode-xml-entities.rkt" (encode-xml-entities-command)]
   ["guess-paragraphs.rkt" (guess-paragraphs-command)]
   [(submod "directory-clean-filenames.rkt" cmd)
    (directory-clean-filenames-command)])

  (command-line
   #:program "raco ricoeur/tei"
   #:usage-help
   "Available Commands:"
   "  to-plain-text"
   "  validate-directory"
   "  directory-clean-filenames"
   "  encode-xml-entities"
   "  guess-paragraphs"
   #:args (command . option/arg)
   (case command
     [("to-plain-text")
      (to-plain-text-command option/arg)]
     [("validate-directory")
      (validate-directory-command option/arg)]
     [("directory-clean-filenames")
      (directory-clean-filenames-command option/arg)]
     [("encode-xml-entities")
      (encode-xml-entities-command option/arg)]
     [("guess-paragraphs")
      (guess-paragraphs-command option/arg)]
     [else
      (error '|raco ricoeur/tei|
             "unknown command: ~e"
             command)])))


