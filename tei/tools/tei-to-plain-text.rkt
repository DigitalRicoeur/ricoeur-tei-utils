#!/usr/bin/env racket
#lang racket/base

(require ricoeur/tei/oop/objects
         racket/class
         racket/cmdline)

(provide to-plain-text)

(define (to-plain-text [argv (current-command-line-arguments)])
  (command-line
   #:program "raco tei to-plain-text"
   #:argv argv
   #:usage-help "Writes the given TEI XML file to STDOUT as plain text"
   #:args (TEI-XML-path)
   (void (write-string
          (send (file->TEI TEI-XML-path)
                to-plain-text)))))

(module+ main
  (to-plain-text))

