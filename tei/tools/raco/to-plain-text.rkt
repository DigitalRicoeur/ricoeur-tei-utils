#!/usr/bin/env racket
#lang racket/base

(require ricoeur/tei/oop-base
         racket/class
         racket/cmdline)

(provide to-plain-text-command)

;; Would be nice to avoid Racket startup costs for each file ...

(define (to-plain-text-command [argv (current-command-line-arguments)])
  (command-line
   #:program "raco tei to-plain-text"
   #:argv argv
   #:usage-help "Writes the given TEI XML file to STDOUT as plain text"
   #:args (TEI-XML-path)
   (void (write-string
          (send (file->TEI TEI-XML-path)
                to-plain-text)))))

