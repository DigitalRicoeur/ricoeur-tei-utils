#lang racket/base

(module+ main
  (require ricoeur/tei
           racket/class
           racket/cmdline)
  (command-line
   #:usage-help "writes the given TEI XML file to STDOUT as plain text"
   #:args (TEI-XML-path)
   (void (write-string
          (send (call-with-input-file TEI-XML-path
                  read-TEI)
                to-plain-text)))))



