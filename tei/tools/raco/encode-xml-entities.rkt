#!/usr/bin/env racket
#lang racket/base

(require xml
         racket/cmdline
         racket/file)

(provide encode-xml-entities-command)

(module+ main
  (encode-xml-entities-command))

(define (encode-xml-entities-command [argv (current-command-line-arguments)])
  (command-line
   #:program "raco ricoeur/tei encode-xml-entities"
   #:argv argv
   #:usage-help
   "(Deprecated in favor of \"TEI Lint\".)"
   "For each path in <paths>, replaces the characters & and <"
   "  with the corresponsing XML entities, overwriting the original file."
   #:args paths
   (for ([pth (in-list paths)]
         #:unless (and (path-string? pth)
                       (file-exists? pth)))
     (error 'encode-entities
            (string-append (if (path-string? pth)
                               "file does not exist or is a directory"
                               "not a valid path")
                           (format "\n  given: ~e" pth))))
   (for ([pth (in-list paths)])
     (let ([raw (file->string pth #:mode 'text)])
       (with-output-to-file pth
         #:exists 'replace
         #:mode 'text
         (λ () (write-xexpr raw)))))))
