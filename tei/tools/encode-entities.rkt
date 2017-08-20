#!/usr/bin/env racket
#lang racket/base

(require xml
         racket/cmdline
         racket/file
         )

(module+ main
  (command-line
   #:usage-help
   "for each path in <paths>, replaces the characters & and <"
   "with the corresponsing XML entities, overwriting the original file"
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
     (let ([raw (file->string pth)])
       (with-output-to-file pth
         #:exists 'replace
         (λ () (write-xexpr raw)))))))
