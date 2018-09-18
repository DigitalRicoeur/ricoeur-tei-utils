#!/usr/bin/env racket
#lang racket/base

(require ricoeur/tei/base
         racket/cmdline)

(provide guess-paragraphs-command)

(module+ main
  (guess-paragraphs-command))

(define (guess-paragraphs-command [argv (current-command-line-arguments)])
  (define mode #f)
  (command-line
   #:program "raco ricoeur/tei guess-paragraphs"
   #:argv argv
   #:usage-help
   "(Deprecated in favor of \"TEI Lint\".)"
   "Replaces <path> with an equivalent TEI XML file,"
   "  but with paragraphs inferred accoding to <option>."
   "Either \"--line-breaks\" or \"--blank-lines\" must be given."
   #:once-any
   ["--line-breaks" "Interpret each line as a paragraph"
                    (set! mode 'line-breaks)]
   ["--blank-lines" "Interpret paragraphs as separated by blank lines"
                    (set! mode 'blank-lines)]
   #:args (path)
   (unless mode
     (error "either \"--line-breaks\" or \"--blank-lines\" must be given"))
   (define doc
     (file->tei-document path))
   (with-output-to-file/unless-exn path
     #:exists 'replace
     #:mode 'text
     (λ ()
       (write-tei-document
        (tei-document-guess-paragraphs doc #:mode mode))))))
