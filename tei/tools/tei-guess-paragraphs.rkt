#!/usr/bin/env racket
#lang racket/base

(module+ main
  (require ricoeur/tei/base
           racket/cmdline
           )
  
  (let ([mode #f])
    (command-line
     #:usage-help
     "Replaces <path> with an equivalent TEI XML file,"
     "but with paragraphs inferred accoding to <option>."
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
          (tei-document-guess-paragraphs doc #:mode mode)))))))


     