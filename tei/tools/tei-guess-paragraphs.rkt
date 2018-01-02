#!/usr/bin/env racket
#lang racket/base

(module+ main
  (require ricoeur/tei/base
           ricoeur/lib
           racket/class
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
     (define obj
       (call-with-input-file path
         read-TEI))
     (with-output-to-file/unless-exn path
       #:exists 'replace
       (λ ()
         (send (send obj guess-paragraphs #:mode mode)
               write-TEI))))))


     