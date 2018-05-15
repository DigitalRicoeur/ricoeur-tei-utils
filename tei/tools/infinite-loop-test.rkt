#lang racket/base

(require ricoeur/tei/oop-base
         (submod ricoeur/tei/search/common private)
         racket/class
         racket/cmdline
         )
#|
(define (infinite-loop-test-command [argv (current-command-line-arguments)])
  (command-line
   #:program "raco tei infinite-loop-test"
   #:argv argv
   "For any XML files in <dir>, which defaults to the current directory,"
   "  converts them to TEI<%> objects and tries to run"
   "  prepare-pre-segments on them, printing progress to STDOUT."
   "This helps to detect files that may cause prepare-pre-segments to"
   "  diverge due to a bug."
   #:args ([dir (current-directory)])
|#

(define (infinite-loop-test [dir (current-directory)])
  (let* ([pths
          (for*/list ([pth (in-directory dir)]
                      #:when (xml-path? pth))
            pth)]
         [count (length pths)])
    (for ([pth (in-list pths)]
          [i (in-naturals 1)])
      (printf "File ~a/~a\n" i count)
      (define doc
        (file->TEI pth))
      (printf "Path: ~a\n" pth)
      (printf "Title: ~v\n" (send doc get-title))
      (printf "Starting: ~a\n" (current-seconds))
      (prepare-pre-segments doc)
      (printf "Finished: ~a\n\n\n" (current-seconds)))
    (displayln "All finished.")))

(infinite-loop-test "/Users/philip/code/ricoeur/texts/diverge")
