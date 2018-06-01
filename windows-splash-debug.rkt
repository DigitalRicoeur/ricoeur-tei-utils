#lang racket/gui

(require ricoeur/tei/tools/tei-lint/lib
         ricoeur/tei/tools/tei-lint/splash
         )

(define splash-frame%
  (class abstract-splash-frame%
    (super-new [label "TEI Lint"]
               [subtitle "TEI Lint"]
               [bitmap photo-bitmap]
               [height (floor (* 5/4 (send photo-bitmap get-height)))])
    (inherit show)
    (define/override-final (on-choose-directory dir)
      (show #f)
      #;(new directory-frame% [dir dir]))))




(void (new splash-frame%))

