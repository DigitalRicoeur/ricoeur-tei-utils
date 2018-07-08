#lang racket/gui

(require ricoeur/tei/base
         "lib.rkt"
         "splash.rkt"
         "directory-frame.rkt"
         )

(module+ main
  (void (new splash-frame%)))

(define splash-frame%
  (class abstract-splash-frame%
    (super-new [label "TEI Lint"]
               [subtitle "TEI Lint"]
               [bitmap photo-bitmap]
               [height (floor (* 5/4 (send photo-bitmap get-height)))])
    (inherit show)
    (define/override-final (on-choose-directory dir)
      (show #f)
      (make-directory-frame dir))))


