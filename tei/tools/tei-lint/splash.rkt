#lang racket/gui

(require ricoeur/tei
         racket/runtime-path
         data/maybe
         xml/path
         gregor
         adjutor
         pict
         "lib.rkt"
         )

(provide abstract-splash-frame%)

(define abstract-splash-frame%
  (class frame%
    (super-new)
    (init subtitle bitmap)
    (inherit show)
    (abstract on-choose-directory)
    (let ([row (new horizontal-pane% [parent this])])
      (new message%
           [parent row]
           [label bitmap])
      (define col
        (new vertical-pane% [parent row]))
      (new message%
           [parent col]
           [label "Digital Ricœur"]
           [font (make-font #:family 'system
                            #:size 24)])
      (new message%
           [parent col]
           [label subtitle]
           [font (make-font #:family 'system
                            #:size 36
                            #:weight 'bold)])
      (define e-c
        (new editor-canvas%
             [parent col]
             [style '(transparent no-border no-hscroll auto-vscroll no-focus)]))
      (define para
        (new text% [auto-wrap #t]))
      (send para insert "To begin, choose a directory containing TEI XML files.")
      (send para lock #t)
      (send e-c set-editor para)
      (new button%
           [parent col]
           [label "Choose Directory …"]
           [callback (λ (b e) (choose-directory))]))
    (show #t)
    (unless (xmllint-available?)
      (show-xmllint-warning this))
    (define/private (choose-directory)
      (let ([dir (get-xml-directory this)])
        (when dir
          (on-choose-directory dir))))))


