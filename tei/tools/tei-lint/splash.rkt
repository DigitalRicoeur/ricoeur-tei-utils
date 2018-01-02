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

(provide abstract-splash-frame%
         splash-frame/no-button%
         )

(define splash-frame/no-button%
  (class frame%
    (super-new)
    (init subtitle bitmap message)
    (inherit show)
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
      (send para insert message)
      (scroll-editor-to-top para)
      (send para lock #t)
      (send e-c set-editor para)
      (on-initialize-col col))
    (define/public (on-initialize-col col)
      (void))
    (show #t)
    #|END abstract-splash-frame/no-button%|#))




(define abstract-splash-frame%
  (class splash-frame/no-button%
    (unless (xmllint-available?)
      (show-xmllint-warning this))
    (super-new 
     [message "To begin, choose a directory containing TEI XML files."])    
    (abstract on-choose-directory)
    (define/override (on-initialize-col col)
      (new button%
           [parent col]
           [label "Choose Directory …"]
           [callback (λ (b e) (choose-directory))]))
    (define/private (choose-directory)
      (let ([dir (get-xml-directory this)])
        (when dir
          (on-choose-directory dir))))))





