#lang racket/gui

(require ricoeur/tei/oop
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
      (new editor-message%
           [parent col]
           [content message])
      (on-initialize-col col))
    (define/public (on-initialize-col col)
      (void))
    (show #t)
    #|END abstract-splash-frame/no-button%|#))




(define abstract-splash-frame%
  (class splash-frame/no-button%
    (init 
     [message "To begin, choose a directory containing TEI XML files."])
    (super-new 
     [message message])
    (unless (xmllint-available?)
      (show-xmllint-warning this))
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





