#lang racket/gui

(require ricoeur/tei/tools/tei-lint/lib
         ricoeur/tei/tools/tei-lint/splash
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
           #;
           [font (make-font #:family 'system
                            #:size 24)])
      (new message%
           [parent col]
           [label subtitle]
           #;
           [font (make-font #:family 'system
                            #:size 36
                            #:weight 'bold)])
      #;
      (new editor-message%
           [parent col]
           [content message])
      (on-initialize-col col))
    (define/public (on-initialize-col col)
      (void))
    (show #t)
    #|END abstract-splash-frame/no-button%|#))


(new splash-frame/no-button%
     [label "label"]
     [subtitle "subtitle"]
     [bitmap "bitmap"]
     [message "message"])



