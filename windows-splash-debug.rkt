#lang racket/gui

(require ricoeur/tei/tools/tei-lint/lib
         )

(define splash%
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
           [label "Digital Ricœur"])
      (new message%
           [parent col]
           [label subtitle])
      (new editor-message%
           [parent col]
           [content message])
      (on-initialize-col col))
    (define/public (on-initialize-col col)
      (void))
    (show #t)
    #|END splash%|#))




(define f
  (new splash%
       [label "Label"]
       [subtitle "Subtitle"]
       [message "Message"]
       [bitmap "Bitmap"]))

