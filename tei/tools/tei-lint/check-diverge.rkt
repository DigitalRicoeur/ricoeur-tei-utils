#lang racket/gui

(require ricoeur/tei/base
         (submod ricoeur/tei/search/common private)
         "lib.rkt"
         )

(provide check-diverge)

(define suspicious-seconds
  3)

(define (check-diverge doc progress-frame)
  ;; Returns #f if (prepare-pre-segments doc) does not diverge
  ;; or else the number of seconds after which we gave up
  (if (doc-quickly-succeeds? doc)
      #f
      (send (new diverge-notice%
                 [doc doc]
                 [parent progress-frame])
            get-diverge-seconds)))

(define (doc-quickly-succeeds? doc)
  (let ([cust (make-custodian)])
    (define th
      (parameterize ([current-custodian cust])
        (thread (λ () (prepare-pre-segments doc)))))
    (define rslt
      (sync/timeout suspicious-seconds th))
    (custodian-shutdown-all cust)
    rslt))
              
(define diverge-notice%
  (class dialog%
    (init-field doc)
    (super-new [label "Warning! - TEI Lint"]
               [alignment '(left top)])
    (inherit show)
    (let ([row (new horizontal-pane%
                    [parent this]
                    [alignment '(left center)])])
      (new message%
           [parent row]
           [label 'caution])
      (new message%
           [parent row]
           [label "Warning!"]
           [font big-bold-system-font]))
    (let ([col (new vertical-pane%
                    [parent this]
                    [alignment '(left top)])])
      (insert-message-row col
                          "Title: "
                          (send doc get-title))
      (insert-message-row col
                          "Path: "
                          (let ([pth (send doc get-full-path)])
                            (cond
                              [(string? pth)
                               pth]
                              [(path? pth)
                               (path->string pth)]
                              [else
                               "<unknown>"]))))
    (let ([para (new text% [auto-wrap #t])])
      (new editor-canvas%
           [parent this]
           [style '(transparent no-border no-hscroll auto-vscroll no-focus)]
           [min-height 150]
           [editor para])
      (send para
            insert
            "Attempting to split this document into segments for search is taking a suspiciously long time.\n\n")
      (send para
            insert
            "It may be affected by a bug which can cause the segmentation function to run forever.\n\n")
      (send para
            insert
            "Please try running it now, and be prepared to give up if it seems to be running forever.")
      (scroll-editor-to-top para)
      (send para lock #t))
    (new message%
         [label "This window will close automatically if the function finishes."]
         [parent this])
    (define ch
      (make-channel))
    (define main
      (new vertical-pane%
           [alignment '(center center)]
           [parent this]))
    (new button%
         [parent main]
         [label "Start"]
         [callback (λ (b e) (start))])
    (show #t)
    (define/public-final (get-diverge-seconds)
      (sync ch))
    (define/augment-final (can-close?)
      #f)
    (define/override-final (can-exit?)
      #t)
    (define/private (start)
      (send main change-children (λ (_) '()))
      (define cust
        (make-custodian))
      (define seconds-counter
        (new seconds-counter%
             [parent main]))
      (new button%
           [parent main]
           [label "Give Up"]
           [callback (λ (b e)
                       (custodian-shutdown-all cust)
                       (show #f)
                       (thread (λ () 
                                 (channel-put ch (send seconds-counter get-seconds)))))])
      (parameterize ([current-custodian cust])
        (define update-th
          (thread (λ ()
                    (let loop ()
                      (send seconds-counter update)
                      (sleep 0.5)
                      (loop)))))
        (thread (λ ()
                  (prepare-pre-segments doc)
                  (show #f)
                  (kill-thread update-th)
                  (thread (λ () 
                            (channel-put ch #f)))))))
    #|END diverge-notice%|#))


(define seconds-counter%
  (class horizontal-pane%
    (super-new [alignment '(left top)])
    (define init-ms
      (current-inexact-milliseconds))
    (define latest-ms
      init-ms)
    (new message%
         [label "Elapsed Time (Seconds): "]
         [parent this])
    (define msg
      (new message%
           [label (get-label)]
           [font bold-system-font]
           [auto-resize #t]
           [parent this]))
    (define/public-final (get-seconds)
      (/ (- latest-ms init-ms)
         1000))
    (define/private (get-label)
      (real->decimal-string (get-seconds)))
    (define/public-final (update)
      (set! latest-ms (current-inexact-milliseconds))
      (send msg set-label (get-label)))))




