#lang racket/gui

(require ricoeur/tei/base
         (submod ricoeur/tei/search/common private)
         "lib.rkt"
         )

(provide (contract-out
          [diverges?
           (-> (is-a?/c TEI<%>)
               (or/c (is-a?/c frame%)
                     (is-a?/c dialog%)
                     #f)
               (or/c #f (>=/c 0)))]
          ))

(define suspicious-seconds
  3)

(define (diverges? doc progress-frame)
  ;; Returns #f if (prepare-pre-segments doc) does not diverge
  ;; or else the number of seconds after which we gave up
  (define cust
    (make-custodian))
  (define seconds-counter
    (new seconds-counter%))
  (define maybe-notice
    #f)
  (define worker-th
    (parameterize ([current-custodian cust])
      (define update-th
        (thread (λ ()
                  (let loop ()
                    (send seconds-counter update)
                    (sleep 0.5)
                    (loop)))))
      (thread (λ ()
                (prepare-pre-segments doc)
                (when maybe-notice
                  (send maybe-notice show #f))
                (kill-thread update-th)))))
  (define quickly-succeeds?
    (sync/timeout suspicious-seconds worker-th))
  (cond
    [quickly-succeeds?
     (custodian-shutdown-all cust)
     #f]
    [else
     (set! maybe-notice
           (new diverge-notice%
                [doc doc]
                [worker-th worker-th]
                [cust cust]
                [seconds-counter seconds-counter]
                [parent progress-frame]))
     (send* maybe-notice
       (show #t)
       (get-diverge-seconds))]))


              
(define diverge-notice%
  ;; Must be explicitly told to show
  (class dialog%
    (init-field doc
                worker-th
                cust
                seconds-counter)
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
      (let ([pth (send doc get-full-path)])
        (if (path-string? pth)
            (insert-message-row col
                                "Path: "
                                #:right-message% path-message%
                                (cond
                                  [(string? pth)
                                   pth]
                                  [(path? pth)
                                   (path->string pth)]))
            (insert-message-row col
                                "Path: "
                                "<unknown>"))))
    (new editor-message%
         [parent this]
         [content '("Attempting to split this document into segments"
                    " for search is taking a suspiciously long time.\n\n"
                    "It may be affected by a bug which can cause the"
                    " segmentation function to run forever.\n\n"
                    "Please try running it now, and be prepared"
                    " to give up if it seems to be running forever.\n\n"
                    "This window will close automatically if"
                    " the function finishes."
                    )])
    (define ch
      (make-channel))
    (define main
      (new vertical-pane%
           [alignment '(center center)]
           [parent this]))
    (send seconds-counter
          reparent
          main)
    (new button%
         [parent main]
         [label "Give Up"]
         [callback (λ (b e)
                     (custodian-shutdown-all cust)
                     (show #f)
                     (thread (λ () 
                               (channel-put ch (send seconds-counter
                                                     get-seconds)))))])
    (parameterize ([current-custodian cust])
      (thread (λ ()
                (sync worker-th)
                (thread (λ () 
                          (channel-put ch #f))))))
    (define/public-final (get-diverge-seconds)
      (sync ch))
    (define/augment-final (can-close?)
      #f)
    (define/override-final (can-exit?)
      #t)
    #|END diverge-notice%|#))


(define seconds-counter%
  (class object%
    (super-new)
    (init [parent #f])
    (define init-ms
      (current-inexact-milliseconds))
    (define latest-ms
      init-ms)
    (define msg
      (make-msg parent))
    (define/public-final (get-seconds)
      (/ (- latest-ms init-ms)
         1000))
    (define/public-final (update)
      (set! latest-ms (current-inexact-milliseconds))
      (when msg
        (send msg set-label (get-label))))
    (define/public-final (reparent new-parent)
      (cond
        [(and msg new-parent)
         (send+ msg
                (get-parent)
                (reparent new-parent))]
        [new-parent
         (set! msg (make-msg new-parent))]
        [msg
         (define panel
           (send msg get-parent))
         (send+ panel
                (get-parent)
                (delete-child panel))
         (set! msg #f)]
        [else
         (void)]))
    (define/private (get-label)
      (real->decimal-string (get-seconds)))
    (define/private (make-msg parent)
      (and parent
           (let ([container (new horizontal-panel%
                                 [alignment '(left top)]
                                 [parent parent])])
             (new message%
                  [label "Elapsed Time (Seconds): "]
                  [parent container])
             (new message%
                  [label (get-label)]
                  [font bold-system-font]
                  [auto-resize #t]
                  [parent container]))))))

