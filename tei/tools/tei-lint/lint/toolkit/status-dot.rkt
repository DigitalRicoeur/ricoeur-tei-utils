#lang racket/gui

(require adjutor
         (only-in "interfaces.rkt" lint-status/c)
         pict)

(provide STATUS-DOT-SIZE
         (contract-out
          [status-dot-pict
           (-> lint-status/c pict?)]
          [status-canvas%
           (class/c
            (init [status lint-status/c]))]
          ))

(def
  [STATUS_DOT_INNER_SIZE 20]
  [STATUS_DOT_BORDER 1]
  [STATUS-DOT-SIZE (+ STATUS_DOT_BORDER
                      STATUS_DOT_INNER_SIZE)]
  [dot-pen (make-pen #:width STATUS_DOT_BORDER)]
  [ok-brush (make-brush #:color "green")]
  [warning-brush (make-brush #:color "yellow")]
  [error-brush (make-brush #:color "red")]
  [status->brush
   (match-lambda
     ['ok ok-brush]
     ['warning warning-brush]
     ['error error-brush])]
  [(draw-status-dot dc status [x 0] [y 0])
   (let ([old-pen (send dc get-pen)]
         [old-brush (send dc get-brush)])
     (send dc set-pen dot-pen)
     (send dc set-brush (status->brush status))
     (send dc draw-ellipse x y STATUS_DOT_INNER_SIZE STATUS_DOT_INNER_SIZE)
     (send dc set-pen old-pen)
     (send dc set-brush old-brush))])

(define* status-dot-pict
  (def
    [(make status)
     (dc (λ (dc x y) (draw-status-dot dc status x y))
         STATUS-DOT-SIZE
         STATUS-DOT-SIZE)]
    [error (make 'error)]
    [warning (make 'warning)]
    [ok (make 'ok)])
  (λ (status)
    (case status
      [(error) error]
      [(warning) warning]
      [else ok])))

(define* status-canvas%
  (def
    [(make status)
     (pict->bitmap (status-dot-pict status)
                   #:make-bitmap make-screen-bitmap)]
    [error (make 'error)]
    [warning (make 'warning)]
    [ok (make 'ok)])
  (class message%
    (init status)
    (super-new [label (case status
                        [(error) error]
                        [(warning) warning]
                        [else ok])])))
