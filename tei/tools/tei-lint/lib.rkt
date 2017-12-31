#lang racket/gui

(require ricoeur/tei
         racket/runtime-path
         data/maybe
         xml/path
         gregor
         adjutor
         pict
         )

(provide photo-bitmap
         status-canvas%
         progress-gauge%
         bold-system-font
         TEI-frame<%>
         STATUS_DOT_SIZE
         (contract-out
          [get-xml-directory
           (->* {}
                {(or/c (is-a?/c frame%)
                       (is-a?/c dialog%)
                       #f)}
                (or/c path? #f))]
          [show-xmllint-warning
           (->* {}
                {(or/c (is-a?/c frame%)
                       (is-a?/c dialog%)
                       #f)}
                any)]
          [pict->canvas%
           (-> pict? (implementation?/c canvas<%>))]
          [red-text-pict
           (-> string? pict?)]
          [struct xmllint-error ([str string?])]
          [draw-status-dot
           (->* {(is-a?/c dc<%>)
                 (or/c 'ok 'error 'warning)}
                {real? real?}
                any)]
          ))

(define-runtime-path photo-path
  "photo.png")

(define photo-bitmap
  (call-with-input-file photo-path
    read-bitmap))

(define (get-xml-directory [parent #f])
  (let ([pth (get-directory "Choose a directory containing TEI XML files."
                            parent)])
    (and pth
         (directory-exists? pth)
         pth)))

(define (show-xmllint-warning [parent #f])
  (message-box "xmllint Not Found"
               "xmllint Not Found\n\nThe program xmllint was not found. Please install libxml2 for full validation."
               parent
               '(ok caution)))

(define (pict->canvas% pict)
  (def
    [init-drawer (make-pict-drawer pict)]
    [w (inexact->exact (ceiling (pict-width pict)))]
    [h (inexact->exact (ceiling (pict-height pict)))])
  (class canvas% 
    (init [style '(transparent)])
    (super-new [style style]
               [min-width w]
               [min-height h]
               [stretchable-width #f]
               [stretchable-height #f])
    (inherit get-dc refresh-now)
    (define/override (on-paint)
      (init-drawer (get-dc) 0 0))))

(define (red-text-pict txt)
  (colorize (text txt '(bold . system)) "red"))


(def
  [STATUS_DOT_INNER_SIZE 20]
  [STATUS_DOT_BORDER 1]
  [STATUS_DOT_SIZE (+ STATUS_DOT_BORDER
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

(define/contract status-canvas%
  (class/c (init [status (or/c 'ok 'warning 'error)])
           [set-status (->m (or/c 'ok 'warning 'error) any)])
  (class canvas%
    (init [(init-status status)]
          [(init-size size) 20])
    (define status
      init-status)
    (super-new [style '(transparent)]
               [min-width STATUS_DOT_SIZE]
               [min-height STATUS_DOT_SIZE]
               [stretchable-width #f]
               [stretchable-height #f])
    (inherit get-dc refresh-now)
    (define/public (set-status new-status)
      (set! status new-status)
      (refresh-now))
    (define/private (color->brush color)
      (make-brush #:color color))
    (define/override (on-paint)
      (draw-status-dot (get-dc) status))))

(struct xmllint-error (str))

(define bold-system-font
  (make-font #:family 'system
             #:weight 'bold))

(define progress-gauge%
  (class gauge%
    (super-new [range 10]
               [label #f])
    (inherit get-value set-value)
    (define/public-final (++)
      (set-value (add1 (get-value))))))


(define TEI-frame<%>
  (interface [(class->interface frame%)]
    [get-status (->m (or/c 'ok 'warning 'error))]
    [get-title (->m (maybe/c string?))]))