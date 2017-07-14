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
         pict-canvas<%>
         dot-canvas%
         bold-system-font
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
           (-> pict? (implementation?/c pict-canvas<%>))]
          [red-text-pict
           (-> string? pict?)]
          [struct xmllint-error ([str string?])]
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

(define pict-canvas<%>
  (interface ((class->interface canvas%))
    [replace-pict (->m pict? any)]))

(define (pict->canvas% pict)
  (def
    [init-drawer (make-pict-drawer pict)]
    [w (inexact->exact (ceiling (pict-width pict)))]
    [h (inexact->exact (ceiling (pict-height pict)))])
  (class* canvas% [pict-canvas<%>]
    (init [style '(transparent)])
    (super-new [style style]
               [min-width w]
               [min-height h]
               [stretchable-width #f]
               [stretchable-height #f])
    (define drawer
      init-drawer)
    (inherit get-dc refresh-now)
    (define/override (on-paint)
      (drawer (get-dc) 0 0))
    (define/public (replace-pict pict)
      (set! drawer (make-pict-drawer pict))
      (refresh-now))))

(define (red-text-pict txt)
  (colorize (text txt '(bold . system)) "red"))

(define/contract dot-canvas%
  (class/c (init [size natural-number/c]
                 [color (or/c string? (is-a?/c color%))])
           [set-color (->m (or/c string? (is-a?/c color%)) any)])
  (let ([dot-pen (make-pen #:width 1)])
  (class canvas%
    (init [(init-color color)]
          [(init-size size) 20])
    (define my-brush
      (color->brush init-color))
    (define size
      init-size)
    (super-new [style '(transparent)]
               [min-width (add1 size)]
               [min-height (add1 size)]
               [stretchable-width #f]
               [stretchable-height #f])
    (inherit get-dc refresh-now)
    (define/private (color->brush color)
      (make-brush #:color color))
    (define/override (on-paint)
      (let* ([dc (get-dc)]
             [old-pen (send dc get-pen)]
             [old-brush (send dc get-brush)])
        (send dc set-pen dot-pen)
        (send dc set-brush my-brush)
        (send dc draw-ellipse 0 0 size size)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)))
    (define/public (set-color c)
      (set! my-brush (color->brush c))
      (refresh-now)))))

(struct xmllint-error (str))

(define bold-system-font
  (make-font #:family 'system
             #:weight 'bold))