#lang racket/gui

(require ricoeur/tei
         racket/runtime-path
         data/maybe
         xml/path
         gregor
         adjutor
         pict
         )

(require-provide "lib/ed.rkt"
                 "lib/xml-preview.rkt"
                 "lib/long-path.rkt"
                 "lib/shutdown.rkt"
                 )

(provide photo-bitmap 
         status-canvas% 
         progress-gauge% 
         bold-system-font
         big-bold-system-font 
         STATUS-DOT-SIZE 
         insert-message-row
         white 
         invalid-bg-color
         (contract-out
          [path->string* 
           (-> path-string? string?)]
          [status-dot-pict 
           (-> (or/c 'ok 'error 'warning) pict?)]
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
          [red-text-message 
           (-> string? (subclass?/c message%))]
          [call-in-eventspace-thread
           (->* {(-> any/c)}
                {#:parent (or/c #f (is-a?/c top-level-window<%>))}
                any/c)]
          ))

(define white
  (make-color 255 255 255))

(define invalid-bg-color
  (make-color 255 0 0 0.333))

(define (path->string* p)
  (if (string? p)
      p
      (path->string p)))

(define-runtime-path photo-path
  "photo.png")

(define photo-bitmap
  (call-with-input-file photo-path
    read-bitmap))

(define (insert-message-row parent l r
                            #:alignment [alignment '(left top)]
                            #:stretchable-height [stretchable-height #t]
                            #:right-message% [right-message% message%]
                            #:left-font [left-font bold-system-font])
  (define row
    (new horizontal-pane%
         [parent parent]
         [stretchable-height stretchable-height]
         [alignment alignment]))
  (new message%
       [label l]
       [font left-font]
       [parent row])
  (new right-message%
       [label r]
       [parent row])
  (void))

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

(define (pict->message% pict)
  (define bmp
    (pict->bitmap pict #:make-bitmap make-screen-bitmap))
  (class message%
    (init [label bmp])
    (super-new [label label])))

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
  (colorize (text txt '(aligned bold . system)) "red"))


(define (red-text-message str)
  (pict->message% (red-text-pict str)))

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

(define status-canvas%
  (class message%
    (init status)
    (super-new [label
                (pict->bitmap (status-dot-pict status)
                              #:make-bitmap make-screen-bitmap)])))



(define bold-system-font
  (make-font #:family 'system
             #:smoothing 'smoothed
             #:weight 'bold))

(define big-bold-system-font
  (make-font #:family 'system
             #:size 24
             #:smoothing 'smoothed
             #:weight 'bold))

(define progress-gauge%
  (class gauge%
    (init [range 10]
          [label #f])
    (super-new [range range]
               [label label])
    (inherit get-value set-value)
    (define/public-final (++)
      (set-value (add1 (get-value))))))










(define (call-in-eventspace-thread thunk
                                   #:parent [parent #f])
  (define es
    (if parent 
        (send parent get-eventspace)
        (current-eventspace)))
  (if (eq? (current-thread) (eventspace-handler-thread es))
      ;; In the right thread:
      (thunk)
      ;; Not in the right thread:
      (let ([ch (make-channel)])
        (parameterize ([current-eventspace es])
          (queue-callback
           (λ () (channel-put ch (thunk)))))
        (channel-get ch))))








