#lang racket/gui

(require adjutor
         racket/runtime-path
         pict
         )

(require-provide "lib/ed.rkt"
                 "lib/xml-preview.rkt"
                 "lib/long-path.rkt"
                 "lib/menu-bar.rkt"
                 "lib/shutdown.rkt"
                 )

(provide photo-bitmap
         bold-system-font
         big-bold-system-font 
         white 
         invalid-bg-color
         ;; should have contracts:
         progress-gauge%
         insert-message-row
         (contract-out
          [path->string* 
           (-> path-string? string?)]
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

(define (red-text-pict txt)
  (colorize (text txt '(aligned bold . system)) "red"))


(define (red-text-message str)
  (pict->message% (red-text-pict str)))





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








