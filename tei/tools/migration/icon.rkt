#lang racket/base

(require racket/draw
         racket/class
         )

(provide draw-DR
         make-DR-bitmap
         )

(module+ write
  (require racket/runtime-path
           file/ico
           racket/system
           racket/port
           )
  (provide write-icons))

(module+ main
  (require (submod ".." write))
  (write-icons))

(define (draw-DR size dc [transparent? #f])
  (unless transparent?
    (send* dc
      (set-pen "white" 0 'transparent)
      (set-brush "white" 'solid)
      (draw-rectangle 0 0 size size)))
  (let loop ([try-font-size (min size 1024.0)])
    (send dc set-font (send the-font-list
                            find-or-create-font
                            try-font-size
                            ""
                            'system
                            'normal
                            'bold
                            #f
                            'smoothed
                            #t))
    (define-values (w h d a)
      (send dc get-text-extent "DR" #f #t))
    (if (or (and (< w size)
                 (< h size))
            (< size 3))
        (send dc draw-text "DR" (/ (- size w) 2) (/ (- size h) 2) #t)
        (loop (sub1 try-font-size)))))

(define (make-DR-bitmap size
                        #:transparent? [transparent? #f]
                        [make-bitmap make-platform-bitmap])
  (define bmp
    (make-bitmap size size))
  (define dc
    (new bitmap-dc% [bitmap bmp]))
  (draw-DR size dc transparent?)
  bmp)

(module+ write
  (define-runtime-path png-path
    "migration-assistant.png")
  (define-runtime-path icns-path
    "migration-assistant.icns")
  (define-runtime-path ico-path
    "migration-assistant.ico")
  (define sips
    (find-executable-path "sips"))


  (define (write-icons)
    (define big
      (make-DR-bitmap 256))
    (send big
          save-file
          png-path
          'png)
    (write-icos
     (for/list ([size (in-list '(256 128 64 48 32 16))]
                [bmp (in-list (cons big
                                    (map make-DR-bitmap
                                         '(128 64 48 32 16))))])
       (argb->ico size size
                  (let ([pixels (make-bytes (* size size 4))])
                    (send bmp get-argb-pixels 0 0 size size pixels)
                    pixels)))
     ico-path
     #:exists 'replace)
    (when sips
      (void
       (parameterize ([current-output-port (open-output-nowhere)])
         (system* sips
                  "-s" "format" "icns"
                  ico-path
                  "--out" icns-path)))))
  #|END module+ write|#)


