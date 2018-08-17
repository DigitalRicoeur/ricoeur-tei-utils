#lang racket/gui

(require framework
         ricoeur/tei/base
         pict
         pict/snip
         "lib.rkt"
         "interfaces.rkt"
         "lint-file.rkt"
         )

(provide file-snip/c
         (contract-out
          [file-snip-before?
           (-> file-snip/c
               file-snip/c
               any/c)]
          [file-snip%
           (class/c
            (init [path (and/c path-string?
                               file-exists?)]
                  dir-valid?))]
          ))

(define (file-snip-before? a b)
  (let ([a-status (send a get-lint-status)]
        [b-status (send b get-lint-status)])
    (or (lint-status-more-urgent? a-status b-status)
        (and (eq? a-status b-status)
             (title<? (send a get-quasititle)
                      (send b get-quasititle))))))

(define file-snip*%
  (class* pict-snip% {(interface (lint-status<%>)
                        [get-quasititle
                         (->m string-immutable/c)])}
    (super-new)
    (init [(_proto-frame proto-frame)]
          [(_path path)])
    (def
      [proto-frame _proto-frame]
      [path _path]
      [quasititle (match (send proto-frame get-maybe-title)
                    [(just str) str]
                    [_ (string->immutable-string
                        (path->string* path))])])
    (define/public-final (get-lint-status)
      (send proto-frame get-lint-status))
    (define/public-final (get-quasititle)
      quasititle)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; general snip stuff
    (inherit-field pict)
    (define/override-final (copy)
      (new file-snip*%
           [pict pict]
           [path path]
           [proto-frame proto-frame]))
    (inherit get-flags
             set-flags)
    (set-flags (list* 'handles-events
                      'handles-all-mouse-events
                      'hard-newline
                      (get-flags)))
    (define/override (adjust-cursor dc	 
                                    x	 
                                    y	 
                                    editorx	 
                                    editory	 
                                    event)
      hand-cursor)
    (define mouse-state #f)
    (define/override-final (on-event dc x y ed-x ed-y evt)
      (case (send evt get-event-type)
        [(left-down)
         (set! mouse-state 'left-down)]
        [(left-up)
         (when mouse-state
           (send proto-frame show #t))
         (set! mouse-state #f)]
        [else
         (set! mouse-state #f)
         (super on-event dc x y ed-x ed-y evt)]))
    #|END class file-snip*%|#))


(define file-snip%
  (class file-snip*%
    (init path
          [dir-frame #f]
          [dir-valid? #f])
    (let ([proto-frame
           (file->proto-frame path
                              #:dir-frame dir-frame
                              #:dir-valid? dir-valid?)])
      (super-new
       [path path]
       [proto-frame proto-frame]
       [pict
        (file-snip-pict
         #:status (send proto-frame get-lint-status)
         #:path path
         #:title (from-just #f (send proto-frame get-maybe-title)))]))))


(define/final-prop file-snip/c
  (is-a?/c file-snip*%))

(define hand-cursor
  (make-object cursor% 'hand))

(define (file-snip-pict #:status status
                        #:path path
                        #:title [title #f])
  (define padding 1.0)
  (define line-padding 1.0)
  (define gutter (/ STATUS_DOT_SIZE 2))
  (define path-pict
    (path->wrapped-pict path
                        #:max-width 600
                        #:font (if title
                                   normal-control-font
                                   bold-system-font)))
  (define rhs-pict
    (cond
      [title
       (define title-pict
         (text title (cons 'aligned bold-system-font)))
       (vl-append line-padding
                  title-pict
                  path-pict)]
      [else
       path-pict]))
  (define base
    (hc-append gutter (status-dot-pict status) rhs-pict))
  (cc-superimpose
   (blank (+ (* 2 padding) (pict-width base))
          (+ (* 2 padding) (pict-height base)))
   base))



