#lang racket/gui

(require ricoeur/tei/base
         "lib.rkt"
         "directory-frame.rkt"
         )

(module+ main
  (start))

(define (start)
  (application-quit-handler
   (λ () (exit 0)))
  (define (the-yield-handler _)
    (wait-to-implicitly-exit))
  (executable-yield-handler the-yield-handler)
  (parameterize ([executable-yield-handler the-yield-handler])
    (new splash-frame%)))

(define-syntax (lift stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax-local-lift-expression #'expr)]))

(define splash-frame%
  (class frame%
    (inherit show)
    (super-new [label "TEI Lint"]
               [height (floor (* 5/4 (send photo-bitmap get-height)))])
    (let ([row (new horizontal-pane% [parent this])])
      (new message%
           [parent row]
           [label photo-bitmap])
      (define col
        (new vertical-pane% [parent row]))
      (new message%
           [parent col]
           [label "Digital Ricœur"]
           [font (lift (make-font #:family 'system
                                  #:size 24))])
      (new message%
           [parent col]
           [label  "TEI Lint"]
           [font (lift (make-font #:family 'system
                                  #:size 36
                                  #:weight 'bold))])
      (new editor-message%
           [parent col]
           [content "To begin, choose a directory containing TEI XML files."])
      (new button%
           [parent col]
           [label "Choose Directory …"]
           [callback (λ (b e) (choose-directory))]))
    (show #t)
    (unless (xmllint-available?)
      (show-xmllint-warning this))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; End of initialization
    (define/private (choose-directory)
      (let ([dir (get-xml-directory this)])
        (when dir
          (show #f)
          (make-directory-frame dir))))
    #|END class splash-frame%|#))
          
