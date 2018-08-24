#lang racket/gui

(require ricoeur/tei/base
         "lib.rkt"
         "lint.rkt"
         "new-tei-document.rkt"
         )

(module+ main
  (start))

(define (start)
  (application-quit-handler
   (λ () (exit 0)))
  (define (the-yield-handler _)
    (wait-to-implicitly-exit))
  (executable-yield-handler the-yield-handler)
  (current-open-splash-frame (λ () (new splash-frame%)))
  (current-lint-directory lint-directory)
  (current-create-new-tei-document create-new-tei-document)
  (parameterize ([executable-yield-handler the-yield-handler])
    (new splash-frame%)))


(define* splash-frame%
  (define title-font
    (make-font #:family 'system
               #:size 36
               #:weight 'bold))
  (define subtitle-font
    (make-font #:family 'system
               #:size 24))
  (class tei-lint-menu-bar-frame%
    (inherit show)
    (super-new [label "TEI Lint"]
               [spacing 10]
               [alignment '(center top)])
    (let ([row (new horizontal-pane%
                    [alignment '(center center)]
                    [stretchable-height #f]
                    [parent this])])
      (new message%
           [parent row]
           [label photo-bitmap])
      (define col
        (new vertical-pane%
             [alignment '(center center)]
             [parent row]))
      (new message%
           [parent col]
           [label "Digital Ricœur"]
           [font subtitle-font])
      (new message%
           [parent col]
           [label  "TEI Lint"]
           [font title-font]))
    (let ([row (new horizontal-pane%
                    [alignment '(center top)]
                    [parent this])])
      (let ([col (new vertical-pane%
                      [alignment '(center top)]
                      [stretchable-width #f]
                      [parent row])])
        (new message%
             [label "New Document"]
             [font subtitle-font]
             [parent col])
        (new editor-message%
             [content "Start a new TEI document based on a plain-text file."]
             [center? #t]
             [parent col])
        (new button%
             [label "Create TEI Document …"]
             [callback (λ (b e) (choose-raw-text-file))]
             [parent col]))
      (new vertical-pane%
           [parent row])
      (let ([col (new vertical-pane%
                      [alignment '(center top)]
                      [stretchable-width #f]
                      [parent row])])
        (new message%
             [label "Check Directory"]
             [font subtitle-font]
             [parent col])
        (new editor-message%
             [content "Check a directory containing TEI XML files for errors."]
             [center? #t]
             [parent col])
        (new button%
             [parent col]
             [label "Choose Directory …"]
             [callback (λ (b e) (choose-directory))])))
    (show #t)
    (unless (xmllint-available?)
      (show-xmllint-warning this))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods:
    (define/private (choose-directory)
      (let ([dir (get-xml-directory this)])
        (when dir
          (show #f)
          (make-directory-frame dir))))
    (define/private (choose-raw-text-file)
      (create-new-tei-document this #:after-frame-show (λ () (show #f))))
    #|END class splash-frame%|#))
          
