#lang racket/gui

(require framework
         ricoeur/tei/base
         "../lib.rkt"
         "../interfaces.rkt"
         "common.rkt"
         "confirm.rkt"
         )

(provide (contract-out
          [paragraphs:prompt%
           (class/c
            (init-field [doc tei-document?] ;; 'skip 'todo
                        [pth (or/c #f path-string?)]
                        [old-modify-seconds real?]
                        [dir-frame dir-frame/false/c]
                        [parent (or/c #f (is-a?/c top-level-window<%>))]))]
          ))

(define paragraphs:prompt%
  (class dialog%
    (init-field doc
                [pth #f]
                [old-modify-seconds -inf.0]
                [dir-frame #f]
                [(my-parent parent) #f])
    (super-new [label "Convert to Paragraphs - TEI Lint"]
               [width 800]
               [height 500]
               [style '(resize-border)]
               [parent my-parent])
    (inherit show)
    (define row
      (new horizontal-panel% ;panel:horizontal-dragable%
           [alignment '(left top)]
           [parent this]))
    (define left-col
      (new vertical-panel%
           [alignment '(center top)]
           [spacing 12]
           [stretchable-width #f]
           [parent row]))
    (new message%
         [label "Convert to Paragraphs"]
         [font big-bold-system-font]
         [parent left-col])
    (add-title+path left-col doc pth)
    (let ([sub-col
           (new vertical-panel%
                [alignment '(left top)]
                [stretchable-height #f]
                [parent left-col])])
      (new message%
           [label "How is this document divided into paragraphs?"]
           [parent sub-col]))
    (define button-col
      (new vertical-panel%
           [alignment '(center top)]
           [stretchable-height #t]
           [parent left-col]))
    (new button%
         [label "Line Breaks"]
         [callback (λ (b e) (do-guess 'line-breaks))]
         [parent button-col])
    (new button%
         [label "Blank Lines"]
         [callback (λ (b e) (do-guess 'blank-lines))]
         [parent button-col])
    (case (tei-document-paragraphs-status doc)
      [(todo)
       (new button% 
            [label "Neither — Skip This File"]
            [callback (λ (b e) (on-skip-clicked))]
            [parent button-col])]
      [(skip)
       (TODO/void unskip)])
    (define cancel-button
      (new button%
           [label "Cancel"]
           [style '(border)]
           [callback (λ (b e)
                       (show #f))]
           [parent button-col]))
    (define ed
      (new xml-preview-text%
           [doc doc]))
    (new editor-canvas%
         [parent row]
         [style '(auto-hscroll auto-vscroll)]
         [editor ed])
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/private (do-guess mode)
      (define new-doc
        (tei-document-guess-paragraphs doc #:mode mode))
      (case (paragraphs:confirm doc new-doc
                                #:parent this
                                #:path pth)
        [(confirm)
         (save-unless-outdated
          new-doc
          #:seconds old-modify-seconds
          #:path pth
          #:dir-frame dir-frame
          #:parent this)]))
    (define/private (on-skip-clicked)
      (case (message-box/custom
             "Really skip paragraphs? - TEI Lint"
             (string-append
              "Are you sure you want to skip converting \""
              (instance-title doc)
              "\" into paragraphs?\n\n"
              "This will mark the file so that you won't be "
              "warned about converting it into paragraphs "
              "in the future. "
              "If you just don't want to do it now, "
              "click \"Cancel\".")
             "Skip and Update File"
             "Cancel"
             #f
             this
             '(default=2 caution)
             2)
        [(1)
         (save-unless-outdated
          (tei-document-skip-guess-paragraphs doc)
          #:seconds old-modify-seconds
          #:path pth
          #:dir-frame dir-frame
          #:parent this)]))
    (define/override (on-traverse-char evt)
      (case (send evt get-key-code)
        [(#\return numpad-enter)
         (send cancel-button
               command
               (new control-event%
                    [event-type 'button]
                    [time-stamp (current-inexact-milliseconds)]))]
        [else
         (super on-traverse-char evt)]))
    #|END class paragraphs:prompt%|#))








(module+ main
  (define p
    "/Users/philip/code/ricoeur/scratch/new-paragraphs/raw-blank-lines-example.xml")
  (send (new paragraphs:prompt%
             [pth p]
             [doc (file->tei-document p)])
        show
        #t)
  #|END module+ main|#)

