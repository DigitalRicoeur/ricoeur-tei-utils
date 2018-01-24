#lang racket/gui

(require framework
         ricoeur/tei
         ricoeur/lib
         (submod ricoeur/tei/oop/interfaces private)
         ricoeur/tei/tools/tei-lint/lib
         ricoeur/tei/tools/tei-lint/xml-preview
         adjutor
         )

(provide paragraphs:prompt%
         )

;; TODO: Replace message-box/custom with something using editor-message%

(define paragraphs:prompt%
  (class frame%
    (init-field doc
                [maybe-dir-frame #f]
                [(my-parent parent) #f]
                )
    (init [width 800]
          [height 500]
          )
    (super-new [label "Convert to Paragraphs - TEI Lint"]
               [width width]
               [height height]
               [parent my-parent]
               )
    (inherit show)
    (when maybe-dir-frame
      (send maybe-dir-frame register-revoke this))
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
    (add-title+path left-col doc)
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
    (new button%
         [label "Neither — Skip This File"]
         [callback (λ (b e) (on-skip-clicked))]
         [parent button-col])
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
    (show #t)    
    (send ed after-show)  
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/private (do-guess mode)
      (show #f)
      (new paragraphs:confirm%
           [doc doc]
           [new-doc (send doc guess-paragraphs #:mode mode)]
           [prompt this]
           [parent my-parent]
           [maybe-dir-frame maybe-dir-frame]
           [mode mode]))
    (define/private (on-skip-clicked)
      (show #f)
      (case (message-box/custom
             "Really skip paragraphs? - TEI Lint"
             (string-append
              "Are you sure you want to skip converting \""
              (send doc get-title)
              "\" into paragraphs?\n\n"
              "This will mark the file so that you won't be "
              "warned about converting it into paragraphs "
              "in the future. "
              "If you just don't want to do it now, "
              "click \"Cancel\".")
             "Skip and Update File"
             "Cancel"
             #f
             my-parent
             '(default=2 caution)
             2)
        [(1)
         (define new-doc
           (send doc update-guess-paragraphs-status 'skip))
         (save-or-refresh doc new-doc maybe-dir-frame)]
        [else
         (show #t)]))
    (define/override (on-traverse-char evt)
      (case (send evt get-key-code)
        [(#\return numpad-enter)
         (send cancel-button
               command
               (new control-event%
                    [event-type 'button]
                    [time-stamp (current-inexact-milliseconds)]))]
        [else
         (super on-traverse-char evt)]))))

(define narrower-path-message%
  (class path-message%
    (super-new [max-width 375])))

(define (add-title+path parent doc)
  (let ([sub-col
         (new vertical-panel%
              [alignment '(left top)]
              [stretchable-height #f]
              [parent parent])])
    (insert-message-row
     sub-col
     "Title: "
     (send doc get-title))
    (insert-message-row
     sub-col
     "Path: "
     #:right-message% narrower-path-message%
     (let ([p (send doc get-full-path)])
       (cond
         [(string? p)
          p]
         [p
          (path->string p)]
         [else
          "<unknown>"])))))


(define paragraphs:confirm%
  (class frame%
    (init-field doc
                new-doc 
                prompt
                mode
                [maybe-dir-frame #f]
                [(my-parent parent) #f]
                )
    (init [width 800]
          [height 800]
          )
    (super-new [parent my-parent]
               [label "Confirm Changes - TEI Lint"]
               [width width]
               [height height]
               [alignment '(center top)]
               [spacing 12]
               )
    (inherit show)
    (when maybe-dir-frame
      (send maybe-dir-frame register-revoke this))
    (new message%
         [label "Does this look right?"]
         [font big-bold-system-font]
         [parent this])
    (add-title+path this doc)
    (let ([sub-col
           (new vertical-panel%
                [alignment '(left top)]
                [stretchable-height #f]
                [parent this])])
      (define row
        (new horizontal-pane%
             [alignment '(left top)]
             [parent sub-col]))
      (new message%
           [label (string-append
                   "Here is the result of converting this document "
                   "into paragraphs based on ")]
           [parent row])
      (new message%
           [label (case mode
                    [(line-breaks) "line breaks."]
                    [(blank-lines) "blank lines."])]
           [font bold-system-font]
           [parent row])
      (new message%
           [label "Is it correct?"]
           [font bold-system-font]
           [parent sub-col]))
    (define-values {old-ed new-ed}
      (let ([row (new panel:horizontal-dragable%
                      [alignment '(center top)]
                      [parent this])])
        (define (make-ed-col label doc)
          (define col
            (new vertical-pane%
                 [parent row]
                 [alignment '(center top)]))
          (new message%
               [label label]
               [font bold-system-font]
               [parent col])
          (define ed
            (new xml-preview-text%
                 [doc doc]))
          (new editor-canvas%
               [parent col]
               [style '(auto-hscroll auto-vscroll)]
               [editor ed])
          ed)
        (values (make-ed-col "Original" doc)
                (make-ed-col "With Paragraphs" new-doc))))
    (define-values {ok-button cancel-button}
      (gui-utils:ok/cancel-buttons
       (new horizontal-pane%
            [alignment '(right center)]
            [stretchable-height #f]
            [parent this])
       (λ (b e) (do-confirm))
       (λ (b e) (do-cancel))
       "Yes — Save Changes"
       #:confirm-style null))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (show #t)
    (send old-ed after-show)
    (send new-ed after-show)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/private (do-cancel)
      (show #f)
      (send prompt show #t))
    (define/private (do-confirm)
      (show #f)
      (save-or-refresh doc new-doc maybe-dir-frame))
    #|END paragraphs:confirm%|#))


(define (save-or-refresh doc new-doc maybe-dir-frame)
  (define pth
    (send doc get-full-path))
  (cond
    [(infix: (file-or-directory-modify-seconds pth)
             <=            
             (send doc get-modify-seconds))
     (with-output-to-file/unless-exn pth
       #:exists 'truncate/replace
       #:mode 'text
       (λ () (send new-doc write-TEI)))
     (refresh-directory-box
      "Changes Saved Successfully - TEI Lint"
      "Your changes have been saved successfully.")]
    [else
     (refresh-directory-box
      "File Out of Sync - TEI Lint"
      (string-append
       "This file appears to have been modified since you "
       "last refreshed TEI Lint. Please try again."))])
  (when maybe-dir-frame
    (send maybe-dir-frame refresh-directory)))


(define (refresh-directory-box title message)
  (message-box/custom
   title
   message
   "Refresh Directory"
   #f
   #f
   #f
   '(default=1)))



(module+ main
  (new paragraphs:prompt%
       [doc (file->TEI "/Users/philip/code/ricoeur/texts/TEI/living_up_to_Death.xml")]))








