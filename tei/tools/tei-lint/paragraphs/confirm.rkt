#lang racket/gui

(require framework
         ricoeur/tei/base
         "../lib.rkt"
         "../xml-preview.rkt"
         "common.rkt"
         )

(provide (contract-out
          [paragraphs:confirm
           (->* {tei-document? ;TODO
                 tei-document?} ; 'line-breaks 'blank-lines
                {#:path (or/c #f path-string?)
                 #:parent (or/c #f (is-a?/c top-level-window<%>))}
                (or/c 'confirm 'cancel))]
          ))

(define (paragraphs:confirm doc
                            new-doc
                            #:path [pth #f]
                            #:parent [parent #f])
  (call-in-eventspace-thread
   #:parent parent
   (λ ()
     (define bx:result
       (box 'cancel))
     (define w
       (new paragraphs:confirmation-dialog%
            [parent parent]
            [doc doc]
            [new-doc new-doc]
            [pth pth]
            [bx:result bx:result]))
     (send w show #t)
     (unbox bx:result))))


(define paragraphs:confirmation-dialog%
  (class dialog%
    (init-field bx:result
                doc
                new-doc
                [pth #f]
                [(my-parent parent) #f])
    (super-new [parent my-parent]
               [label "Confirm Changes - TEI Lint"]
               [width 800]
               [height 800]
               [alignment '(center top)]
               [style '(resize-border)]
               [spacing 12])
    (inherit show)
    (new message%
         [label "Does this look right?"]
         [font big-bold-system-font]
         [parent this])
    (add-title+path this doc pth)
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
           [label (case (tei-document-paragraphs-status new-doc)
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
    (gui-utils:ok/cancel-buttons
     (new horizontal-pane%
          [alignment '(right center)]
          [stretchable-height #f]
          [parent this])
     (λ (b e)
       (set-box! bx:result 'confirm)
       (show #f))
     (λ (b e)
       (set-box! bx:result 'cancel)
       (show #f))
     "Yes — Save Changes"
     #:confirm-style null)
    #|END paragraphs:confirmation-dialog%|#))







