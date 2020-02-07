#lang racket/gui

(require framework
         ricoeur/tei/base
         ricoeur/tei/tools/tei-lint/lib
         "../toolkit.rkt"
         "prompt.rkt"
         (only-in adjutor/unstable TODO/void))

(provide document-frame-component:paragraphs)

(define document-frame-component:paragraphs
  (document-frame-component
   (λ (doc) 
     (define status
       (tei-document-paragraphs-status doc))
     (case status
       [(line-breaks blank-lines done)
        (values
         'ok
         (λ (this)
           (new message%
                [parent (paragraphs-row this)]
                [label (case status
                         [(done) "Done"]
                         [(line-breaks) "Line Breaks"]
                         [(blank-lines) "Blank Lines"])])))]
       [(skip)
        (values
         'ok
         (λ (this)
           (define row
             (paragraphs-row this))
           (new message%
                [parent row]
                [label "Skipped "])
           (TODO/void unskip)
           (new prompt-button%
                [doc doc]
                [frame this]
                [dir-frame (send this get-dir-frame/false)]
                [parent row])))]
       [(todo)
        (values
         'warning
         (λ (this)
           (define row
             (paragraphs-row this))
           (new not-done-message%
                [parent row])
           (new prompt-button%
                [doc doc]
                [frame this]
                [dir-frame (send this get-dir-frame/false)]
                [parent row])))]))))


(define (paragraphs-row frame)
  (define row
    (new horizontal-pane%
         [parent frame]
         [alignment '(left center)]))
  (new message%
       [label "Paragraphs: "]
       [font bold-system-font]
       [parent row])
  row)

(define not-done-message%
  (red-text-message "Not Done"))

(define prompt-button%
  (class button%
    (init doc
          frame
          [dir-frame #f]
          [label "Do Now"])
    (define pr:prompt
      (with-method ([get-old-modify-seconds
                     {frame get-old-modify-seconds}]
                    [get-path
                     {frame get-path}])
        (delay
          (new paragraphs:prompt%
               [doc doc]
               [pth (get-path)]
               [old-modify-seconds (get-old-modify-seconds)]
               [dir-frame dir-frame]
               [parent frame]))))
    (super-new [label label]
               [callback
                (λ (b e)
                  (send (force pr:prompt) show #t))])))

