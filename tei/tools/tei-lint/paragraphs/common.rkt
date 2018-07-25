#lang racket/gui

(require framework
         ricoeur/tei/base
         "../lib.rkt"
         "../xml-preview.rkt"
         "../interfaces.rkt"
         )

(provide (contract-out
          [call-in-eventspace-thread
           (->* {(-> any/c)}
                {#:parent (or/c #f (is-a?/c top-level-window<%>))}
                any/c)]
          [add-title+path
           (->* {(or/c (is-a?/c frame%)
                       (is-a?/c dialog%)
                       (is-a?/c panel%)
                       (is-a?/c pane%))
                 tei-document?}
                {(or/c path-string? #f)}
                any)]
          [save-unless-outdated
           (->* {tei-document?}
                {#:path (or/c #f path-string?)
                 #:seconds real?
                 #:dir-frame dir-frame/false/c
                 #:parent (or/c #f (is-a?/c top-level-window<%>))}
                any)]
          ))

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


(define narrower-path-message%
  (class path-message%
    (super-new [max-width 375])))


(define (add-title+path parent doc [p #f])
  (let ([sub-col
         (new vertical-panel%
              [alignment '(left top)]
              [stretchable-height #f]
              [parent parent])])
    (insert-message-row
     sub-col
     "Title: "
     (instance-title doc))
    (insert-message-row
     sub-col
     "Path: "
     #:right-message% narrower-path-message%
     (cond
       [(string? p)
        p]
       [p
        (path->string p)]
       [else
        "<unknown>"]))))



(define (refresh-directory-box title message [parent #f])
  (TODO/void refresh-directory-box #:
             Replace message-box/custom with something
             using editor-message%)
  (message-box/custom
   title
   message
   "Refresh Directory"
   #f
   #f
   parent
   '(default=1)))


(define (save-unless-outdated new-doc
                              #:seconds [old-modify-seconds -inf.0]
                              #:path [pth #f]
                              #:dir-frame [dir-frame #f]
                              #:parent [parent #f])
  (cond
    [(and pth
          (infix: (file-or-directory-modify-seconds pth)
                  <=            
                  old-modify-seconds))
     (with-output-to-file/unless-exn pth
       #:exists 'truncate/replace
       #:mode 'text
       (λ () (write-tei-document new-doc)))
     (refresh-directory-box
      "Changes Saved Successfully - TEI Lint"
      "Your changes have been saved successfully."
      parent)]
    [else
     (refresh-directory-box
      "File Out of Sync - TEI Lint"
      (string-append
       "This file appears to have been modified since you "
       "last refreshed TEI Lint. Please try again.")
      parent)])
  (when dir-frame
    (send dir-frame refresh-directory!)))





