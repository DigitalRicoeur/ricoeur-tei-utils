#lang racket/gui

(require framework
         ricoeur/tei/base
         "../lib.rkt"
         "interfaces.rkt"
         "menu.rkt"
         "proto-frame.rkt"
         (submod "proto-frame.rkt" private)
         )

(provide (contract-out
          [error-proto-frame%
           (class/c
            (init [path path-string?]
                  [dir-frame dir-frame/false/c]
                  [val (or/c exn:fail? string?)]))]
          ))

(define error-proto-frame%
  (class proto-frame%
    (init path val)
    (inherit get-dir-frame/false)
    (super-new
     [lint-status 'error]
     [maybe-title (nothing)]
     [make-frame
      (λ ()
        (new error-frame%
             [path path]
             [dir-frame (get-dir-frame/false)]
             [val val]))])))

          
(define error-frame%
  (class frame%
    (init path
          val
          [dir-frame #f])
    (super-new [label (gui-utils:quote-literal-label
                       (string-append (if (string? path)
                                          path
                                          (path->string path))
                                      " - TEI Lint"))]
               [alignment '(center top)]
               [width 400]
               [height 500])
    (let ([row (new horizontal-pane%
                    [parent this]
                    [alignment '(left center)])])
      (new status-canvas%
           [status 'error]
           [parent row])
      (new path-message%
           [parent row]
           [font bold-system-font]
           [path path]))
    (new message%
         [parent this]
         [label (if (string? val)
                    "xmllint found an error."
                    "The file is invalid.")])
    (new constant-editor-canvas%
         [parent this]
         [min-height 400]
         [content (match val
                    [(exn:fail msg _) msg]
                    [str str])])
    (new menu-bar:file+edit%
         [parent this]
         [dir-frame dir-frame])
    #|END class error-frame%|#))

