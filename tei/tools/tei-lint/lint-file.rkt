#lang racket/gui

(require framework
         ricoeur/tei/base
         "lib.rkt"
         "interfaces.rkt"
         "menu.rkt"
         "proto-frame.rkt"
         "document.rkt"
         )

(provide (contract-out
          [file->proto-frame
           (->* {path-string?}
                {#:dir-valid? any/c
                 #:dir-frame (or/c #f (is-a?/c directory-frame<%>))}
                (is-a?/c proto-frame%))]
          ))

(define (file->proto-frame pth
                           #:dir-frame [dir-frame #f]
                           #:dir-valid? [dir-valid? #f])
  (define xmllint-out
    (open-output-string))
  (define old-modify-seconds
    (file-or-directory-modify-seconds pth))
  (define (handle-error str/exn)
    (new error-proto-frame%
         [path pth]
         [dir-frame dir-frame]
         [val str/exn]))
  (cond
    [(not (or dir-valid?
              (parameterize ([current-output-port xmllint-out]
                             [current-error-port xmllint-out])
                (valid-xml-file? #:quiet? #f pth))))
     (handle-error (get-output-string xmllint-out))]
    [else
     (with-handlers ([exn:fail? handle-error])
       (new tei-document-proto-frame%
            [doc (file->tei-document pth)]
            [dir-frame dir-frame]
            [path pth]
            [old-modify-seconds old-modify-seconds]))]))


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


