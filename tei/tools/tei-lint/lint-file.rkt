#lang racket/gui

(require framework
         ricoeur/tei/base
         "lib.rkt"
         "interfaces.rkt"
         "proto-frame.rkt"
         "document.rkt"
         )

(provide (contract-out
          [file->proto-frame
           (->* {path-string?}
                {#:dir-valid? any/c}
                (is-a?/c proto-frame%))]
          ))

(define (file->proto-frame pth
                           #:dir-valid? [dir-valid? #f])
  (define xmllint-out
    (open-output-string))
  (define old-modify-seconds
    (file-or-directory-modify-seconds pth))
  (define (handle-error str/exn)
    (new error-proto-frame%
         [path pth]
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
            [path pth]
            [old-modify-seconds old-modify-seconds]))]))


(define error-proto-frame%
  (class proto-frame%
    (init path val)
    (super-new
       [lint-status 'error]
       [maybe-title (nothing)]
       [make-frame
        (λ ()
          (new error-frame%
               [path path]
               [val val]))])))

          
(define error-frame%
  (class frame%
    (init path val)
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
    #|(let ([mb (new menu-bar% [parent this])])
      (add-file-menu mb dir-frame)
      (define m-edit (new menu% [label "Edit"] [parent mb]))
      (append-editor-operation-menu-items m-edit #t))|#
    #|END class error-frame%|#))


