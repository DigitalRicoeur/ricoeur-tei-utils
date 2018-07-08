#lang racket/gui

(require framework
         ricoeur/tei/base
         "../lib.rkt"
         "../interfaces.rkt"
         "../proto-frame.rkt"
         )

(provide tei-document-proto-frame%/c
         (contract-out
          [make-tei-document-proto-frame-class
           (-> document-frame-component?
               tei-document-proto-frame%/c)]
          ))

(define/final-prop tei-document-proto-frame%/c
  (and/c (subclass?/c proto-frame%)
         (class/c
          (init [doc tei-document?]
                [path path-string?]
                [old-modify-seconds real?]))))

(define tei-document-frame%
  (class* frame% {tei-document-frame<%>}
    (init [(status lint-status)]
          [(secs old-modify-seconds)]
          [(pth path)]
          [initialize void]
          [title pth])
    (def
      [lint-status status]
      [old-modify-seconds secs]
      [path pth])
    (define/public-final (get-lint-status)
      lint-status)
    (define/public-final (get-old-modify-seconds)
      old-modify-seconds)
    (define/public-final (get-path)
      path)
    (super-new [label (gui-utils:quote-literal-label
                       (string-append title
                                      " - TEI Lint"))]
               [alignment '(left top)]
               [width 400]
               [height 500])
    (initialize this)
    #|(let ([mb (new menu-bar% [parent this])])
      (add-file-menu mb dir-frame)
      (define m-edit (new menu% [label "Edit"] [parent mb]))
      (append-editor-operation-menu-items m-edit #t))|#
    #|END class tei-document-frame%|#))



(define (make-tei-document-proto-frame-class dfc)
  (class proto-frame%
    (init doc
          path
          old-modify-seconds)
    (let-values ([(status initialize)
                  (document-frame-component-run dfc doc)])
      (define title
        (instance-title doc))
      (super-new
       [lint-status status]
       [maybe-title (just title)]
       [make-frame
        (λ ()
          (new tei-document-frame%
               [lint-status status]
               [path path]
               [old-modify-seconds old-modify-seconds]
               [title title]
               [initialize initialize]))]))))

      

