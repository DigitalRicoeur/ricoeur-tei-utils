#lang racket/gui

(require framework
         ricoeur/tei/base
         "../lib.rkt"
         "../interfaces.rkt"
         "../menu.rkt"
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
          [(d dir-frame) #f]
          [initialize void]
          [title pth])
    (def
      [dir-frame d]
      [lint-status status]
      [old-modify-seconds secs]
      [path pth])
    (define/public-final (get-dir-frame/false)
      dir-frame)
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
    (new menu-bar:file+edit%
         [parent this]
         [dir-frame dir-frame])
    #|END class tei-document-frame%|#))



(define (make-tei-document-proto-frame-class dfc)
  (class proto-frame%
    (init doc
          path
          old-modify-seconds)
    (inherit get-dir-frame/false)
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
               [dir-frame (get-dir-frame/false)]
               [lint-status status]
               [path path]
               [old-modify-seconds old-modify-seconds]
               [title title]
               [initialize initialize]))]))))

      

