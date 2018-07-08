#lang racket/gui

(require framework
         ricoeur/tei/base
         "lib.rkt"
         "interfaces.rkt"
         "file-snip.rkt"
         )

(provide (contract-out
          [directory-frame%
           (class/c
            (init [dir (and/c path-string?
                              directory-exists?)]))]
          ))

(module+ main
  (new directory-frame%
       [dir "/Users/philip/code/ricoeur/texts/TEI/"]))

(define directory-frame%
  (class frame%
    (init [(_dir dir)])
    (define dir _dir)
    (super-new [label (gui-utils:quote-literal-label
                       (string-append (path->string* dir)
                                      " - TEI Lint"))]
               [width 800]
               [height 600]
               [alignment '(left top)])
    (inherit show)
    (define file-snips
      (let ([progress (new loading-frame%
                           [dir dir])])
        (let ([row (new horizontal-pane%
                        [parent this]
                        [stretchable-height #f]
                        [alignment '(left center)])])
          (new message%
               [parent row]
               [label (gui-utils:quote-literal-label
                       (path->string* dir))])
          (new button%
               [parent row]
               [label "Refresh"]
               [callback (λ (b e) (TODO refresh-directory))]))
        (define ed
          (new text%))
        (define ec
          (new editor-canvas%
               [style '(transparent auto-hscroll auto-vscroll)]
               [parent this]
               [editor ed]))
        (define file-snips
          (let ([pths (for/list ([pth (in-directory dir)]
                                 #:when (xml-path? pth))
                        pth)])
            (send progress set-range (length pths))
            (define dir-valid?
              (directory-validate-xml #:quiet? #t
                                      dir))
            (with-method ([progress++ {progress ++}])
              (for/list ([pth (in-list pths)])
                (begin0 (new file-snip%
                             [path pth]
                             [dir-valid? dir-valid?])
                        (progress++))))))
        (for ([snip (in-list (sort file-snips
                                   file-snip-before?))])
          (send ed insert snip))
        (scroll-editor-to-top ed)
        (send ed lock #t)
        (send ed hide-caret #t)
        (TODO/void menu-bar%)
        #|(add-file-menu (new menu-bar% [parent this])
                       this
                       #:directory? #t)|#
        ;; Refresh the editor-canvas here b/c otherwise some strange
        ;; circumstance sometimes makes it appear empty.
        (send ec refresh) 
        (send progress show #f)
        (show #t)
        file-snips))
    #|END class directory-frame%|#))


(define loading-frame%
  (class frame%
    (init dir
          [dir-string (path->string* dir)])
    (super-new [label (gui-utils:quote-literal-label
                       (string-append "Loading "
                                      dir-string
                                      "… - TEI Lint"))]
               [alignment '(center top)])
    (inherit show)
    (new message%
         [parent this]
         [label (gui-utils:quote-literal-label
                 (string-append "Checking files in "
                                dir-string
                                " …"))])
    (define progress
      (new progress-gauge%
           [parent this]))
    (show #t)
    (define/public-final (set-range v)
      (send progress set-range v))
    (define/public-final (++)
      (send progress ++))))

