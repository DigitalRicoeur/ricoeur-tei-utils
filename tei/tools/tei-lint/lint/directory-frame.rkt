#lang racket/gui

(require framework
         racket/async-channel
         ricoeur/tei/base
         ricoeur/tei/tools/tei-lint/lib
         "toolkit.rkt"
         "file-snip.rkt"
         )

(provide lint-directory
         (contract-out
          [make-directory-frame
           (-> (and/c path-string?
                      directory-exists?)
               (is-a?/c directory-frame<%>))]
          ))

(module+ main
  (make-directory-frame
   "/Users/philip/code/ricoeur/texts/TEI/"))

(define (make-directory-frame dir)
  (new directory-frame%
       [dir dir]))

(define refresh-worker
  (thread (λ ()
            (let loop ()
              (thread (thread-receive))
              (loop)))))

(define directory-frame%
  (class* frame% {directory-frame<%>}
    (init [(_dir dir)])
    (define dir _dir)
    (super-new [label (gui-utils:quote-literal-label
                       (string-append (path->string* dir)
                                      " - TEI Lint"))]
               [width 800]
               [height 600]
               [alignment '(left top)])
    (inherit show
             get-eventspace)
    (define cust
      (make-custodian))
    (define child-eventspace
      (parameterize ([current-custodian cust])
        (make+register-eventspace)))
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
               [callback (λ (b e) (refresh-directory!))]))
        (define* file-snips
          (define pths
            (for/list ([pth (in-directory dir)]
                       #:when (xml-path? pth))
              pth))
          (send progress set-range (length pths))
          (define dir-valid?
            (directory-validate-xml #:quiet? #t
                                    dir))
          (define unsorted-file-snips
            (parameterize ([current-custodian cust])
              (with-method ([progress++ {progress ++}])
                (for/list ([pth (in-list pths)])
                  (begin0 (new file-snip%
                               [path pth]
                               [dir-frame this]
                               [dir-valid? dir-valid?])
                          (progress++))))))
          (sort unsorted-file-snips file-snip-before?))
        (define ec
          (new editor-canvas%
               [style '(transparent auto-hscroll auto-vscroll)]
               [parent this]
               [editor (new file-snip-editor%
                            [snips file-snips])]))
        (new menu-bar:file%
             [parent this]
             [dir-frame this])
        ;; Refresh the editor-canvas here b/c otherwise some strange
        ;; circumstance sometimes makes it appear empty.
        (send ec refresh) 
        (send progress show #f)
        file-snips))
    (show #t)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public-final (refresh-directory!)
      ;; Checking for the handler thread of child-eventspace
      ;; isn't good enough b/c could be in some document
      ;; frame's eventspace.
      (thread-send
       refresh-worker
       (λ ()
         (call/disable-implicit-exit
          (λ () 
            (custodian-shutdown-all cust)
            (show #f)
            (make-directory-frame dir))))))
    (define/public-final (call-in-directory-context thunk use-fresh-eventspace?)
      (parameterize ([current-custodian cust])
        (define ach
          (make-async-channel))
        (define es
          (if use-fresh-eventspace?
              (make+register-eventspace)
              child-eventspace))
        (parameterize ([current-eventspace es])
          (queue-callback
           (λ ()
             (parameterize ([current-custodian cust]
                            [current-eventspace es])
               (async-channel-put ach (thunk)))))
          (yield ach))))
    (define/public-final (open-additional)
      (lint-directory))
    #|END class directory-frame%|#))

(define (lint-directory)
  (let ([dir (get-xml-directory)])
    (when dir
      (make-directory-frame dir))))

(define file-snip-editor%
  (class text%
    (super-new)
    (inherit insert
             hide-caret
             begin-edit-sequence
             end-edit-sequence)
    (init [snips null])
    (define initializing? #t)
    (begin-edit-sequence)
    (for ([s (in-list snips)])
      (insert s))
    (end-edit-sequence)
    (scroll-editor-to-top this)
    (set! initializing? #f)
    (hide-caret #t)
    (define/augment (can-delete? start len)
      #f)
    (define/augment (can-insert? start len)
      initializing?)
    #|END class file-snip-editor%|#))

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
      (when (infix: v >= 1)
        (send progress set-range v)))
    (define/public-final (++)
      (send progress ++))))

