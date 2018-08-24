#lang racket/gui

(require adjutor)

(provide tei-lint-menu-bar-frame%
         (contract-out
          [current-lint-directory
           indirection-proc/c]
          [current-create-new-tei-document
           indirection-proc/c]
          [current-open-splash-frame
           indirection-proc/c]
          [tei-lint-menu-bar%
           (class/c
            (init [parent (is-a?/c frame%)]))]
          [tei-lint-menu-bar-frame-mixin
           (make-mixin-contract frame%)]
          ))

(define/final-prop indirection-proc/c
  (case->
   (-> any)
   (-> (-> any) any)))

(define* current-lint-directory
  (define (proc)
    (displayln "Called current-lint-directory"))
  (case-lambda
    [()
     (proc)]
    [(new-proc)
     (set! proc new-proc)]))

(define* current-create-new-tei-document
  (define (proc)
    (displayln "Called current-create-new-tei-document"))
  (case-lambda
    [()
     (proc)]
    [(new-proc)
     (set! proc new-proc)]))

(define* current-open-splash-frame
  (define (proc)
    (displayln "Called current-open-splash-frame"))
  (case-lambda
    [()
     (proc)]
    [(new-proc)
     (set! proc new-proc)]))



(define tei-lint-menu-bar%
  (class menu-bar%
    (init parent)
    (super-new [parent parent])
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; File Menu:
    (define m-file
      (new menu%
           [label "File"]
           [parent this]))
    (new menu-item%
         [parent m-file]
         [label "Check Additional Directory…"]
         [shortcut #\O]
         [callback (λ (i e)
                     (current-lint-directory))])
    (new menu-item%
         [parent m-file]
         [label "New TEI Document…"]
         [shortcut #\N]
         [callback (λ (i e)
                     (current-create-new-tei-document))])
    (new menu-item%
         [parent m-file]
         [label "Open Splash Screen"]
         [callback (λ (i e)
                     (current-open-splash-frame))])
    (initialize-file-menu-extras m-file)
    (new separator-menu-item%
         [parent m-file])
    (new menu-item%
         [parent m-file]
         [label "Close Window"]
         [shortcut #\W]
         [callback (λ (i e)
                     (send parent show #f))])
    (new menu-item%
         [parent m-file]
         [label "Quit TEI Lint"]
         [callback (λ (i e)
                     (exit 0))])
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Edit Menu:
    (define m-edit
      (new menu%
           [label "Edit"]
           [parent this]))
    (append-editor-operation-menu-items m-edit 'text-only?)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods:
    (define/public (initialize-file-menu-extras m-file)
      (void))
    #|END class tei-lint-menu-bar%|#))


(define tei-lint-menu-bar-frame-mixin
  (mixin {(class->interface frame%)} {}
    (super-new)
    (new tei-lint-menu-bar%
         [parent this])))


(define tei-lint-menu-bar-frame%
  (tei-lint-menu-bar-frame-mixin frame%))


