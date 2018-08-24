#lang racket/gui

(require "interfaces.rkt" adjutor)

(provide (contract-out
          [menu-bar:file%
           (class/c
            (init [dir-frame dir-frame/false/c]))]
          [menu-bar:file+edit%
           (class/c
            (init [dir-frame dir-frame/false/c]))]
          ))

(TODO/void replace this w/ something based on tei-lint-menu-bar%)

(define menu-item:disabled%
  (class menu-item%
    (super-new)
    (define/override-final (is-enabled?)
      #f)))

(define (append-directory-frame-menu-items m-file
                                           [dir-frame #f])
  (define %
    (if dir-frame
        menu-item%
        menu-item:disabled%))
  (define-syntax-rule (callback-when thunk)
    (if dir-frame
        thunk
        void))
  (new %
       [parent m-file]
       [label "Show Directory"]
       [callback (callback-when
                  (λ (i e) (send dir-frame show #t)))])
  (new %
       [parent m-file]
       [label "Check Additional Directory…"]
       [callback (callback-when
                  (λ (i e)
                    (send dir-frame open-additional)))]
       [shortcut #\O])
  (new %
       [parent m-file]
       [label "Refresh Directory"]
       [callback (callback-when
                  (λ (i e)
                    (send dir-frame refresh-directory!)))]
       [shortcut #\R]))

(define file-menu%
  (class menu%
    (init [dir-frame #f])
    (super-new [label "File"])
    (append-directory-frame-menu-items this dir-frame)))


(define menu-bar:file%
  (class menu-bar%
    (super-new)
    (init [dir-frame #f])
    (new file-menu%
         [parent this]
         [dir-frame dir-frame])))

(define menu-bar:file+edit%
  (class menu-bar:file%
    (super-new)
    (let ([m-edit (new menu%
                       [label "Edit"]
                       [parent this])])
      (append-editor-operation-menu-items m-edit #t))))


    
