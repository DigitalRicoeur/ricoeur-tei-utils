#lang racket/gui

(require "interfaces.rkt"
         ricoeur/tei/tools/tei-lint/lib
         adjutor)

(provide dir-menu-bar-frame<%>
         (contract-out
          [dir-menu-bar%
           (class/c
            (init [dir-frame dir-frame/false/c]))]
          [dir-menu-bar-frame-mixin
           (->* {(subclass?/c frame%)}
                {#:pass-through? any/c}
                (class/c
                 (init [dir-frame dir-frame/false/c])))]
          [dir-menu-bar-frame%
           (class/c
            (init [dir-frame dir-frame/false/c]))]
          ))

(define menu-item:disabled%
  (class menu-item%
    (super-new)
    (define/override-final (is-enabled?)
      #f)))

(define dir-menu-bar%
  (class tei-lint-menu-bar%
    (init [(_dir-frame dir-frame) #f])
    (define dir-frame _dir-frame)
    (super-new)
    (define/override (initialize-file-menu-extras m-file)
      (new separator-menu-item%
           [parent m-file])
      (define %
        (if dir-frame menu-item% menu-item:disabled%))
      (define-syntax-rule (callback-when thunk)
        (if dir-frame thunk void))
      (new %
           [parent m-file]
           [label "Show Directory"]
           [callback (callback-when
                      (λ (i e) (send dir-frame show #t)))])
      (new %
           [parent m-file]
           [label "Refresh Directory"]
           [callback (callback-when
                      (λ (i e) (send dir-frame refresh-directory!)))]
           [shortcut #\R])
      (void))
    #|END class dir-frame-menu-bar%|#))

(define-member-name secret-method (generate-member-key))

(define dir-menu-bar-frame<%>
  (interface () secret-method))

(define (dir-menu-bar-frame-mixin % #:pass-through? [pass-through? #f])
  (class* % {dir-menu-bar-frame<%>}
    (init [dir-frame #f])
    (if pass-through?
        (super-new [dir-frame dir-frame])
        (super-new))
    (new dir-menu-bar%
         [dir-frame dir-frame]
         [parent this])
    (define/public-final (secret-method)
      (void))))

(define dir-menu-bar-frame%
  (dir-menu-bar-frame-mixin frame%))


    
