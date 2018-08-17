#lang racket/gui

(require framework
         ricoeur/tei/base
         "../lib.rkt"
         "interfaces.rkt"
         "menu.rkt"
         )

(provide proto-frame<%>)

(module+ private
  (provide (contract-out
            [proto-frame%
             (class/c
              (init [lint-status lint-status/c]
                    [maybe-title (maybe/c string?)]
                    [make-frame (-> (is-a?/c frame%))]
                    [dir-frame dir-frame/false/c]))]
            )))

(define proto-frame<%>
  (interface (lint-status<%> get-dir-frame/false<%>)
    [show (->m any/c any)]))

(define proto-frame%
  (class* object% {proto-frame<%>}
    (super-new)
    (init [(status lint-status)]
          [(t maybe-title)]
          [(do-make-frame make-frame)]
          [(d dir-frame) #f])
    (def
      [dir-frame d]
      [lint-status status]
      [maybe-title t])
    (define pr:frame
      (delay/sync
       (if dir-frame
           (send dir-frame
                 call-in-directory-context
                 do-make-frame)
           (do-make-frame))))
    (define/public-final (show should-show?)
      (if should-show?
          (send (force pr:frame) show #t)
          (when (or (promise-forced? pr:frame)
                    (promise-running? pr:frame))
            (send (force pr:frame) show #f))))
    (define/public-final (get-dir-frame/false)
      dir-frame)
    (define/public-final (get-lint-status)
      lint-status)
    (define/public-final (get-maybe-title)
      maybe-title)
    #|END class proto-frame%|#))

