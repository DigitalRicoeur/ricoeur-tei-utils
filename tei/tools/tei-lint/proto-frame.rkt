#lang racket/gui

(require framework
         ricoeur/tei/base
         "lib.rkt"
         "interfaces.rkt"
         )

(provide (contract-out
          [proto-frame%
           (class/c
            (init [lint-status lint-status/c]
                  [maybe-title (maybe/c string?)]
                  [make-frame (-> (is-a?/c frame%))]))]
          ))

(define proto-frame%
  (class* object% {lint-status<%>}
    (super-new)
    (init [(status lint-status)]
          [(t maybe-title)]
          [(do-make-frame make-frame)])
    (def
      [lint-status status]
      [maybe-title t])
    (define pr:frame
      (delay/sync (do-make-frame)))
    (define/public-final (show should-show?)
      (if should-show?
          (send (force pr:frame) show #t)
          (when (or (promise-forced? pr:frame)
                    (promise-running? pr:frame))
            (send (force pr:frame) show #f))))
    (define/public-final (get-lint-status)
      lint-status)
    (define/public-final (get-maybe-title)
      maybe-title)
    #|END class proto-frame%|#))

            