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
                    [maybe-title (maybe/c string-immutable/c)]
                    [make-frame (-> (is-a?/c frame%))]
                    [dir-frame dir-frame/false/c]))]
            )))

(define proto-frame<%>
  (interface (lint-status<%> get-dir-frame/false<%>)
    [use-fresh-eventspace? (->m any/c)]
    [show! (->m any)]))

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
    (define initialize-sema (make-semaphore 1))
    (define (frame/thunk)
      (cond
        [dir-frame
         (send dir-frame
               call-in-directory-context
               do-make-frame
               (use-fresh-eventspace?))]
        [else
         (do-make-frame)]))
    (define/public-final (show!)
      (when (procedure? frame/thunk)
        (call-with-semaphore initialize-sema
          (λ ()
            (when (procedure? frame/thunk)
              (set! frame/thunk (frame/thunk))))))
      (send frame/thunk show #t))
    (define/public (use-fresh-eventspace?)
      #false) 
    (define/public-final (get-dir-frame/false)
      dir-frame)
    (define/public-final (get-lint-status)
      lint-status)
    (define/public-final (get-maybe-title)
      maybe-title)
    #|END class proto-frame%|#))

