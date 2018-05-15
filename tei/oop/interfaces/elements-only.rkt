#lang racket

(require "element.rkt"
         ricoeur/tei/oop-kernel
         )

(provide elements-only<%>
         elements-only-mixin
         )

;; Improve this:
;;   - Contract on elements-only-mixin including
;;     initialization arg contract on range
;;   - More efficient filter ?

(define elements-only<%>
  (interface (element<%>)
    [get-body/elements-only (->m (listof tei-element?))]))


(define elements-only-mixin
  (mixin {element<%>} {elements-only<%>}
    (init [body null])
    (super-new [body (filter (λ (v)
                               (or (tei-element? v)
                                   (comment? v)
                                   (p-i? v)))
                             body)])
    (define body/elements-only
      (filter tei-element? body))
    (define/public-final (get-body/elements-only)
      body/elements-only)))
