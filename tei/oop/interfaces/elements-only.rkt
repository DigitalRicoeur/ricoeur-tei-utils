#lang racket

(require "element.rkt"
         xml
         )

(provide elements-only<%>
         elements-only-mixin
         )

;; Improve this:
;;   - Contract on elements-only-mixin including
;;     initialization arg contract on range
;;   - More efficient filter ?
;;   - Is delay really good?

(define elements-only<%>
  (interface (element<%>)
    [get-body/elements-only (->m (listof tei-element?))]))


(define elements-only-mixin
  (mixin {element<%>} {elements-only<%>}
    (init [body null])
    (super-new [body (filter (or/c tei-element?
                                   comment?
                                   p-i?)
                             body)])
    (define pr:body/elements-only
      (delay/sync (filter tei-element? body))) 
    (define/public-final (get-body/elements-only)
      (force pr:body/elements-only))))
