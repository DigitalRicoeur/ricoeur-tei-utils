#lang racket

(require adjutor
         ricoeur/tei/oop/objects
         ricoeur/xexpr/normalize
         )

(define current-title
  (make-parameter #f))

(define current-resp
  (make-parameter "#ricoeur"))

(define current-location-stack
  (make-parameter '()))

(define (prepare-pre-segments doc)
  #;(-> (is-a?/c TEI<%>) (listof pre-segment?))
  (parameterize ([current-title (send doc get-title)])
    (for/fold/define ([segs null]
                      [i 0]
                      [pb (tag->element '(pb))])
                     ([child (in-list
                              (send (last (send doc get-body/elements-only))
                                    get-body/elements-only))])
      (do-prepare-segments child #:segs segs #:i i #:pb pb))
    segs))

(define (get-updated-resp child)
  (or (car (dict-ref (send child get-attributes) 'resp '(#f)))
      (current-resp)))

(define (get-updated-location-stack child)
  (error 'todo))

(define (do-prepare-segments child
                             #:segs [segs null]
                             #:i [i 0]
                             #:pb [pb (tag->element '(pb))])
  (parameterize ([current-resp (get-updated-resp child)]
                 [current-location-stack (get-updated-location-stack child)])
    (case (send child get-name)
      [(ab p head note item)
       (do-prepare-segments/content child #:segs segs #:i i #:pb pb)]
      [else
       (for/fold ([segs segs]
                  [i i]
                  [pb pb])
                 ([child (in-list (send child get-body/elements-only))])
         (do-prepare-segments child #:segs segs #:i i #:pb pb))])))
#|
; not very useful b/c of pb%
(define (normalize-content-body body)
  (match body
    ['() '()]
    [(cons (? tei-element? elem) more)
     (cons elem (normalize-content-body more))]
    [(cons non-element more)
     (let normalize-textual-data ([l-str-so-far
                                   (list (non-element-xexpr->plain-text
                                          non-element))]
                                  [body more])
       (match body
         [(cons (? non-element-xexpr/c non-element)
                more)
          (normalize-textual-data
           (cons (non-element-xexpr->plain-text non-element)
                 l-str-so-far)
           more)]
         [more
          (cons (string-join (reverse l-str-so-far) "")
                (normalize-xexpr-body more))]))]))
|#

(define (do-prepare-segments/content child
                                     #:segs [segs null]
                                     #:i [i 0]
                                     #:pb [pb (tag->element '(pb))])
  (normalize-content-body (send child get-body))


    
  