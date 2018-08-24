#lang racket/gui

(require adjutor)

(provide (contract-out
          [scroll-editor-to-top
           (-> (is-a?/c editor<%>) any)]
          [constant-editor-canvas%
           any/c]
          [editor-message%
           any/c]
          [natural-height-mixin
           any/c]
          ))

(define t%
  (class text%
    (init [content '()]
          [auto-wrap #t]
          )
    (inherit insert
             begin-edit-sequence
             end-edit-sequence
             )
    (super-new [auto-wrap auto-wrap]
               )
    (define initializing? #t)
    (begin-edit-sequence)
    (if (string? content)
        (insert content)
        (for ([str (in-list content)])
          (insert str)))
    (end-edit-sequence)
    (scroll-editor-to-top this)
    (set! initializing? #f)
    (define/augment (can-delete? start len)
      #f)
    (define/augment (can-insert? start len)
      initializing?)
    #|END class t%|#))


(define constant-editor-canvas%
  (class editor-canvas%
    (init [content null]
          [auto-wrap #t]
          [line-spacing 1.0]
          [transparent #f]
          [auto-hscroll #t]
          [auto-vscroll #t]
          [style null]
          )
    (define t
      (new t%
           [content content]
           [auto-wrap auto-wrap]
           [line-spacing line-spacing]))
    (super-new [style (let* ([style (if auto-hscroll
                                        (cons 'auto-hscroll style)
                                        style)]
                             [style (if auto-vscroll
                                        (cons 'auto-vscroll style)
                                        style)])
                        (if transparent
                            (cons 'transparent style)
                            style))]
               [editor t])
    (inherit vertical-inset
             horizontal-inset
             min-height
             )
    (define/public (set-natural-height)
      (min-height (inexact->exact (get-natural-height))))
    (define/public (get-natural-width) ;; maybe not necessary ?
      (define-values (w h)
        (get-text-extent))
      (+ w (* 2 (horizontal-inset)) 5))
    (define/public (get-natural-height)
      (define-values (w h)
        (get-text-extent))
      (+ h (* 2 (vertical-inset)) 5)) ;; don't know why 5, but it fixes problems
    (define/public (get-text-extent)
      (let ([w (box 0)]
            [h (box 0)])
        (send t get-extent w h)
        (values (unbox w) (unbox h))))
    #|END class constant-editor-canvas%|#))



(define (scroll-editor-to-top ed)
  (let loop ([wait 1])
    (cond
      [(send ed locked-for-flow?)
       (unless (infix: wait > 5)
         (sleep wait)
         (loop (add1 wait)))]
      [else
       (send ed scroll-to-position 0)])))


(define natural-height-mixin
  (mixin {(class->interface constant-editor-canvas%)} {}
    (super-new)
    (inherit set-natural-height
             get-parent
             get-top-level-window
             )
    (let ([parent (get-top-level-window)])
      (when parent
        (TODO/void reflow-container bugs)
        #|
ed.rkt:110:8: sequence-contract-violation: negative:
 method set-max-width cannot be called, except in states
(unlocked write-lock), args 441|#
        (send parent reflow-container))
      (set-natural-height)
      (when parent
        (send parent reflow-container)))
    #|END natural-height-mixin|#))


(define editor-message%
  (class (natural-height-mixin constant-editor-canvas%)
    (super-new [auto-hscroll #f]
               [transparent #t]
               [style '(no-border no-hscroll no-focus)])
    #|END class editor-message%|#))



#|
(module+ main
  (define f
    (new frame%
         [label "Editor Test"]))

  (define str
    ;"Apples are very delicious. I like to eat them in the autumn, and in the winter, and in summer afternoons. ")
    "/Users/philip/code/ricoeur/texts/TEI/reflections_on_a_new_ethos_for_Europe,_in_Kearney-Paul_Ricoeur__The_Hermeneutics_of_Action,_pp3-13.xml")
  (define e-c
    (new constant-editor-canvas%
         [parent f]
         [content str #;(make-list 20 str)]
         ))

  

  (send f show #t))
|#

