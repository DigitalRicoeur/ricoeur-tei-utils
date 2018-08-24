#lang racket/gui

(require framework
         ricoeur/tei/base
         "ed.rkt"
         "xml-color.rkt"
         )

(provide (contract-out
          [xml-preview-text%
           (subclass?/c text%)]
          ))

;; This seems to work elsewhere, but the overlap
;; problem happens in main.

;; (icon:get-autowrap-bitmap)

(define markup-color
  (make-color 128 0 0))

(define basic-name
  (let ([sl (editor:get-standard-style-list)])
    (define basic
      (send sl basic-style))
    (define entity-style
      (send sl
            find-or-create-style
            basic
            (let ([delta (make-object style-delta% 'change-nothing)])
              (send delta set-delta-foreground markup-color))))
    (define tag-style
      (send sl
            find-or-create-style
            entity-style
            (make-object style-delta% 'change-bold)))       
    (send sl
          replace-named-style
          "ricoeur:xml-entity"
          entity-style)
    (send sl
          replace-named-style
          "ricoeur:xml-tag"
          tag-style)
    "Standard"))


(define xml-preview-text%
  (class (color:text-mixin
          (text:line-numbers-mixin
           (editor:standard-style-list-mixin
            text:basic%)))
    (super-new [auto-wrap #t]
               [line-spacing 1.0])
    (inherit show-line-numbers!
             set-line-numbers-color ;trying this seems to trigger a bug
             begin-edit-sequence
             end-edit-sequence
             insert
             start-colorer)
    ;; do a dance with show-line-numbers! to prevent them
    ;; from covering things
    (show-line-numbers! #f)
    (init doc)
    (define initializing? #t)
    (let ()
      (begin-edit-sequence)
      (insert
       (with-output-to-string
         (λ () (write-tei-document doc))))
      (end-edit-sequence))
    (set! initializing? #f)
    (scroll-editor-to-top this)
    (start-colorer token-sym->style tokenize-xml null)
    (show-line-numbers! #t)
    (define/augment (can-delete? start len)
      #f)
    (define/augment (can-insert? start len)
      initializing?)
    #|END xml-preview-text%|#))



(module+ main
  (define f
    (new frame%
         [label "XML Preview"]
         [width 400]
         [height 400]))

  (define ed
    (new xml-preview-text%
         [doc (file->tei-document "/Users/philip/code/ricoeur/texts/TEI/living_up_to_Death.xml" #;"function_of_fiction_in_shaping_reality.xml")]))

  (void
   (new editor-canvas%
        [parent f]
        [style '(auto-hscroll auto-vscroll)]
        [editor ed]))

  (send f show #t)
  #|END module+ main|#)



