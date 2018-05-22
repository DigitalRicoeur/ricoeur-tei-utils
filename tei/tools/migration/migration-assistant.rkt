#lang racket/gui

(require ricoeur/tei/oop
         "icon.rkt"
         "../tei-lint/splash.rkt"
         "../tei-lint/lib.rkt"
         adjutor
         framework
         )

(module+ main
  (void (new splash%)))

(define splash%
  (class splash-frame/no-button%
    (super-new [label "DR Migration Assistant"]
               [subtitle "Migration Assistant"]
               [message "Nothing needs to be migrated right now."]
               [bitmap (make-DR-bitmap 150 make-screen-bitmap
                                       #:transparent? #t)])
    (define/override (on-initialize-col col)
      (new button%
           [parent col]
           [label "Quit"]
           [callback (λ (b e) (exit))]))
    #|(define/override-final (on-choose-directory dir)
      (new loading-frame% [dir dir])
      (show #f))|#
    #|END splash%|#))

#|
(define document%
  (class* object% {TEI-info<%>}
    (super-new)
    (init [(:doc doc)]
          [(:path path)])
    (define/TEI-info doc :doc)
    (define path :path)
    (when (get-book/article)
      (set! doc (send doc get-teiHeader)))
    (define/public-final (get-path)
      path)
    (define/public-final (set-book/article! which)
      (define updated
        (add-book/article doc which))
      (with-output-to-file path
        #:exists 'replace
        (λ () (send updated write-TEI)))
      (set! doc (send updated get-teiHeader)))))


(define loading-frame%
  (class frame%
    (init dir
          [dir-string (path->string dir)])
    (super-new [label (string-append "Loading "
                                     dir-string
                                     "… - DR Migration Assistant")]
               [alignment '(center top)])
    (inherit show)
    (new message%
         [parent this]
         [label (string-append "Loading "
                               dir-string
                               " …")])
    (define progress
      (new progress-gauge%
           [parent this]))
    (show #t)
    (define/public-final (set-range v)
      (send progress set-range v))
    (define/public-final (++)
      (send progress ++))
    (let ([pths (for/list ([pth (in-directory dir)]
                           #:when (xml-path? pth))
                  pth)])
      (set-range (add1 (length pths)))
      (for*/fold/define ([todo '()]
                         [books '()]
                         [articles '()])
                        ([pth (in-list pths)]
                         [obj (in-value
                               (with-handlers ([exn:fail? not])
                                 (file->TEI pth)))]
                         #:when (or obj (not (++)))
                         [doc (in-value
                               (new document%
                                    [doc obj]
                                    [path pth]))])
        (begin0 (case (send obj get-book/article)
                  [(#f)
                   (values (cons doc todo)
                           books
                           articles)]
                  [(book)
                   (values todo
                           (cons doc books)
                           articles)]
                  [(article)
                   (values todo
                           books
                           (cons doc articles))])
                (++)))
      (if (null? todo)
          (new finished-frame%
               [dir dir]
               [dir-string dir-string]
               [books books]
               [articles articles]
               [maybe-loading-frame this])
          (new working-frame%
               [dir dir]
               [dir-string dir-string]
               [todo todo]
               [books books]
               [articles articles]
               [maybe-loading-frame this])))))
|#

(define (sort-documents docs)
  (sort docs title<? #:key (λ (doc) (send doc get-title))))

(define proto-result-frame%
  (class frame%
    (init-field dir
                dir-string)
    (inherit show)
    (super-new [label (string-append dir-string
                                     " - DR Migration Assistant")]
               [width 800]
               [height 800]
               [alignment '(left top)])
    (let ([row (new horizontal-pane%
                    [parent this]
                    [stretchable-height #f]
                    [alignment '(left center)])])
      (new message%
           [parent row]
           [label dir-string])
      (new button%
           [parent row]
           [label "Refresh"]
           [callback (λ (b e) (refresh-dir))]))
    (define/public (refresh-dir)
      (new (error 'loading-frame%) ;loading-frame%
           [dir dir]
           [dir-string dir-string])
      (show #f))))

(define finished-frame%
  (class proto-result-frame%
    (super-new)
    (inherit show)
    (init [(:books books)]
          [(:articles articles)]
          [maybe-loading-frame #f])
    (define books
      (sort-documents :books))
    (define articles
      (sort-documents :articles))
    (let ([row (new horizontal-pane%
                    [parent this]
                    [stretchable-height #f]
                    [alignment '(left center)])])
      (new message%
           [parent row]
           [font bold-system-font]
           [label "This directory is finished!"]))
    (let ([row (new panel:horizontal-dragable% 
                    [parent this]
                    [alignment '(left center)])])
      (let ([sub (new vertical-pane%
                      [parent row])])
        (new message%
             [parent sub]
             [font bold-system-font]
             [label "Books"])
        (insert-snip-ed sub books))
      (let ([sub (new vertical-pane%
                      [parent row])])
        (new message%
             [parent sub]
             [font bold-system-font]
             [label "Articles"])
        (insert-snip-ed sub articles)))
    ;;;;
    (when maybe-loading-frame
      (send maybe-loading-frame ++)
      (send maybe-loading-frame show #f))
    (show #t)))

    
(define working-frame%
  (class proto-result-frame%
    (inherit-field dir
                   dir-string)
    (init [(:todo todo)]
          [(:books books)]
          [(:articles articles)]
          [maybe-loading-frame #f])
    (super-new)
    (inherit show)
    (define todo
      (sort-documents :todo))
    (define books
      (sort-documents :books))
    (define articles
      (sort-documents :articles))
    (define selection
      (mutable-set))
    (define clear-button
      (let ([row (new horizontal-pane%
                      [parent this]
                      [stretchable-height #f]
                      [alignment '(left center)])])
        (new button%
             [label "Clear Selection"]
             [parent row]
             [enabled #f]
             [callback (λ (b e)
                         (set-clear! selection)
                         (enable-buttons #f)
                         (for ([sn (in-list select-snips)])
                           (send sn un-select)))])))
    (define mark-row
      (new horizontal-pane%
           [parent this]
           [stretchable-height #f]
           [alignment '(left center)]))
    (new message%
         [parent mark-row]
         [label "Mark selected as: "]
         [font bold-system-font])
    (define books-button
      (new button%
           [label "Books"]
           [parent mark-row]
           [enabled #f]
           [callback (make-mark-callback 'book)]))
    (define articles-button
      (new button%
           [label "Articles"]
           [parent mark-row]
           [enabled #f]
           [callback (make-mark-callback 'article)]))
    (define/private ((make-mark-callback which) b e)
      (new confirm%
           [which which]
           [dir dir]
           [dir-string dir-string]
           [todo todo]
           [books books]
           [articles articles]
           [selection selection]
           [working-frame this]))
    (define eds-row
      (new panel:horizontal-dragable% 
           [parent this]
           [alignment '(left center)]))
    (define select-snips
      (let ([col (new vertical-pane%
                      [parent eds-row])])
        (new message%
             [parent col]
             [font bold-system-font]
             [label "To Do:"])
        (insert-snip-ed col todo (λ (doc)
                                   (new select-snip%
                                        [working-frame this]
                                        [doc doc])))))
    (let ([col (new panel:vertical-dragable% 
                    [parent eds-row])])
      (let ([sub (new vertical-pane%
                      [parent col])])
        (new message%
             [parent sub]
             [font bold-system-font]
             [label "Books"])
        (insert-snip-ed sub books))
      (let ([sub (new vertical-pane%
                      [parent col])])
        (new message%
             [parent sub]
             [font bold-system-font]
             [label "Articles"])
        (insert-snip-ed sub articles)))
    (define/private (enable-buttons state)
      (send books-button enable state)
      (send articles-button enable state)
      (send clear-button enable state))
    (define/public (select-doc doc)
      (set-add! selection doc)
      (enable-buttons #t))
    (define/public (un-select-doc doc)
      (set-remove! selection doc)
      (when (set-empty? selection)
        (enable-buttons #f)))
    ;;;;
    (when maybe-loading-frame
      (send maybe-loading-frame ++)
      (send maybe-loading-frame show #f))
    (show #t)))



(define std-pen
  (make-pen #:immutable? #t
            #:width 0 ;1
            #:style 'transparent
            #:join 'miter))

(define std-brush
  (make-brush #:immutable? #t
              #:style 'transparent))

(define selected-brush
  (make-brush #:immutable? #t
              #:color "yellow"))

(define doc-snip%
  (class* snip% {TEI-info<%>}
    (super-new)
    (init [(:doc doc)])
    (field [current-brush std-brush])
    (inherit get-flags
             set-flags)
    (set-flags (cons 'hard-newline
                     (get-flags)))
    (define/TEI-info doc :doc)
    (define/public (get-doc)
      doc)
    (define title
      (get-title))
    (define pth-str
      (path->string (send doc get-path)))
    (define box-padding 3.5)
    (define padding (* 2 box-padding))
    (define line-padding 1.0)
    (define extent-cache (make-hasheq))
    (define/override-final (size-cache-invalid)
      (hash-clear! extent-cache))
    (define/public (lookup-width dc)
      (hash-ref extent-cache
                'w
                (λ ()
                  (populate-extent-cache! dc)
                  (lookup-width dc))))
    (define/public (lookup-height dc)
      (hash-ref extent-cache
                'h
                (λ ()
                  (populate-extent-cache! dc)
                  (lookup-height dc))))
    (define/private (lookup-line1-height dc)
      (hash-ref extent-cache
                'line1-h
                (λ ()
                  (populate-extent-cache! dc)
                  (lookup-line1-height dc))))
    (define/private (populate-extent-cache! dc)
      (define-values {pth-w pth-h desc vspace}
        (send dc get-text-extent pth-str normal-control-font #t))
      (define-values {title-w title-h title-desc title-vspace}
        (send dc get-text-extent title bold-system-font #t))
      (hash-set! extent-cache 'line1-h title-h)
      (hash-set! extent-cache
                 'w
                 (+ (* 2 padding) (max pth-w title-w)))
      (hash-set! extent-cache
                 'h
                 (+ (* 2 padding) (+ pth-h title-h line-padding))))
    (define/override-final (get-extent dc x y	 	 	 	 
                                       [w #f]
                                       [h #f]
                                       [descent #f]
                                       [space #f]
                                       [lspace #f]
                                       [rspace #f])
      (define-syntax-rule (maybe-set-boxes! [b v] ...)
        (begin (when b (set-box! b v)) ...))
      (maybe-set-boxes!
       [w (lookup-width dc)]
       [h (lookup-height dc)]
       [descent padding]
       [space padding]
       [lspace padding]
       [rspace padding]))
    (define/override (copy)
      (new this%
           [doc doc]))
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define old-font
        (send dc get-font))
      (define old-pen
        (send dc get-pen))
      (define old-brush
        (send dc get-brush))
      (define text-x
        (+ x padding))
      (send* dc
        (set-pen std-pen)
        (set-brush current-brush)
        (draw-rectangle (+ x box-padding)
                        (+ y box-padding)
                        (- (lookup-width dc)
                           (* 2 box-padding))
                        (- (lookup-height dc)
                           (* 2 box-padding)))
        (set-font bold-system-font)
        (draw-text title text-x (+ y padding))
        (set-font normal-control-font)
        (draw-text pth-str text-x (+ y
                                     padding
                                     (lookup-line1-height dc)
                                     line-padding))
        (set-font old-font)
        (set-pen old-pen)
        (set-brush old-brush)))))


(define hand-cursor
  (make-object cursor% 'hand))

(define select-snip%
  (class doc-snip%
    (super-new)
    (init-field working-frame)
    (inherit-field current-brush)
    (inherit get-flags
             set-flags
             get-admin
             lookup-width
             lookup-height
             get-doc
             )
    (set-flags (list* 'handles-events
                      'handles-all-mouse-events
                      (get-flags)))
    (define/override (adjust-cursor dc	 
                                    x	 
                                    y	 
                                    editorx	 
                                    editory	 
                                    event)
      hand-cursor)
    (define mouse-state #f)
    (define/override-final (on-event dc x y ed-x ed-y evt)
      (case (send evt get-event-type)
        [(left-down)
         (set! mouse-state 'left-down)]
        [(left-up)
         (when mouse-state
           (handle-click))
         (set! mouse-state #f)]
        [else
         (set! mouse-state #f)
         (super on-event dc x y ed-x ed-y evt)]))
    (define/public (un-select)
      (when selected?
        (set! selected? #f)
        (set! current-brush std-brush)
        (re-draw)))
    (define selected? #f)
    (define/private (handle-click)
      (cond
        [selected?
         (set! selected? #f)
         (set! current-brush std-brush)
         (send working-frame un-select-doc (get-doc))]
        [else
         (set! selected? #t)
         (set! current-brush selected-brush)
         (send working-frame select-doc (get-doc))])
      (re-draw))
    (define/private (re-draw)
      (let ([admin (get-admin)])
        (when admin
          (let ([dc (send admin get-dc)])
            (when dc
              (send admin
                    needs-update
                    this
                    0
                    0
                    (lookup-width dc)
                    (lookup-height dc)))))))))
    
(define (insert-snip-ed parent docs [doc->snip (λ (doc)
                                                 (new doc-snip%
                                                      [doc doc]))])
  (define ed
    (new text%))
  (new editor-canvas%
       [style '(transparent auto-hscroll auto-vscroll)]
       [parent parent]
       [editor ed])
  (define snips
    (for/list ([doc (in-list docs)])
      (define sn (doc->snip doc))
      (send ed insert sn)
      sn))
  (let loop ([wait 1])
    (cond
      [(send ed locked-for-flow?)
       (unless [wait . > . 5]
         (sleep wait)
         (loop (add1 wait)))]
      [else
       (send ed scroll-to-position 0)]))
  (send ed lock #t)
  (send ed hide-caret #t)
  snips)





(define confirm%
  (class dialog%
    (init-field which
                dir
                dir-string
                todo
                books
                articles
                selection
                working-frame)
    (super-new [parent working-frame]
               [label (string-append
                       "Confirm Marking As "
                       (case which
                         [(book) "Books"]
                         [else "Articles"]))]
               [alignment '(left top)]
               [width
                (inexact->exact
                 (floor (* 2/3 (send working-frame get-width))))]
               [height
                (inexact->exact
                 (floor (* 2/3 (send working-frame get-height))))])
    (inherit show)
    (new message%
         [parent this]
         [font bold-system-font]
         [label (string-append
                 "Are you sure you want to mark the following items as "
                 (case which
                   [(book) "books"]
                   [else "articles"])
                 "?")])
    (define l-selection
      (sort-documents (set->list selection)))
    (insert-snip-ed this l-selection)
    (let ([row (new horizontal-pane%
                    [parent this]
                    [alignment '(right center)]
                    [stretchable-height #f])])
      (gui-utils:ok/cancel-buttons
       row
       (λ (b e) (confirm))
       (λ (b e) (cancel))
       "Save Changes"))
    (show #t)
    (define/public (confirm)
      (show #f)
      (send working-frame show #f)
      (define progress
        (new saving-changes%
             [count (length l-selection)]))
      (for ([doc (in-list l-selection)])
        (send doc set-book/article! which)
        (send progress ++))
      (define new-todo
        (for/list ([doc (in-list todo)]
                   #:unless (set-member? selection doc))
          doc))
      (define-values (new-books new-articles)
        (case which
          [(book)
           (values (append l-selection books)
                   articles)]
          [else
           (values books
                   (append l-selection articles))]))
      (if (null? new-todo)
          (new finished-frame%
               [dir dir]
               [dir-string dir-string]
               [books new-books]
               [articles new-articles]
               [maybe-loading-frame progress])
          (new working-frame%
               [dir dir]
               [dir-string dir-string]
               [todo new-todo]
               [books new-books]
               [articles new-articles]
               [maybe-loading-frame progress])))
    (define/public (cancel)
      (show #f))))
     



(define saving-changes%
  (class frame%
    (init count)
    (super-new [label "Saving Changes … | DR Migration Assistant"])
    (inherit show)
    (new message%
         [parent this]
         [label "Saving Changes …"])
    (define progress
      (new progress-gauge%
           [parent this]))
    (set-range (add1 count))
    (show #t)
    (define/public-final (set-range v)
      (send progress set-range v))
    (define/public-final (++)
      (send progress ++))))













