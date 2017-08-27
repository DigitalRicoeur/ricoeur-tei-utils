#lang racket/gui

(require ricoeur/tei
         racket/runtime-path
         data/maybe
         xml/path
         gregor
         adjutor
         pict
         "lib.rkt"
         )

(module+ main
  (void (new splash-frame%)))

(def
  [missing-canvas%
   (pict->canvas% (red-text-pict "MISSING"))]
  [no-ricoeur-xml:id-canvas%
   (pict->canvas% (red-text-pict "No author element with xml:id=\"ricoeur\""))]
  [none-canvas%
   (pict->canvas% (red-text-pict "NONE"))])


;                                                  
;                                                  
;                                                  
;                                                  
;                   ;;;;                    ;;     
;                     ;;                    ;;     
;     ;;    ; ;;      ;;      ;;      ;;    ;; ;   
;   ;;  ;   ;;  ;     ;;     ;  ;   ;;  ;   ;;; ;  
;    ;      ;;  ;     ;;        ;;   ;      ;;  ;; 
;     ;;    ;;  ;;    ;;      ;;;;    ;;    ;;  ;; 
;       ;;  ;;  ;     ;;     ;  ;;      ;;  ;;  ;; 
;   ;   ;   ;;  ;      ;    ;;  ;;  ;   ;   ;;  ;; 
;    ;;;    ;;;;        ;;   ;;; ;   ;;;    ;;  ;; 
;           ;;                                     
;           ;;                                     
;           ;;                                     
;                                                  

(define splash-frame%
  (class frame%
    (super-new [label "TEI Lint"]
               [height (floor (* 5/4 (send photo-bitmap get-height)))])
    (inherit show)
    (define row
      (new horizontal-pane% [parent this]))
    (new message%
         [parent row]
         [label photo-bitmap])
    (define col
      (new vertical-pane% [parent row]))
    (new message%
         [parent col]
         [label "Digital Ricœur"]
         [font (make-font #:family 'system
                          #:size 24)])
    (new message%
         [parent col]
         [label "TEI Lint"]
         [font (make-font #:family 'system
                          #:size 36
                          #:weight 'bold)])
    (define e-c
      (new editor-canvas%
           [parent col]
           [style '(transparent no-border no-hscroll auto-vscroll no-focus)]))
    (define para
      (new text% [auto-wrap #t]))
    (send para insert "To begin, choose a directory containing TEI XML files.")
    (send para lock #t)
    (send e-c set-editor para)
    (new button%
         [parent col]
         [label "Choose Directory …"]
         [callback (λ (b e) (choose-directory))])
    (unless (xmllint-available?)
      (show-xmllint-warning this))
    (show #t)
    (define/private (choose-directory)
      (let ([dir (get-xml-directory this)])
        (when dir
          (show #f)
          (new directory-frame% [dir dir]))))))


;                                                                  
;                                                                  
;                                                                  
;                                                                  
;                                           ;;                     
;                                           ;;                     
;  ; ;; ;;    ;;;   ;; ;    ;;  ;;          ;;;;      ;;    ;; ;;; 
;  ;; ;; ;  ;;   ;  ;;; ;   ;;  ;;          ;;  ;    ;  ;   ;;;    
;  ;; ;; ;; ;    ;  ;;  ;;  ;;  ;;          ;;  ;       ;;  ;;     
;  ;; ;; ;;;;;;;;;; ;;  ;;  ;;  ;;          ;;  ;;    ;;;;  ;;     
;  ;; ;; ;; ;       ;;  ;;  ;;  ;;          ;;  ;    ;  ;;  ;;     
;  ;; ;; ;; ;;   ;  ;;  ;;   ; ;;;          ;;  ;   ;;  ;;  ;;     
;  ;; ;; ;;   ;;;   ;;  ;;    ; ;;          ; ;;     ;;; ;  ;;     
;                                                                  
;                                                                  
;                                                                  
;                                                                  


(define (add-file-menu mb dir-frame)
  (define m-file (new menu% [label "File"] [parent mb]))
  (new menu-item%
       [parent m-file]
       [label "Open additional directory …"]
       [callback (λ (i e)
                   (let ([dir (get-xml-directory)])
                     (when dir
                       (new directory-frame% [dir dir]))))]
       [shortcut #\O])
  (new menu-item%
       [parent m-file]
       [label "Refresh"]
       [callback (λ (i e)
                   (send dir-frame refresh-directory))]
       [shortcut #\R]))

;                                                                          
;                                                                          
;                                                                          
;                                                                          
;       ;;     ;                             ;;                            
;       ;;     ;;                            ;;                            
;    ;;;;;  ;;;;;   ;; ;;;    ;;;      ;;; ;;;;;;;   ;;;    ;; ;;; ;     ; 
;   ;   ;;     ;;   ;;;     ;;   ;   ;;   ;  ;;     ;   ;   ;;;     ;   ;  
;   ;   ;;     ;;   ;;      ;    ;   ;       ;;     ;   ;   ;;      ;   ;  
;  ;;   ;;     ;;   ;;     ;;;;;;;; ;;       ;;    ;;   ;;  ;;       ;  ;  
;   ;   ;;     ;;   ;;      ;        ;       ;;     ;   ;   ;;       ; ;   
;   ;   ;;     ;;   ;;      ;;   ;   ;;   ;   ;     ;   ;   ;;       ; ;   
;    ;;; ;     ;;   ;;        ;;;      ;;;     ;;;   ;;;    ;;        ;    
;                                                                     ;    
;                                                                    ;     
;                                                                  ;;      
;                                                                          

(define progress-gauge%
  (class gauge%
    (super-new [range 10]
               [label #f])
    (inherit get-value set-value)
    (define/public-final (++)
      (set-value (add1 (get-value))))))

(define loading-frame%
  (class frame%
    (init-field dir)
    (super-new [label (string-append "Loading "
                                     (path->string dir)
                                     "… - TEI Lint")]
               [alignment '(center top)])
    (inherit show)
    (new message%
         [parent this]
         [label (string-append "Checking files in "
                               (path->string dir)
                               " …")])
    (define progress
      (new progress-gauge%
           [parent this]))
    (show #t)
    (define/public-final (set-range v)
      (send progress set-range v))
    (define/public-final (++)
      (send progress ++))))
           


(define directory-frame%
  (class frame%
    (init-field dir)
    (super-new [label (string-append (path->string dir)
                                     " - TEI Lint")]
               [width 800]
               [height 600]
               [alignment '(left top)])
    (inherit show)
    (define file-snips
      (let ([progress (new loading-frame%
                           [dir dir])])
        (let ([row (new horizontal-pane%
                        [parent this]
                        [stretchable-height #f]
                        [alignment '(left center)])])
          (new message%
               [parent row]
               [label (path->string dir)])
          (new button%
               [parent row]
               [label "Refresh"]
               [callback (λ (b e) (refresh-directory))]))
        (define e-c
          (new editor-canvas%
               [style '(transparent auto-hscroll auto-vscroll)]
               [parent this]))
        (define ed
          (new text%))
        (send e-c set-editor ed)
        (define file-snips
          (let ([pths (for/list ([pth (in-directory dir)]
                                 #:when (xml-path? pth))
                        pth)])
            (send progress set-range (length pths))
            (define dir-valid?
              (directory-validate-xml #:quiet? #t
                                      dir))
            (for/list ([pth (in-list pths)])
              (begin0 (new file-snip%
                           [pth pth]
                           [dir dir]
                           [dir-valid? dir-valid?]
                           [dir-frame this])
                      (send progress ++)))))
        (for ([snip (in-list (sort file-snips
                                   file-snip-more-urgent?))])
          (send ed insert snip))
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
        (define mb (new menu-bar% [parent this]))
        (add-file-menu mb this)
        (send progress show #f)
        (show #t)
        file-snips))
    (define/public (refresh-directory)
      (show #f)
      (map (λ (w) (send w revoke))
           file-snips)
      (new this% [dir dir]))))

;                                  
;                                  
;                                  
;                                  
;                      ;           
;                      ;;          
;     ;;    ;; ;    ;;;;;   ; ;;   
;   ;;  ;   ;;; ;      ;;   ;;  ;  
;    ;      ;;  ;;     ;;   ;;  ;  
;     ;;    ;;  ;;     ;;   ;;  ;; 
;       ;;  ;;  ;;     ;;   ;;  ;  
;   ;   ;   ;;  ;;     ;;   ;;  ;  
;    ;;;    ;;  ;;     ;;   ;;;;   
;                           ;;     
;                           ;;     
;                           ;;     
;                                  

(define (file-snip-more-urgent? a b)
  (case (send a get-status)
    [(ok) #f]
    [(error) (not (eq? 'error (send b get-status)))]
    [(warning) (eq? 'ok (send b get-status))]))

(define file-snip%
  (class snip%
    (super-new)
    (inherit get-flags
             set-flags)
    (init-field dir
                pth
                dir-frame
                [dir-valid? #f]
                [val
                 (let ([xmllint-out (open-output-string)])
                   (cond
                     [(not (or dir-valid?
                               (parameterize ([current-output-port xmllint-out]
                                              [current-error-port xmllint-out])
                                 (valid-xml-file? #:quiet? #f pth))))
                      (xmllint-error (get-output-string xmllint-out))]
                     [else
                      (with-handlers ([exn:fail? values])
                        (call-with-input-file pth
                          read-TEI))]))]
                [frame
                 (new (if (or (xmllint-error? val)
                              (exn? val))
                          error-frame%
                          file-frame%)
                      [dir dir]
                      [pth pth]
                      [val val]
                      [dir-frame dir-frame]
                      [widget this])]
                [status (send frame get-status)]
                [maybe-title (send frame get-title)])
    (set-flags (list* 'handles-events
                      'handles-all-mouse-events
                      'hard-newline
                      (get-flags)))
    (define pth-str (path->string pth))
    (define padding 1.0)
    (define line-padding 1.0)
    (define gutter (/ STATUS_DOT_SIZE 2))
    (define extent-cache (make-hasheq))
    (define/override (size-cache-invalid)
      (hash-clear! extent-cache))
    (define/private (lookup-width dc)
      (hash-ref extent-cache
                'w
                (λ ()
                  (populate-extent-cache! dc)
                  (lookup-width dc))))
    (define/private (lookup-height dc)
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
      (match maybe-title
        [(nothing)
         (define-values {pth-w pth-h desc vspace}
           (send dc get-text-extent pth-str bold-system-font #t))
         (hash-set! extent-cache 'line1-h pth-h)
         (hash-set! extent-cache
                    'w
                    (+ (* 2 padding) STATUS_DOT_SIZE gutter pth-w))
         (hash-set! extent-cache
                    'h
                    (+ (* 2 padding) (max STATUS_DOT_SIZE pth-h)))]
        [(just title)
         (define-values {pth-w pth-h desc vspace}
           (send dc get-text-extent pth-str normal-control-font #t))
         (define-values {title-w title-h title-desc title-vspace}
           (send dc get-text-extent title bold-system-font #t))
         (hash-set! extent-cache 'line1-h title-h)
         (hash-set! extent-cache
                    'w
                    (+ (* 2 padding) STATUS_DOT_SIZE gutter (max pth-w title-w)))
         (hash-set! extent-cache
                    'h
                    (+ (* 2 padding) (max STATUS_DOT_SIZE
                                          (+ pth-h title-h line-padding))))]))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (draw-status-dot dc status (+ x padding) (+ y padding))
      (define old-font
        (send dc get-font))
      (define text-x
        (+ x padding STATUS_DOT_SIZE gutter))
      (match maybe-title
        [(nothing)
         (send dc set-font bold-system-font)
         (send dc draw-text pth-str text-x (+ y padding))]
        [(just title)
         (send dc set-font bold-system-font)
         (send dc draw-text title text-x (+ y padding))
         (send dc set-font normal-control-font)
         (send dc draw-text pth-str text-x (+ y
                                              padding
                                              (lookup-line1-height dc)
                                              line-padding))])
      (send dc set-font old-font))
    (define/override (get-extent dc x y	 	 	 	 
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
    (define mouse-state #f)
    (define/override (on-event dc x y ed-x ed-y evt)
      (case (send evt get-event-type)
        [(left-down)
         (set! mouse-state 'left-down)]
        [(left-up)
         (when mouse-state
           (send frame show #t))
         (set! mouse-state #f)]
        [else
         (set! mouse-state #f)
         (super on-event dc x y ed-x ed-y evt)]))
    (define/override (copy)
      (new this%
           [dir dir]
           [pth pth]
           [dir-frame dir-frame]
           [val val]
           [frame frame]
           [status status]
           [maybe-title maybe-title]))
    (define/public (get-status)
      status)
    (define/public (revoke)
      (send frame show #f))))

;                                          
;                                          
;                                          
;                                          
;                                          
;                                          
;     ;;;   ;; ;;;  ;; ;;;   ;;;    ;; ;;; 
;   ;;   ;  ;;;     ;;;     ;   ;   ;;;    
;   ;    ;  ;;      ;;      ;   ;   ;;     
;  ;;;;;;;; ;;      ;;     ;;   ;;  ;;     
;   ;       ;;      ;;      ;   ;   ;;     
;   ;;   ;  ;;      ;;      ;   ;   ;;     
;     ;;;   ;;      ;;       ;;;    ;;     
;                                          
;                                          
;                                          
;                                          

(define error-frame%
  (class* frame% [TEI-frame<%>]
    (init-field dir pth val widget dir-frame)
    (super-new [label (string-append (path->string pth)
                                     " - TEI Lint")]
               [alignment '(center top)]
               [width 400]
               [height 500])
    (let ([row (new horizontal-pane%
                    [parent this]
                    [alignment '(left center)])])
      (new status-canvas%
           [status 'error]
           [parent row])
      (new message%
           [parent row]
           [font bold-system-font]
           [label (path->string pth)]))
    (new message%
         [parent this]
         [label (if (xmllint-error? val)
                    "xmllint found an error."
                    "The file is invalid.")])
    (define e-c
      (new editor-canvas%
           [parent this]
           [min-height 400]
           [style '(auto-hscroll auto-vscroll)]))
    (define para
      (new text% [auto-wrap #t]))
    (send para
          insert
          (match val
            [(xmllint-error str) str]
            [(exn:fail msg _) msg]))
    (send para lock #t)
    (send e-c set-editor para)
    ;;;;;;;;;;;;;;;;;;;
    (define mb (new menu-bar% [parent this]))
    (add-file-menu mb dir-frame)
    (define m-edit (new menu% [label "Edit"] [parent mb]))
    (append-editor-operation-menu-items m-edit #t)
    ;;;;;;;;;;;;;;;;;;;
    (define/public (get-status)
      'error)
    (define/public (get-title)
      nothing)
    #|END class error-frame%|#))

;                                  
;                                  
;                                  
;                                  
;       ;;;    ;    ;;;;           
;     ;;       ;;     ;;           
;   ;;;;;;; ;;;;;     ;;      ;;;  
;     ;;       ;;     ;;    ;;   ; 
;     ;;       ;;     ;;    ;    ; 
;     ;;       ;;     ;;   ;;;;;;;;
;     ;;       ;;     ;;    ;      
;     ;;       ;;      ;    ;;   ; 
;     ;;       ;;       ;;    ;;;  
;                                  
;                                  
;                                  
;                                  

(define file-frame%
  (class* frame% [TEI-frame<%>]
    (init-field dir pth val widget dir-frame)
    (super-new [label (string-append (path->string pth)
                                     " - TEI Lint")]
               [alignment '(left top)]
               [width 400]
               [height 500])
    (define all-ok?
      #t)
    (define/public (get-status)
      (if all-ok? 'ok 'warning))
    (define title
      (send val get-title))
    (define/public (get-title)
      (just title))
    (define status-dot-canvas
      (let* ([row (new horizontal-pane%
                       [parent this]
                       [alignment '(left center)])]
             [status-dot-canvas (new status-canvas%
                                     [status 'ok]
                                     [parent row])])
        (new message%
             [parent row]
             [font bold-system-font]
             [label (path->string pth)])
        status-dot-canvas))
    ;; Title
    (let ([row (new horizontal-pane%
                    [parent this]
                    [alignment '(left top)])])
      (new message%
           [parent row]
           [font bold-system-font]
           [label "Title:"])
      (new message%
           [parent row]
           [label title]))
    ;; Citation
    (let ([row (new horizontal-pane%
                    [parent this]
                    [alignment '(left top)])])
      (new message%
           [parent row]
           [font bold-system-font]
           [label "Citation:"])
      (define e-c
        (new editor-canvas%
             [parent row]
             [style '(auto-hscroll auto-vscroll)]))
      (define para
        (new text% [auto-wrap #t]))
      (send para
            insert
            (send val get-citation))
      (send para lock #t)
      (send e-c set-editor para))
    ;; Date
    (let ([row (new horizontal-pane%
                    [parent this]
                    [alignment '(left top)])]
          [maybe-date (send val get-publication-date)])
      (new message%
           [parent row]
           [font bold-system-font]
           [label "Publication Date:"])
      (match maybe-date
        [(just dt)
         (new message%
              [parent row]
              [label (~t dt "y")])]
        [_
         (set! all-ok? #f)
         (new missing-canvas%
              [parent row])]))
    ;; "ricoeur" xml:id ??
    (unless (member "ricoeur"
                    (se-path*/list `(author #:xml:id)
                                   (send val to-xexpr)))
      (set! all-ok? #f)
      (new no-ricoeur-xml:id-canvas%
           [parent (new horizontal-pane%
                        [parent this]
                        [alignment '(left top)])]))
    ;; pages
    (let ([row (new horizontal-pane%
                    [parent this]
                    [alignment '(left top)])])
      (new message%
           [parent row]
           [font bold-system-font]
           [label "Pages:"])
      (let ([pages (send val get-page-breaks)])
        (cond
          [(null? pages)
           (set! all-ok? #f)
           (new none-canvas%
                [parent row])]
          [else
           (define e-c
             (new editor-canvas%
                  [parent row]
                  [min-height 300]
                  [style '(auto-hscroll auto-vscroll)]))
           (define para
             (new text% [auto-wrap #t]))
           (let analyze-pages ([pages pages]
                               [arabic-seen? #f])
             (cond
               [(null? pages)
                (void)]
               [else
                (define this-kind
                  (send (car pages) get-kind))
                (when arabic-seen?
                  (unless (eq? 'number this-kind)
                    (set! all-ok? #f)))
                (case this-kind
                  [(none)
                   (define-values {nones more}
                     (splitf-at pages
                                (λ (p) (eq? 'none (send p get-kind)))))
                   (define len
                     (length nones))
                   (send para
                         insert
                         (format "• ~a non-numbered page~a\n"
                                 len
                                 (if (= 1 len) "" "s")))
                   (analyze-pages more arabic-seen?)]
                  [(other)
                   (set! all-ok? #f)
                   (define-values {other more}
                     (splitf-at pages
                                (λ (p) (eq? 'other (send p get-kind)))))
                   (define len
                     (length other))
                   (send para
                         insert
                         (format "• ~a page~a with ~aunreadable number~a\n"
                                 len
                                 (if (= 1 len) "" "s")
                                 (if (= 1 len) "an " "")
                                 (if (= 1 len) "" "s")))
                   (analyze-pages more arabic-seen?)]
                  [(roman)
                   (define-values {roman more}
                     (splitf-at pages
                                (λ (p) (eq? 'roman (send p get-kind)))))
                   (handle-numeric-pages
                    roman
                    para
                    "• ~a page~a with Roman numerals from ~v to ~v\n")
                   (analyze-pages more arabic-seen?)]
                  [(number)
                   (define-values {arabic more}
                     (splitf-at pages
                                (λ (p) (eq? 'number (send p get-kind)))))
                   (handle-numeric-pages
                    arabic
                    para
                    "• ~a page~a numbered from ~v to ~v\n")
                   (analyze-pages more #t)])]))
           (send para lock #t)
           (send e-c set-editor para)])))
    ;;;;;;;;;;;;;;;;;;;
    (define mb (new menu-bar% [parent this]))
    (add-file-menu mb dir-frame)
    (define m-edit (new menu% [label "Edit"] [parent mb]))
    (append-editor-operation-menu-items m-edit #t)
    ;;;;;;;;;;;;;;;;;;;
    (unless all-ok?
      (send status-dot-canvas set-status 'warning))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/private (handle-numeric-pages pages ed format-str)
      (define groups
        (group-sequential-page-breaks pages))
      (unless (= 1 (length groups))
        (set! all-ok? #f))
      (for ([grp (in-list groups)])
        (match-define (list count from to)
          grp)
        (send ed insert (format format-str
                                count
                                (if (= 1 count) "" "s")
                                from
                                to))))
    (define/private (group-sequential-page-breaks pages)
      (def
        [init (car pages)]
        [init-string
         (from-just! (send init get-page-string))]
        [init-num
         (from-just! (send init get-numeric))])
      (let loop ([count 1]
                 [from-str init-string]
                 [to-str init-string]
                 [expect-num (add1 init-num)]
                 [pages (cdr pages)])
        (cond
          [(null? pages)
           (list (list count from-str to-str))]
          [(= expect-num (from-just! (send (car pages) get-numeric)))
           (loop (add1 count)
                 from-str
                 (from-just! (send (car pages) get-page-string))
                 (add1 expect-num)
                 (cdr pages))]
          [else
           (def
             [next (car pages)]
             [next-str (from-just! (send next get-page-string))])
           (cons (list count from-str to-str)
                 (loop 1
                       next-str
                       next-str
                       (add1 (from-just! (send next get-numeric)))
                       (cdr pages)))])))
    #|END class file-frame%|#))
