#lang racket/gui

(require ricoeur/tei
         racket/runtime-path
         data/maybe
         xml/path
         gregor
         adjutor
         pict
         "lib.rkt"
         "splash.rkt"
         )

(module+ main
  (void (new splash-frame%)))

(def
  [missing-canvas%
   (pict->canvas% (red-text-pict "MISSING"))]
  [no-ricoeur-xml:id-canvas%
   (pict->canvas% (red-text-pict "No author element with xml:id=\"ricoeur\""))]
  [bad-date-order-canvas%
   (pict->canvas% (red-text-pict "Original publication date after this publication date."))]
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
  (class abstract-splash-frame%
    (super-new [label "TEI Lint"]
               [subtitle "TEI Lint"]
               [bitmap photo-bitmap]
               [height (floor (* 5/4 (send photo-bitmap get-height)))])
    (inherit show)
    (define/override-final (on-choose-directory dir)
      (show #f)
      (new directory-frame% [dir dir]))))


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


(define (add-file-menu mb dir-frame #:directory? [directory? #f])
  (define m-file (new menu% [label "File"] [parent mb]))
  (unless directory?
    (new menu-item%
         [parent m-file]
         [label "Show directory"]
         [callback (λ (i e) (send dir-frame show #t))]))
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

(define loading-frame%
  (class frame%
    (init dir
          [dir-string (path->string dir)])
    (super-new [label (string-append "Loading "
                                     dir-string
                                     "… - TEI Lint")]
               [alignment '(center top)])
    (inherit show)
    (new message%
         [parent this]
         [label (string-append "Checking files in "
                               dir-string
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
        (define ed
          (new text%))
        (new editor-canvas%
             [style '(transparent auto-hscroll auto-vscroll)]
             [parent this]
             [editor ed])
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
                                   file-snip-before?))])
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
        (add-file-menu (new menu-bar% [parent this])
                       this
                       #:directory? #t)
        (send progress show #f)
        (show #t)
        file-snips))
    (define/public (refresh-directory)
      (show #f)
      (for-each (λ (w) (send w revoke))
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

(define (status-more-urgent? a b)
  (case a
    [(ok) #f]
    [(error) (not (eq? 'error b))]
    [(warning) (eq? 'ok b)]))


(define (file-snip-before? a b)
  (let ([a-status (send a get-status)]
        [b-status (send b get-status)])
    (or (status-more-urgent? a-status b-status)
        (and (eq? a-status b-status)
             (title<? (send a get-title)
                      (send b get-title))))))

(define hand-cursor
  (make-object cursor% 'hand))

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
                          error-proto-frame%
                          file-proto-frame%)
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
    (define/override (adjust-cursor dc	 
                                    x	 
                                    y	 
                                    editorx	 
                                    editory	 
                                    event)
      hand-cursor)
    (define pth-str (path->string pth))
    (define quasi-title
      (if (or (xmllint-error? val)
              (exn? val))
          pth-str
          (send val get-title)))
    (define padding 1.0)
    (define line-padding 1.0)
    (define gutter (/ STATUS_DOT_SIZE 2))
    (define extent-cache (make-hasheq))
    (define/override-final (size-cache-invalid)
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
    (define/override-final (draw dc x y left top right bottom dx dy draw-caret)
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
    (define mouse-state #f)
    (define/override-final (on-event dc x y ed-x ed-y evt)
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
    (define/override-final (copy)
      (new this%
           [dir dir]
           [pth pth]
           [dir-frame dir-frame]
           [val val]
           [frame frame]
           [status status]
           [maybe-title maybe-title]))
    (define/public-final (get-title)
      quasi-title)
    (define/public-final (get-status)
      status)
    (define/public-final (revoke)
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

(define abstract-proto-frame%
  (class object%
    (super-new)
    (init-field dir pth val widget dir-frame)
    (abstract get-status
              get-title
              do-make-frame)
    (define real-frame #f)
    (define/private (make-frame!)
      (set! real-frame (do-make-frame)))
    (define/public-final (show [should-show? #t])
      (cond
        [should-show?
         (unless real-frame
           (make-frame!))
         (send real-frame show #t)]
        [else
         (when real-frame
           (send real-frame show #f))]))))

(define error-proto-frame%
  (class abstract-proto-frame%
    (super-new)
    (inherit-field dir pth val widget dir-frame)
    (define/override-final (do-make-frame)
      (new error-frame%
           [dir dir]
           [pth pth]
           [val val]
           [widget widget]
           [dir-frame dir-frame]))
    (define/override-final (get-status)
      'error)
    (define/override-final (get-title)
      nothing)))

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
    (let ([e-c (new editor-canvas%
                    [parent this]
                    [min-height 400]
                    [style '(auto-hscroll auto-vscroll)])])
      (define para
        (new text% [auto-wrap #t]))
      (send para
            insert
            (match val
              [(xmllint-error str) str]
              [(exn:fail msg _) msg]))
      (send para lock #t)
      (send e-c set-editor para))
    ;;;;;;;;;;;;;;;;;;;
    (let ([mb (new menu-bar% [parent this])])
      (add-file-menu mb dir-frame)
      (define m-edit (new menu% [label "Edit"] [parent mb]))
      (append-editor-operation-menu-items m-edit #t))
    ;;;;;;;;;;;;;;;;;;;
    (define/public-final (get-status)
      'error)
    (define/public-final (get-title)
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

(define file-proto-frame%
  (class abstract-proto-frame%
    (super-new)
    (inherit-field dir pth val widget dir-frame)
    (define title
      (send val get-title))
    (define-values (pages-ok? page-descriptions)
      (get-and-analyze-pages val))
    (define promise:ricoeur-xml:id-ok?
      (delay (member "ricoeur"
                     (se-path*/list `(author #:xml:id)
                                    (send val to-xexpr)))))
    (define status
      (if (and pages-ok?
               (date<=? (send val get-original-publication-date)
                        (send val get-publication-date))
               (force promise:ricoeur-xml:id-ok?))
          'ok
          'warning))
    ;; Methods
    (define/override-final (get-title)
      (just title))
    (define/override-final (get-status)
      status)
    (define/override-final (do-make-frame)
      (new file-frame%
           [dir dir]
           [pth pth]
           [val val]
           [status status]
           [promise:ricoeur-xml:id-ok? promise:ricoeur-xml:id-ok?]
           [page-descriptions page-descriptions]
           [widget widget]
           [dir-frame dir-frame]))))

(define file-frame%
  (class* frame% [TEI-frame<%>]
    (init-field dir pth val widget dir-frame
                status promise:ricoeur-xml:id-ok?
                page-descriptions)
    (super-new [label (string-append (path->string pth)
                                     " - TEI Lint")]
               [alignment '(left top)]
               [width 400]
               [height 500])
    ;; Status and Path
    (let* ([row (new horizontal-pane%
                     [parent this]
                     [alignment '(left center)])]
           [status-dot-canvas (new status-canvas%
                                   [status status]
                                   [parent row])])
      (new message%
           [parent row]
           [font bold-system-font]
           [label (path->string pth)])
      status-dot-canvas)
    ;; Title
    (define title
      (send val get-title))
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
    ;; Type
    (let ([row (new horizontal-pane%
                    [parent this]
                    [alignment '(left top)])])
      (case (send val get-book/article)
        [(book)
         (new message%
              [parent row]
              [label "Type: Book"])]
        [(article)
         (new message%
              [parent row]
              [label "Type: Article"])]))
    ;; Date
    (let ([sect (new vertical-pane%
                     [parent this]
                     [alignment '(left top)])]
          [this-dt (send val get-publication-date)]
          [orig-dt (send val get-original-publication-date)])
      (unless (date<=? orig-dt this-dt)
        (new bad-date-order-canvas%
             [parent sect]))
      (install-date-row sect "Publication Date:" this-dt)
      (install-date-row sect "Original Publication Date:" orig-dt))
    ;; "ricoeur" xml:id 
    (unless (force promise:ricoeur-xml:id-ok?)
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
      (cond
        [(null? page-descriptions)
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
         (for ([str (in-list page-descriptions)])
           (send para insert str))
         (send para lock #t)
         (send e-c set-editor para)]))
    ;;;;;;;;;;;;;;;;;;;
    (let ([mb (new menu-bar% [parent this])])
      (add-file-menu mb dir-frame)
      (define m-edit (new menu% [label "Edit"] [parent mb]))
      (append-editor-operation-menu-items m-edit #t))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public-final (get-title)
      (just title))
    (define/public-final (get-status)
      status)
    #|END class file-frame%|#))

(define (install-date-row parent str dt)
  (let ([row (new horizontal-pane%
                  [parent parent]
                  [alignment '(left top)])])
    (new message%
         [parent row]
         [font bold-system-font]
         [label str])
    (new message%
         [parent row]
         [label (~t dt "y")])))
  
(define/contract (get-and-analyze-pages val)
  (-> (is-a?/c TEI<%>)
      (values any/c (listof string?)))
  (define pages
    (send val get-page-breaks))
  (if (null? pages)
      (values #f null)
      (let analyze-pages ([pages pages]
                          [arabic-seen? #f]
                          [so-far null]
                          [all-ok? #t])
        (cond
          [(null? pages)
           (values all-ok? (reverse so-far))]
          [else
           (define this-kind
             (send (car pages) get-kind))
           (let ([all-ok? (and all-ok?
                               (if arabic-seen?
                                   (eq? 'number this-kind)
                                   #t))])
             (case this-kind
               [(none)
                (define-values {nones more}
                  (splitf-at pages
                             (λ (p) (eq? 'none (send p get-kind)))))
                (define len
                  (length nones))
                (analyze-pages more
                               arabic-seen?
                               (cons (format "• ~a non-numbered page~a\n"
                                             len
                                             (if (= 1 len) "" "s"))
                                     so-far)
                               all-ok?)]
               [(other)
                (define-values {other more}
                  (splitf-at pages
                             (λ (p) (eq? 'other (send p get-kind)))))
                (define len
                  (length other))
                (analyze-pages more
                               arabic-seen?
                               (cons (format "• ~a page~a with ~aunreadable number~a\n"
                                             len
                                             (if (= 1 len) "" "s")
                                             (if (= 1 len) "an " "")
                                             (if (= 1 len) "" "s"))
                                     so-far)
                               #f)]
               [(roman)
                (define-values {roman more}
                  (splitf-at pages
                             (λ (p) (eq? 'roman (send p get-kind)))))
                (define-values {these-ok? these-strs}
                  (handle-numeric-pages roman))
                (analyze-pages more
                               arabic-seen?
                               (append (reverse these-strs)
                                       so-far)
                               (and all-ok? these-ok?))]
               [(number)
                (define-values {arabic more}
                  (splitf-at pages
                             (λ (p) (eq? 'number (send p get-kind)))))
                (define-values {these-ok? these-strs}
                  (handle-numeric-pages arabic))
                (analyze-pages more
                               #t
                               (append (reverse these-strs)
                                       so-far)
                               (and all-ok? these-ok?))]))]))))
       

(define/contract (handle-numeric-pages pages)
  (-> (and/c list? (not/c null?)) 
      (values any/c (listof string?)))
  (define format-str
    (case (send (car pages) get-kind)
      [(roman) "• ~a page~a with Roman numerals from ~v to ~v\n"]
      [else "• ~a page~a numbered from ~v to ~v\n"]))
  (define groups
    (group-sequential-page-breaks pages))
  (values (= 1 (length groups))
          (for/list ([grp (in-list groups)])
            (match-define (list count from to)
              grp)
            (format format-str
                    count
                    (if (= 1 count) "" "s")
                    from
                    to))))

(define (group-sequential-page-breaks pages)
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

