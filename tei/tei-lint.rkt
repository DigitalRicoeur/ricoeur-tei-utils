#lang racket/gui

(require ricoeur/tei
         racket/runtime-path
         data/maybe
         xml/path
         gregor
         adjutor
         pict
         )

(module+ main
  (void (new splash-frame%)))

(define-runtime-path photo-path
  "photo.png")

(define photo-bitmap
  (call-with-input-file photo-path
    read-bitmap))

(define (get-xml-directory [parent #f])
  (let ([pth (get-directory "Choose a directory containing TEI XML files."
                            parent)])
    (and pth
         (directory-exists? pth)
         pth)))

(define (show-xmllint-warning [parent #f])
  (message-box "xmllint Not Found"
               "xmllint Not Found\n\nThe program xmllint was not found. Please install libxml2 for full validation."
               parent
               '(ok caution)))

(def
  [error-bitmap
   (pict->bitmap (colorize (text "X" '(bold . modern)) "red"))]
  [warning-bitmap
   (pict->bitmap (colorize (text "?" '(bold . modern)) "orange"))]
  [ok-bitmap
   (pict->bitmap (colorize (text "√" '(bold . modern)) "green"))])

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

(struct xmllint-error (str))

(define bold-system-font
  (make-font #:family 'system
             #:weight 'bold))

(define directory-frame%
  (class frame%
    (init-field dir)
    (super-new [label (string-append (path->string dir)
                                     " - TEI Lint")]
               [alignment '(left top)])
    (inherit show)
    (let ([row (new horizontal-pane% [parent this])])
      (new message%
           [parent row]
           [label (path->string dir)])
      (new button%
           [parent row]
           [label "Refresh"]
           [callback (λ (b e) (refresh))]))
    (define file-widgets
      (for/list ([pth (in-directory dir)]
                 #:when (xml-path? pth))
        (new file-widget%
             [pth pth]
             [dir dir]
             [parent this])))
    (show #t)
    (define/private (refresh)
      (show #f)
      (map (λ (w) (send w revoke))
           file-widgets)
      (new this% [dir dir]))))


(define file-widget%
  (class horizontal-panel%
    (super-new)
    (init-field dir pth)
    (define val
      (let ([xmllint-out (open-output-string)])
        (cond
          [(not (parameterize ([current-output-port xmllint-out]
                               [current-error-port xmllint-out])
                  (valid-xml-file? #:quiet? #f pth)))
           (xmllint-error (get-output-string xmllint-out))]
          [else
           (with-handlers ([exn:fail? values])
             (call-with-input-file pth
               read-TEI))])))
    (define frame
      (new (if (or (xmllint-error? val)
                   (exn? val))
               error-frame%
               file-frame%)
           [dir dir]
           [pth pth]
           [val val]))
    (new message%
         [parent this]
         [label (case (send frame get-color)
                  [(green) ok-bitmap]
                  [(red) error-bitmap]
                  [(yellow) warning-bitmap])])
    (new message%
         [parent this]
         [label (path->string pth)])
    (define mouse-state #f)
    (define/override (on-subwindow-event r evt)
      (case (send evt get-event-type)
        [(left-down)
         (set! mouse-state 'left-down)]
        [(left-up)
         (when mouse-state
           (send frame show #t))
         (set! mouse-state #f)]
        [else
         (set! mouse-state #f)
         (super on-subwindow-event r evt)]))
    (define/public (revoke)
      (send frame show #f))))

(define error-frame%
  (class frame%
    (init-field dir pth val)
    (super-new [label (string-append (path->string pth)
                                     " - TEI Lint")]
               [alignment '(center top)]
               [width 400]
               [height 500])
    (new message%
         [parent this]
         [label (path->string pth)])
    (new message%
         [parent this]
         [label (if (xmllint-error? val)
                    "xmllint found an error."
                    "The file is invalid.")])
    (define e-c
      (new editor-canvas%
           [parent this]
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
    (define/public (get-color)
      'red)))

(define file-frame%
  (class frame%
    (init-field dir pth val)
    (super-new [label (string-append (path->string pth)
                                     " - TEI Lint")]
               [alignment '(left top)]
               [width 400]
               [height 500])
    (define all-ok?
      #t)
    (define/public (get-color)
      (if all-ok? 'green 'yellow))
    ;; Heading
    (new message%
         [parent this]
         [label (path->string pth)])
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
           [label (send val get-title)]))
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
         (new message%
              [parent row]
              [font bold-system-font]
              [label "MISSING"])]))
    ;; "ricoeur" xml:id ??
    (unless (member "ricoeur"
                    (se-path*/list `(author #:xml:id)
                                   (send val to-xexpr)))
      (set! all-ok? #f)
      (new message%
           [parent (new horizontal-pane%
                    [parent this]
                    [alignment '(left top)])]
           [font bold-system-font]
           [label "No author element with xml:id=\"ricoeur\""]))
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
           (new message%
                [parent row]
                [font bold-system-font]
                [label "NONE"])]
          [else
           (define col
             (new vertical-pane%
                  [parent row]
                  [alignment '(left top)]))
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
                   (new message%
                        [parent col]
                        [label (format "• ~a non-numbered page~a"
                                       len
                                       (if (= 1 len) "" "s"))])
                   (analyze-pages more arabic-seen?)]
                  [(other)
                   (set! all-ok? #f)
                   (define-values {other more}
                     (splitf-at pages
                                (λ (p) (eq? 'other (send p get-kind)))))
                   (define len
                     (length other))
                   (new message%
                        [parent col]
                        [label (format "• ~a page~a with ~aunreadable number~a"
                                       len
                                       (if (= 1 len) "" "s")
                                       (if (= 1 len) "an " "")
                                       (if (= 1 len) "" "s"))])
                   (analyze-pages more arabic-seen?)]
                  [(roman)
                   (define-values {roman more}
                     (splitf-at pages
                                (λ (p) (eq? 'roman (send p get-kind)))))
                   (handle-numeric-pages
                    roman
                    col
                    "• ~a page~a with Roman numerals from ~v to ~v")
                   (analyze-pages more arabic-seen?)]
                  [(number)
                   (define-values {arabic more}
                     (splitf-at pages
                                (λ (p) (eq? 'number (send p get-kind)))))
                   (handle-numeric-pages
                    arabic
                    col
                    "• ~a page~a numbered from ~v to ~v")
                   (analyze-pages more #t)])]))])))
    (define/private (handle-numeric-pages pages parent format-str)
      (define groups
        (group-sequential-page-breaks pages))
      (unless (= 1 (length groups))
        (set! all-ok? #f))
      (for ([grp (in-list groups)])
        (match-define (list count from to)
          grp)
        (new message%
             [parent parent]
             [label (format format-str
                            count
                            (if (= 1 count) "" "s")
                            from
                            to)])))
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











