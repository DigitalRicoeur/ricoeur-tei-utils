#lang racket/gui

(require ricoeur/tei/tools/tei-lint/lib
         ricoeur/tei
         framework
         adjutor
         data/maybe
         roman-numeral)

(provide (contract-out
          [pages-panel%
           (class/c
            (init [string string?])
            [ready? (->m boolean?)]
            [get-ab-xexpr (->m (or/c #f (tei-xexpr/c ab)))]
            [get-description-strings (->m (listof string-immutable/c))])]
          ))

(define (split-pages str)
  (map string->immutable-string
       (regexp-split #rx"\f" str)))

(struct pre-page (maybe-n string)
  ;; maybe-n: (maybe/c string-immutable/c)
  ;; string: string-immutable/c
  #:transparent)

(struct unnumbered-group (these)
  ;; these: (non-empty-listof pre-page?)
  #:transparent)

(struct roman-group (init case these)
  ;; init: exact-positive-integer?
  ;; case: (or/c 'lower 'upper)
  ;; these: (non-empty-listof pre-page?)
  #:transparent)

(struct arabic-group (init these)
  ;; init: exact-positive-integer?
  ;; these: (non-empty-listof pre-page?)
  #:transparent)

(define page-group-pre-pages
  (match-lambda
    [(or (unnumbered-group these)
         (roman-group _ _ these)
         (arabic-group _ these))
     these]))

(define page-group->string
  (match-lambda
    [(unnumbered-group (app length count))
     (if (= 1 count)
         "• 1 page without a number."
         (format "• ~a pages without numbers." count))]
    [(roman-group init case (app length count))
     (parameterize ([current-roman-numeral-case case])
       (if (= 1 count)
           (format "• 1 page with Roman numeral ~v." (number->roman init))
           (format "• ~a pages with Roman numerals from ~v to ~v."
                   count
                   (number->roman init)
                   (number->roman (+ init -1 count)))))]
    [(arabic-group init (app length count))
     (if (= 1 count)
         (format "• 1 page numbered ~a." init)
         (format "• ~a pages numbered from ~a to ~a."
                 count
                 init
                 (+ init -1 count)))]))

(struct page-group-state (group to-go prev)
  ;; to-go: (listof string-immutable/c)
  ;; group: (or/c #f unnumbered-group? roman-group? arabic-group?)
  ;; prev: (or/c #f page-group-state?)
  ;;   prev and group are both #f for initial state;
  ;;   neither is #f otherwise
  #:transparent)        

(define page-group-state-finished?
  (match-lambda
    [(page-group-state _ '() _)
     #t]
    [_
     #f]))

(define page-group-state-initial?
  (match-lambda
    [(page-group-state #f _ _)
     #t]
    [_
     #f]))

(define (count-pages-to-go p-g-s)
  (length (page-group-state-to-go p-g-s)))

(define (describe-page-group-state state)
  (let loop ([state state]
             [so-far null])
    (match state
      [(page-group-state #f _ #f)
       so-far]
      [(page-group-state group _ state)
       (loop state
             (cons (string->immutable-string
                    (page-group->string group))
                   so-far))])))

(define (page-group-state->xexpr-forest state)
  (let loop ([state state]
             [so-far null])
    (match state
      [(page-group-state #f _ #f)
       so-far]
      [(page-group-state (or (unnumbered-group these)
                             (roman-group _ _ these)
                             (arabic-group _ these))
                         _
                         state)
       (loop state
             (for/fold ([so-far so-far])
                       ([this (in-list (reverse these))])
               (match-define (pre-page maybe-n string)
                 this)
               (list* (match maybe-n
                        [(just n)
                         `(pb ([n ,n]))]
                        [_
                         '(pb ())])
                      string
                      so-far)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pre-page-display-widget%
  (class vertical-pane%
    (init label
          [(val pre-page)])
    (super-new [alignment '(left top)])
    (when label
      (new message%
           [label label]
           [font bold-system-font]
           [parent this]))
    (new message%
         [label (match val
                  [(pre-page (just n) _)
                   (string-append "Number: " n)]
                  [_
                   "Not numbered."])]
         [parent this])
    (new constant-editor-canvas%
         [content (pre-page-string val)]
         [parent this])
    #|END class pre-page-display-widget%|#))

(define confirm-page-group-dialog%
  (class dialog%
    (inherit show)
    (init [(grp group)]
          [(_bx bx:result) (box #f)])
    (define bx:result _bx)
    (super-new [label "Confirm Adding Pages"]
               [style '(resize-border)]
               [alignment '(center top)])
    (new message%
         [label "Confirm Adding Pages"]
         [font big-bold-system-font]
         [parent this])
    (let ([body (new vertical-pane%
                     [alignment '(left top)]
                     [parent this])])
      (define l-pre-pages
        (page-group-pre-pages grp))
      (define count
        (length l-pre-pages))
      (new message%
           [label (if (= 1 count)
                      "Please confirm adding a single page."
                      (format "Please confirm adding ~a pages." count))]
           [parent body])
      (cond
        [(= 1 count)
         (new pre-page-display-widget%
              [label #f]
              [pre-page (car l-pre-pages)]
              [parent body])]
        [else
         (define row
           (new panel:horizontal-dragable%
                [parent body]))
         (new pre-page-display-widget%
              [label "Initial Page:"]
              [pre-page (car l-pre-pages)]
              [parent row])
         (new pre-page-display-widget%
              [label "Final Page:"]
              [pre-page (last l-pre-pages)]
              [parent row])])
      (gui-utils:ok/cancel-buttons
       (new horizontal-pane%
            [alignment '(right center)]
            [stretchable-height #f]
            [parent body])
       (λ (b e) (return-result 'confirm))
       (λ (b e) (return-result #f))
       "Add Pages"))
    (define/public-final (return-result rslt)
      (set-box! bx:result rslt)
      (show #f))
    #|END class confirm-page-group-dialog%|#))

(define (confirm-add-page-group grp [parent #f])
  (define (go)
    (define bx:result
      (box #f))
    (define it
      (new confirm-page-group-dialog%
           [parent parent]
           [group grp]
           [bx:result bx:result]))
    (send it show #t)
    (unbox bx:result))
  (TODO/void confirm-add-page-group:
             see call-in-eventspace-thread from
             ricoeur/tei/tools/tei-lint/paragraphs/common)
  (define es
    (if parent 
        (send parent get-eventspace)
        (current-eventspace)))
  (if (eq? (current-thread) (eventspace-handler-thread es))
      ;; In the right thread:
      (go)
      ;; Not in the right thread:
      (let ([ch (make-channel)])
        (parameterize ([current-eventspace es])
          (queue-callback
           (λ () (channel-put ch (go)))))
        (channel-get ch))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define num-to-go-widget%
  (class vertical-panel%
    (inherit add-child delete-child)
    (init [(_n n)])
    (define n _n)
    (super-new [alignment '(left top)])
    (define non-zero-row
      (new horizontal-panel%
           [style (if (= 0 n)
                      '(deleted)
                      null)]
           [parent this]))
    (new message%
         [label "Pages to go:"]
         [font bold-system-font]
         [parent non-zero-row])
    (define num-message
      (new message%
           [label (number->string n)]
           [parent non-zero-row]))
    (define finished-message
      (new message%
           [label "All page numbers have been assigned."]
           [font bold-system-font]
           [style (if (= 0 n)
                      null
                      '(deleted))]
           [parent this]))
    (define/public (set-n! new-n)
      (cond
        [(= 0 new-n)
         (unless (= 0 n)
           (delete-child non-zero-row)
           (add-child finished-message))]
        [else
         (send num-message set-label (number->string new-n))
         (when (= 0 n)
           (delete-child finished-message)
           (add-child non-zero-row))])
      (set! n new-n))
    #|END class num-to-go-widget%|#))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-lexical-member-names id ...)
  (begin (define-member-name id (generate-member-key)) ...))

(define-lexical-member-names
  initialize-prelude-children
  on-add-clicked
  invoke-callback
  reset!
  show-invalid-message!
  hide-invalid-message!)

(define positive-integer-field%
  (class text-field%
    (inherit get-value set-value set-field-background)
    (define ui-shows-invalid? #f)
    (init [label #f]
          [(_init-value init-value) ""]
          [validation-callback void])
    (define init-value _init-value)
    (super-new [label label]
               [init-value init-value]
               [style '(single)]
               [callback (λ (t e)
                           (validation-callback
                            (get-validated-value)))])
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (reset!)
      (set-value init-value)
      (set! ui-shows-invalid? #f)
      (set-field-background white))
    (define/public (get-validated-value)
      (define n
        (string->number (string-trim (get-value))))
      (cond
        [(exact-positive-integer? n)
         (when ui-shows-invalid?
           (set! ui-shows-invalid? #f)
           (set-field-background white))
         n]
        [else
         (unless ui-shows-invalid?
           (set! ui-shows-invalid? #t)
           (set-field-background invalid-bg-color))
         #f]))
    #|END class positive-integer-input%|#))


(define* abstract-add-widget%
  (define invalid-int-message%
    (red-text-message
     "Fields must be positive integers (in Arabic numerals)."))
  (class vertical-panel%
    (inherit change-children)
    (abstract initialize-prelude-children
              on-add-clicked)
    (define ui-shows-invalid? #f)
    (init [(_callback callback)]
          add-button-label)
    (define callback _callback)
    (super-new [alignment '(left top)])
    (initialize-prelude-children)
    (define invalid-message
      (new invalid-int-message%
           [style '(deleted)]
           [parent this]))
    (define add-button
      (new button%
           [label add-button-label]
           [style '(border)]
           [callback (λ (b e) (on-add-clicked))]
           [parent this]))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public-final (invoke-callback n make-group+togo)
      (callback n make-group+togo))
    (define/pubment (reset!)
      (hide-invalid-message!)
      (inner (void) reset!))
    (define/pubment (show-invalid-message!)
      (unless ui-shows-invalid?
        (set! ui-shows-invalid? #t)
        (send add-button enable #f)
        (change-children
         (λ (old-children)
           (define-values {before after}
             (split-at-right old-children 1))
           (append before (cons invalid-message after))))
        (inner (void) show-invalid-message!)))
    (define/pubment (hide-invalid-message!)
      (when ui-shows-invalid?
        (set! ui-shows-invalid? #f)
        (send add-button enable #t)
        (change-children
         (λ (old-children)
           (remove invalid-message old-children)))
        (inner (void) hide-invalid-message!)))
    #|END class abstract-add-widget%|#))

(define add-unnumbered-widget%
  (class abstract-add-widget%
    (inherit invoke-callback
             show-invalid-message!
             hide-invalid-message!)
    (define field #f)
    (define/override (initialize-prelude-children)
      (define grp
        (new vertical-pane%
             [alignment '(left top)]
             [stretchable-height #f]
             [parent this]))
      (set! field
            (new positive-integer-field%
                 [label "Add this many pages:"]
                 [validation-callback (λ (n) (check-validity n))]
                 [parent grp]))
      (new message%
           [label "that don't have page numbers in the source."]
           [parent grp]))
    (super-new [add-button-label "Add Unnumbered Pages"])
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/augment (reset!)
      (send field reset!))
    (define/private (check-validity n)
      (if n
          (hide-invalid-message!)
          (show-invalid-message!)))
    (define/override (on-add-clicked)
      (define n (send field get-validated-value))
      (check-validity n)
      (when n
        (invoke-callback
         n
         (λ (old-to-go)
           (define-values {raw-these to-go}
             (split-at old-to-go n))
           (values (unnumbered-group
                    (for/list ([str (in-list raw-these)])
                      (pre-page (nothing) str)))
                   to-go)))))
    #|END class add-unnumbered-widget%|#))


(define add-arabic-widget%
  (class abstract-add-widget%
    (inherit invoke-callback
             show-invalid-message!
             hide-invalid-message!)
    (define count-field #f)
    (define init-num-field #f)
    (define/override (initialize-prelude-children)
      (define (validation-callback _)
        (get-validated-values)
        (void))
      (define grp
        (new vertical-pane%
             [alignment '(left top)]
             [stretchable-height #f]
             [parent this]))
      (set! count-field 
            (new positive-integer-field%
                 [label "Add this many pages:"]
                 [validation-callback validation-callback]
                 [parent grp]))
      (new message%
           [label "numbered with Arabic numerals"]
           [parent grp])
      (set! init-num-field
            (new positive-integer-field%
                 [label "starting with number:"]
                 [init-value "1"]
                 [validation-callback validation-callback]
                 [parent grp])))
    (super-new [add-button-label "Add Numbered Pages"])
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/augment (reset!)
      (send count-field reset!)
      (send init-num-field reset!))
    (define/private (get-validated-values)
      (let ([count (send count-field get-validated-value)]
            [init (send init-num-field get-validated-value)])
        (if (and count init)
            (hide-invalid-message!)
            (show-invalid-message!))
        (values count init)))
    (define/override (on-add-clicked)
      (define-values {count init}
        (get-validated-values))
      (when (and count init)
        (invoke-callback
         count
         (λ (old-to-go)
           (define-values {raw-these to-go}
             (split-at old-to-go count))
           (values (arabic-group init
                                 (for/list ([str (in-list raw-these)]
                                            [n (in-naturals init)])
                                   (pre-page (just (string->immutable-string
                                                    (number->string n)))
                                             str)))
                   to-go)))))
    #|END class add-arabic-widget%|#))


(define* add-roman-widget%
  (define r-n-case-choice%
    (class choice%
      (inherit set-selection get-selection)
      (init [label #f])
      (super-new [label label]
                 [choices '("lower case"
                            "upper case")])
      (define/public (reset!)
        (set-selection 0))
      (define/public (get-validated-value)
        (case (get-selection)
          [(0) 'lower]
          [(1) 'upper]))
      #|END class r-n-case-choice%|#))
  (class abstract-add-widget%
    (inherit invoke-callback
             show-invalid-message!
             hide-invalid-message!)
    (define count-field #f)
    (define init-num-field #f)
    (define r-n-case-control #f)
    (define/override (initialize-prelude-children)
      (define (validation-callback _)
        (get-validated-values)
        (void))
      (define grp
        (new vertical-pane%
             [alignment '(left top)]
             [stretchable-height #f]
             [parent this]))
      (set! count-field 
            (new positive-integer-field%
                 [label "Add this many pages:"]
                 [validation-callback validation-callback]
                 [parent grp]))
      (new message%
           [label "numbered with Roman numerals"]
           [parent grp])
      (set! init-num-field
            (new positive-integer-field%
                 [label "starting with number:"]
                 [init-value "1"]
                 [validation-callback validation-callback]
                 [parent grp]))
      (define row
        (new horizontal-pane%
             [parent grp]))
      (new message%
           [label "using"]
           [parent row])
      (set! r-n-case-control
            (new r-n-case-choice%
                 [parent row]))
      (new message%
           [label "letters."]
           [parent row]))
    (super-new [add-button-label "Add Roman Numeral Pages"])
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/augment (reset!)
      (send count-field reset!)
      (send init-num-field reset!)
      (send r-n-case-control reset!))
    (define/private (get-validated-values)
      (let ([count (send count-field get-validated-value)]
            [init (send init-num-field get-validated-value)])
        (if (and count init)
            (hide-invalid-message!)
            (show-invalid-message!))
        (values count init)))
    (define/override (on-add-clicked)
      (define-values {count init}
        (get-validated-values))
      (define r-n-case
        (send r-n-case-control get-validated-value))
      (when (and count init)
        (invoke-callback
         count
         (λ (old-to-go)
           (define-values {raw-these to-go}
             (split-at old-to-go count))
           (values (roman-group init
                                r-n-case
                                (parameterize ([current-roman-numeral-case r-n-case])
                                  (for/list ([str (in-list raw-these)]
                                             [n (in-naturals init)])
                                    (pre-page (just (string->immutable-string
                                                     (number->roman n)))
                                              str))))
                   to-go)))))
    #|END class add-roman-widget%|#))


(define add-pages-widget%
  (class group-box-panel%
    (inherit change-children)
    (init callback)
    (super-new [label "Add Pages"]
               [alignment '(left top)])
    (define type-choice
      (new choice%
           [label "Type:"]
           [choices '("Numbered Pages (Arabic)"
                      "Roman Numeral Pages"
                      "Unnumbered Pages")]
           [callback (λ (c e) (on-choose-widget))]
           [parent this]))
    (define arabic-widget
      (new add-arabic-widget%
           [callback callback]
           [parent this]))
    (define roman-widget
      (new add-roman-widget%
           [callback callback]
           [style '(deleted)]
           [parent this]))
    (define unnumbered-widget
      (new add-unnumbered-widget%
           [callback callback]
           [style '(deleted)]
           [parent this]))
    (define active-widget arabic-widget)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (reset!)
      (send type-choice set-selection 0)
      (set-active-widget! arabic-widget)
      (send arabic-widget reset!)
      (send roman-widget reset!)
      (send unnumbered-widget reset!)
      (void))
    (define/private (on-choose-widget)
      (set-active-widget!
       (case (send type-choice get-selection)
         [(0) arabic-widget]
         [(1) roman-widget]
         [(2) unnumbered-widget])))
    (define/private (set-active-widget! w)
      (unless (equal? w active-widget)
        (set! active-widget w)
        (change-children
         (λ (old-children)
           (reverse
            (cons w (cdr (reverse old-children))))))))
    #|END class add-pages-widget%|#))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-lexical-member-names get-state)

(define pages-panel%
  (class vertical-panel%
    (inherit get-top-level-window)
    (init [(_str string)])
    (define state
      (page-group-state #f (split-pages _str) #f))
    (super-new [alignment '(left top)])
    (define header-area
      (new vertical-pane%
           [alignment '(left top)]
           [stretchable-height #f]
           [parent this]))
    (define num-to-go
      (new num-to-go-widget%
           [n (count-pages-to-go state)]
           [parent header-area]))
    (let ([row (new horizontal-panel%
                    [parent header-area])])
      (new message%
           [label "Total Pages:"]
           [parent row])
      (new message%
           [label (number->string (count-pages-to-go state))]
           [parent row]))
    (define-values {so-far-panel undo-button}
      (let ([pane (new vertical-pane%
                       [alignment '(left top)]
                       [parent this])])
        (values (new group-box-panel%
                     [label "Assigned So Far:"]
                     [alignment '(left top)]
                     [min-height 75]
                     [parent pane])
                (new button%
                     [label "Remove Last Group"]
                     [enabled #f]
                     [callback (λ (b e) (undo!))]
                     [parent pane]))))
    (define add-widget
      (new add-pages-widget%
           [callback (λ (count make-group+togo)
                       (on-add-page-group count make-group+togo))]
           [parent this]))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public-final (ready?)
      (page-group-state-finished? state))
    (define/public-final (get-ab-xexpr)
      (and (ready?)
           `(ab () ,@(page-group-state->xexpr-forest state))))
    (define/public-final (get-description-strings)
      (describe-page-group-state state))
    (define/public-final (get-state)
      ;; for debugging
      state)
    (define/private (undo!)
      (unless (page-group-state-initial? state)
        (set! state (page-group-state-prev state))
        (send so-far-panel
              change-children
              (λ (old-children)
                (drop-right old-children 1))))
      (update-after-change-state!))
    (define/private (on-add-page-group count make-group+togo)
      (define num-to-go
        (count-pages-to-go state))
      (cond
        [(infix: count > num-to-go)
         (message-box "Error: Too Many Pages!"
                      (string-append "Error: Too Many Pages!\n\n"
                                     "You tried to add " (number->string count)
                                     " pages, but there are only "
                                     (number->string num-to-go)
                                     " pages left to be assigned.")
                      (get-top-level-window)
                      '(ok stop))
         (void)]
        [else
         (define-values {group new-to-go}
           (make-group+togo (page-group-state-to-go state)))
         (when (confirm-add-page-group group (get-top-level-window))
           (define new-state
             (page-group-state group new-to-go state))
           (set! state new-state)
           (send add-widget reset!)
           (new message%
                [label (page-group->string group)]
                [parent so-far-panel])
           (update-after-change-state!))]))
    (define/private (update-after-change-state!)
      (send num-to-go set-n! (count-pages-to-go state))
      (define finished?
        (page-group-state-finished? state))
      (send add-widget enable (not finished?))
      (define initial?
        (page-group-state-initial? state))
      (send undo-button enable (not initial?)))
    #|END class pages-panel%|#))


;                                  
;                                  
;                                  
;                                  
;                      ;           
;                      ;;          
;  ; ;; ;;    ;;    ;;;;;   ;; ;   
;  ;; ;; ;   ;  ;      ;;   ;;; ;  
;  ;; ;; ;;     ;;     ;;   ;;  ;; 
;  ;; ;; ;;   ;;;;     ;;   ;;  ;; 
;  ;; ;; ;;  ;  ;;     ;;   ;;  ;; 
;  ;; ;; ;; ;;  ;;     ;;   ;;  ;; 
;  ;; ;; ;;  ;;; ;     ;;   ;;  ;; 
;                                  
;                                  
;                                  
;                                  

(module+ main
  (provide (all-defined-out))
  (define f
    (new frame% [label "Pages Test"]))

  (define pages-panel
    (new pages-panel%
         [string "a\fb\fc"]
         [parent f]))

  (send f show #t))

