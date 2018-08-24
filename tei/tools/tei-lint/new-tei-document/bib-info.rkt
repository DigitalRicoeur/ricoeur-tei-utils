#lang racket/gui

(require ricoeur/tei/tools/tei-lint/lib
         ricoeur/tei/base
         adjutor
         pict
         gregor)

(provide bib-info-panel% ;; method: get-bib-info
         author/editor-spec? 
         publication-date-list/c
         (contract-out
          [struct publication-date-spec
            ([this publication-date-list/c]
             [orig (or/c 'thisIsOriginal publication-date-list/c)])
            #:omit-constructor]
          [struct bib-info
            ([title (or/c #f (and/c string-immutable/c trimmed-string-px))]
             [book/article (or/c #f 'book 'article)]
             [lang (or/c #f 'en 'fr)]
             [citation (or/c #f (and/c string-immutable/c trimmed-string-px))]
             [date (or/c #f publication-date-spec?)]
             [authors+editors (non-empty-listof author/editor-spec?)])
            #:omit-constructor]
          [author/editor-spec->xexpr
           (-> author/editor-spec?
               (or/c (tei-xexpr/c author)
                     (tei-xexpr/c editor)))]
          [bib-info-valid?
           (-> bib-info? any/c)]
          [author/editor-panel%
           (class/c
            (init [author/editor-spec author/editor-spec?]
                  [delete-callback
                   (or/c #f (-> author/editor-spec?
                                (is-a?/c author/editor-panel%)
                                any))]))]
          ))

(define nonempty-textarea-control%
  (class text-field%
    (inherit get-value
             set-field-background)
    (define ui-shows-valid? #t)
    (init [label #f])
    (super-new [label label]
               [style '(multiple vertical-label)]
               [callback (λ (t e) (on-edit))])
    (define/public (get-validated-value)
      (on-edit)
      (and (valid-value?)
           (string->immutable-string
            (string-normalize-spaces (get-value)))))
    (define/private (valid-value?)
      (regexp-match? #px"\\S" (get-value)))
    (define/private (on-edit)
      (cond
        [(valid-value?)
         (unless ui-shows-valid?
           (set! ui-shows-valid? #t)
           (set-field-background white))]
        [else
         (when ui-shows-valid?
           (set! ui-shows-valid? #f)
           (set-field-background invalid-bg-color))]))
    #|END class title-control%|#))



(define abstract-radio-widget%
  (class vertical-panel%
    (inherit add-child delete-child)
    (define ui-shows-valid? #t)
    (init control% invalid-message%)
    (super-new [alignment '(left top)])
    (define ctl
      (new control%
           [callback (λ (r e) (check-validity))]
           [parent this]))
    (define invalid-msg
      (new invalid-message%
           [parent this]
           [style '(deleted)]))
    (define/private (check-validity)
      (get-validated-value)
      (void))
    (define/public (get-validated-value)
      (define rslt
        (send ctl get-validated-value))
      (cond
        [rslt
         (unless ui-shows-valid?
           (set! ui-shows-valid? #t)
           (delete-child invalid-msg))]
        [else
         (when ui-shows-valid?
           (set! ui-shows-valid? #f)
           (add-child invalid-msg))])
      rslt)
    #|END class abstract-radio-widget%|#))


(define* book/article-widget%
  (define book/article-control%
    (class radio-box%
      (inherit get-selection)
      (super-new [label "Document Type:"]
                 [choices '("Book" "Article")]
                 [selection #f]
                 [style '(horizontal vertical-label)])
      (define/public (get-validated-value)
        (case (get-selection)
          [(0) 'book]
          [(1) 'article]
          [else #f]))
      #|END class book/article-control%|#))
  (define book/article-invalid-message%
    (red-text-message "Please choose \"Book\" or \"Article.\""))
  (class abstract-radio-widget%
    (super-new [control% book/article-control%]
               [invalid-message% book/article-invalid-message%])
    #|END class book/article-widget%|#))



(define* language-widget%
  (define language-control%
    (class radio-box%
      (inherit get-selection)
      (super-new [label "Language:"]
                 [choices '("English" "French")]
                 [selection #f]
                 [style '(horizontal vertical-label)])
      (define/public (get-validated-value)
        (case (get-selection)
          [(0) 'en]
          [(1) 'fr]
          [else #f]))
      #|END class language-control%|#))
  (define language-invalid-message%
    (red-text-message "Please choose a language."))
  (class abstract-radio-widget%
    (super-new [control% language-control%]
               [invalid-message% language-invalid-message%])
    #|END class language-widget%|#))


;                                  
;                                  
;                                  
;                                  
;       ;;           ;;            
;       ;;           ;;            
;    ;;;;;    ;;   ;;;;;;;    ;;;  
;   ;   ;;   ;  ;    ;;     ;;   ; 
;   ;   ;;      ;;   ;;     ;    ; 
;  ;;   ;;    ;;;;   ;;    ;;;;;;;;
;   ;   ;;   ;  ;;   ;;     ;      
;   ;   ;;  ;;  ;;    ;     ;;   ; 
;    ;;; ;   ;;; ;     ;;;    ;;;  
;                                  
;                                  
;                                  
;                                  


(define* year-choice%
  (define-values {values-vec labels-lst}
    (let ([this-year (->year (now))])
      (for/lists/define (year-ints year-strings)
                        ([y (in-naturals 1913)] ;; Ricoeur's birth year
                         #:final (= y this-year))
        (values y (number->string y)))
      (values (apply vector-immutable #f year-ints)
              (cons "" year-strings))))
  (class choice%
    (inherit get-selection)
    (init [label #f])
    (super-new [label label]
               [choices labels-lst])
    (define/public (get-validated-value)
      (vector-ref values-vec (get-selection)))
    #|END class year-choice%|#))

(define* month-choice%
  (define-values {values-vec labels-lst}
    (let* ([strs '("January"
                   "February"
                   "March"
                   "April"
                   "May"
                   "June"
                   "July"
                   "August"
                   "September"
                   "October"
                   "November"
                   "December")]
           [ints (for/list ([_ (in-list strs)]
                            [i (in-naturals 1)])
                   i)])
      (values (apply vector-immutable #f ints)
              (cons "--Month Not Specified--"
                    strs))))
  (class choice%
    (inherit get-selection)
    (init [label #f])
    (super-new [label label]
               [choices labels-lst])
    (define/public (get-validated-value)
      (vector-ref values-vec (get-selection)))
    #|END class month-choice%|#))


(define* labeled-date-widget%
  (define year+month-widget%
    (class horizontal-pane%
      (init [callback void]
            [month-callback void])
      (super-new)
      (define y
        (new year-choice%
             [callback callback]
             [parent this]))
      (define m
        (new month-choice%
             [callback month-callback]
             [parent this]))
      (define/public (get-validated-value)
        (define year
          (send y get-validated-value))
        (cond
          [year
           (filter values
                   (list year (send m get-validated-value)))]
          [else
           #f]))))
  (define invalid-year-message%
    (red-text-message "Please select a publication year."))
  (class vertical-panel%
    (inherit add-child delete-child)
    (init label
          [(_validation-callback validation-callback) void])
    (define ui-shows-valid? #t)
    (define validation-callback _validation-callback)
    (super-new [alignment '(left top)])
    (new message%
         [label label] 
         [parent this])
    (define year+month-row
      (new year+month-widget%
           [callback (λ (c e) (check-validity))]
           [month-callback (λ (c e) (validation-callback))]
           [parent this]))
    (define invalid-message
      (new invalid-year-message%
           [style '(deleted)]
           [parent this]))
    (define/private (check-validity)
      (get-validated-value)
      (validation-callback)
      (void))
    (define/public (get-validated-value)
      (define rslt
        (send year+month-row get-validated-value))
      (cond
        [rslt
         (unless ui-shows-valid?
           (set! ui-shows-valid? #t)
           (delete-child invalid-message))]
        [else
         (when ui-shows-valid?
           (set! ui-shows-valid? #f)
           (add-child invalid-message))])
      rslt)
    #|END class this-date-widget%|#))



(define* orig-date-widget%
  (define publication-original?-control%
    (class radio-box%
      (inherit get-selection)
      (super-new [label "Was this the first published instance?"]
                 [choices '("Yes" "No")]
                 [selection #f]
                 [style '(horizontal vertical-label)])
      (define/public (get-validated-value)
        (case (get-selection)
          [(0) 'yes]
          [(1) 'no]
          [else #f]))
      #|END class language-control%|#))
  (define missing-selection-message%
    (red-text-message "Please choose \"Yes\" or \"No.\""))
  (class vertical-panel%
    (inherit add-child delete-child change-children)
    (init [(_validation-callback validation-callback) void])
    (define validation-callback _validation-callback)
    (define ui-state 'init) ;; (or/c 'init 'yes 'no 'bad)
    (super-new [alignment '(left top)])
    (define orig?-ctl
      (new publication-original?-control%
           [callback (λ (r e) (on-toggle))]
           [parent this]))
    (define invalid-message
      (new missing-selection-message%
           [parent this]
           [style '(deleted)]))
    (define orig-date-panel
      (new labeled-date-widget%
           [parent this]
           [label "Original publication date:"]
           [validation-callback validation-callback]
           [style '(deleted)]))
    (define/private (on-toggle)
      (get-validated-orig?-value)
      (validation-callback)
      (void))
    (define/private (get-validated-orig?-value)
      (define orig?
        (send orig?-ctl get-validated-value))
      (case orig?
        [(yes)
         (unless (eq? 'yes ui-state)
           (set! ui-state 'yes)
           (change-children
            (λ (old-children)
              (remove invalid-message
                      (remove orig-date-panel
                              old-children)))))]
        [(no)
         (unless (eq? 'no ui-state)
           (set! ui-state 'no)
           (change-children
            (λ (old-children)
              (define lst
                (remove invalid-message old-children))
              (if (member orig-date-panel lst)
                  lst
                  (append lst (list orig-date-panel))))))]
        [else
         (unless (eq? 'bad ui-state)
           (set! ui-state 'bad)
           (change-children
            (λ (old-children)
              (define lst
                (remove orig-date-panel old-children))
              (if (member invalid-message lst)
                  lst
                  (append lst (list invalid-message))))))])
      orig?)
    (define/public (get-validated-value)
      (define orig?
        (get-validated-orig?-value))
      (case orig?
        [(yes)
         'thisIsOriginal]
        [(no)
         (send orig-date-panel get-validated-value)]
        [else
         #f]))
    #|END class language-control%|#))

(struct publication-date-spec (this orig)
  #:transparent)

(define/final-prop publication-date-list/c
  (or/c (list/c natural-number/c)
        (list/c natural-number/c natural-number/c))) ;;between/c ??

(define* publication-date-widget%
  (define invalid-order-message%
    (red-text-message "The publication dates must be in order."))
  (class group-box-panel%
    (inherit add-child delete-child)
    (define ui-shows-order-valid? #t)
    (super-new [label "Publication Date:"]
               [alignment '(left top)])
    (define-values {this-date orig-date}
      (let ([validation-callback (λ () (on-state-change))])
        (values
         (new labeled-date-widget%
              [label "Publication date of this instance:"]
              [validation-callback validation-callback]
              [parent this])
         (new orig-date-widget%
              [validation-callback validation-callback]
              [parent this]))))
    (define invalid-message
      (new invalid-order-message%
           [style '(deleted)]
           [parent this]))
    ;;;;;;;;;;;;;;;;;;;;
    (define/private (on-state-change)
      (get-validated-value)
      (void))
    (define/public (get-validated-value)
      (define t (send this-date get-validated-value))
      (define o (send orig-date get-validated-value))
      (define order-ok?
        (in-order-if-applicable? t o))
      (if order-ok?
          (unless ui-shows-order-valid?
            (set! ui-shows-order-valid? #t)
            (delete-child invalid-message))
          (when ui-shows-order-valid?
            (set! ui-shows-order-valid? #f)
            (add-child invalid-message)))
      (and t o order-ok? (publication-date-spec t o)))
    (define/private (in-order-if-applicable? this orig)
      (cond
        [(and (list? this) (list? orig))
         (date<=? (apply date orig)
                  (apply date this))]
        [else
         'not-applicable]))
    #|END class publication-date-widget%|#))


;                                                  
;                                                  
;                                                  
;                                                  
;                    ;;     ;;                     
;                    ;;     ;;                     
;     ;;    ;;  ;; ;;;;;;;  ;; ;     ;;;    ;; ;;; 
;    ;  ;   ;;  ;;   ;;     ;;; ;   ;   ;   ;;;    
;       ;;  ;;  ;;   ;;     ;;  ;;  ;   ;   ;;     
;     ;;;;  ;;  ;;   ;;     ;;  ;; ;;   ;;  ;;     
;    ;  ;;  ;;  ;;   ;;     ;;  ;;  ;   ;   ;;     
;   ;;  ;;   ; ;;;    ;     ;;  ;;  ;   ;   ;;     
;    ;;; ;    ; ;;     ;;;  ;;  ;;   ;;;    ;;     
;                                                  
;                                                  
;                                                  
;                                                  

(struct author/editor-spec (type xml:id? name)
  #:transparent)

(define (a/e-type->string type)
  (case type
    [(author) "Author"]
    [(editor) "Editor"]
    [(translator) "Translator"]
    [(compiler) "Compiler"]
    [(preface) "Preface author"]))

(define author/editor-spec->xexpr
  (match-lambda
    [(author/editor-spec type xml:id? name)
     (list (if (eq? 'author type)
               'author
               'editor)
           (append (list-unless (eq? 'author type)
                     `([type ,(string->immutable-string
                               (symbol->string type))]))
                   (list-when xml:id?
                     `([xml:id ,xml:id?])))
           name)]))

(define* a/e-type-choice%
  (define-values {types-vec choices-lst}
    (let ([types '(author editor translator compiler preface)])
      (values (apply vector-immutable types)
              (map a/e-type->string types))))
  (class choice%
    (inherit get-selection)
    (init [label #f])
    (super-new [label label]
               [choices choices-lst])
    (define/public (get-validated-value)
      (vector-ref types-vec (get-selection)))
    #|END class a/e-type-choice%|#))
                   
(define add-author/editor-widget%
  (class group-box-panel%
    (super-new [label "Add additional:"]
               [alignment '(left top)])
    (init [(_callback callback) void])
    (define callback _callback)
    (define type-ctl
      (new a/e-type-choice%
           [label "Type:"]
           [parent this]))
    (define name-background-normal? #t)
    (define name-ctl
      (new text-field%
           [label "Name:"]
           [callback (λ (t e) (on-edit-name))]
           [parent this]))
    (define add-button
      (new button%
           [label "Add"]
           [callback (λ (b e) (on-add-clicked))]
           [enabled #f]
           [parent this]))
    (define/private (reset!)
      (send add-button enable #f)
      (set-name-normal-background!)
      (send type-ctl set-selection 0)
      (send name-ctl set-value ""))
    (define/private (name-is-valid? [v (send name-ctl get-value)])
      (regexp-match? #px"\\S" v))
    (define/private (get-validated-name)
      (define v (send name-ctl get-value))
      (and (name-is-valid? v)
           (string->immutable-string
            (string-normalize-spaces v))))
    (define/private (on-edit-name)
      (cond
        [(name-is-valid?)
         (set-name-normal-background!)
         (send add-button enable #t)]
        [else
         (send add-button enable #f)]))
    (define/private (set-name-normal-background!)
      (unless name-background-normal?
        (set! name-background-normal? #t)
        (send name-ctl set-background white)))
    (define/private (on-add-clicked)
      (define name (get-validated-name))
      (cond
        [name
         (define type
           (send type-ctl get-validated-value))
         (define it
           (author/editor-spec type #f name))
         (callback it)
         (reset!)]
        [else
         (when name-background-normal?
           (set name-background-normal? #f)
           (send name-ctl set-background invalid-bg-color))]))
    #|END class add-author/editor-widget%|#))


(define author/editor-panel%
  (class horizontal-panel%
    (super-new)
    (init [(a/e author/editor-spec)]
          [delete-callback #f])
    (new message%
         [label (string-append (a/e-type->string
                                (author/editor-spec-type a/e))
                               ":")]
         [font bold-system-font]
         [parent this])
    (new message%
         [label (author/editor-spec-name a/e)]
         [parent this])
    (when delete-callback
      (new button%
           [label "Remove"]
           [callback (λ (b e) (delete-callback a/e this))]
           [parent this]))
    #|END class author+editor-panel%|#))

(define author+editor-widget%
  (class group-box-panel%
    (super-new [label "Authors & Editors:"]
               [alignment '(left top)])
    (define vals
      (list (author/editor-spec 'author "ricoeur" "Paul Ricœur")))
    (define current-vals-panel
      (new vertical-panel%
           [alignment '(left top)]
           [parent this]))
    (new author/editor-panel%
         [parent current-vals-panel]
         [author/editor-spec (car vals)])
    (define add-widget
      (new add-author/editor-widget%
           [parent this]
           [callback (λ (a/e) (on-add a/e))]))
    (define/private (on-add a/e)
      (set! vals (append vals (list a/e)))
      (new author/editor-panel%
           [parent current-vals-panel]
           [author/editor-spec a/e]
           [delete-callback (λ (spec panel)
                              (on-delete spec panel))]))
    (define/private (on-delete spec panel)
      (set! vals (remove spec vals))
      (send current-vals-panel delete-child panel))
    (define/public (get-validated-value)
      vals)
    #|END class author+editor-widget%|#))


;                                  
;                                  
;                                  
;                                  
;      ;                ;;;        
;      ;;             ;;           
;   ;;;;;   ;; ;    ;;;;;;;  ;;;   
;      ;;   ;;; ;     ;;    ;   ;  
;      ;;   ;;  ;;    ;;    ;   ;  
;      ;;   ;;  ;;    ;;   ;;   ;; 
;      ;;   ;;  ;;    ;;    ;   ;  
;      ;;   ;;  ;;    ;;    ;   ;  
;      ;;   ;;  ;;    ;;     ;;;   
;                                  
;                                  
;                                  
;                                  

(struct bib-info (title
                  book/article
                  lang
                  citation
                  date
                  authors+editors)
  #:transparent)

(define bib-info-valid?
  (match-lambda
    [(bib-info (not #f) (not #f) (not #f)
               (not #f) (not #f) (not #f))
     #t]
    [_
     #f]))

(define bib-info-panel%
  (class vertical-panel%
    (super-new [spacing 7]
               [alignment '(left top)])
    (define title
      (new nonempty-textarea-control%
           [label "Title:"]
           [parent this]))
    (define b/a
      (new book/article-widget%
           [parent this]))
    (define lang
      (new language-widget%
           [parent this]))
    (define citation
      (new nonempty-textarea-control%
           [label "Citation:"]
           [parent this]))
    (define dt
      (new publication-date-widget%
           [parent this]))
    (define a+e
      (new author+editor-widget%
           [parent this]))
    (define/public (get-bib-info)
      (bib-info (send title get-validated-value)
                (send b/a get-validated-value)
                (send lang get-validated-value)
                (send citation get-validated-value)
                (send dt get-validated-value)
                (send a+e get-validated-value)))
    #|END class bib-info-panel%|#))

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
    (new tei-lint-menu-bar-frame%
         [label "Test"]))

  (define info-panel
    (new bib-info-panel%
         [parent f]))

  (send f show #t))

;; (send info-panel get-bib-info)


