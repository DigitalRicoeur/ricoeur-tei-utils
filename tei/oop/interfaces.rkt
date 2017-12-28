#lang racket

(require xml 
         ricoeur/tei/oop/xml-entity-utils
         ricoeur/tei/xexpr/tei-xexpr-contracts
         data/maybe
         gregor
         adjutor
         )

(provide element-or-xexpr->plain-text
         element<%>
         tei-element?
         element-or-xexpr/c
         elements-only<%>
         TEI-info<%>
         teiHeader<%>
         get-page-breaks<%>
         TEI-body<%>
         pb<%>
         guess-paragraphs<%>
         guess-paragraphs?
         p<%>
         ab<%>
         TEI<%>
         )

(module+ private
  (provide element%
           elements-only-mixin
           can-have-id<%>
           get-title<%>
           get-publication-date<%>
           get-citation<%>
           body-element%
           guess-paragraphs-element%
           ab?
           content-containing-element-mixin
           ab-to-pre-segments-mixin
           element:elements-only+guess-paragraphs+to-pre-segments%
           ))

(module+ search
  (provide location-stack-entry/c
           location-stack-entry:div/c
           location-stack-entry:note/c
           ))

(define/contract (cdata->plain-text it)
  (-> cdata? string?)
  (match (cdata-string it)
    [(regexp #rx"<!\\[CDATA\\[(.*)\\]\\]>" (list _ content))
     content]
    [content content]))

(define/contract (valid-char->plain-text num)
  (-> valid-char? string?)
  (string (integer->char num)))

;                                                                  
;                                                                  
;                                                                  
;                                                                  
;           ;;;;                                     ;;      ;     
;             ;;                                     ;;     ; ;   ;
;     ;;;     ;;      ;;;  ; ;; ;;    ;;;   ;; ;   ;;;;;;; ;; ;; ; 
;   ;;   ;    ;;    ;;   ; ;; ;; ;  ;;   ;  ;;; ;    ;;     ; ;  ; 
;   ;    ;    ;;    ;    ; ;; ;; ;; ;    ;  ;;  ;;   ;;      ;     
;  ;;;;;;;;   ;;   ;;;;;;;;;; ;; ;;;;;;;;;; ;;  ;;   ;;         ;  
;   ;         ;;    ;      ;; ;; ;; ;       ;;  ;;   ;;     ; ;; ;;
;   ;;   ;     ;    ;;   ; ;; ;; ;; ;;   ;  ;;  ;;    ;     ;  ; ; 
;     ;;;       ;;    ;;;  ;; ;; ;;   ;;;   ;;  ;;     ;;; ;;   ;  
;                                                                  
;                                                                  
;                                                                  
;                                                                  

(define-values {element% element-or-xexpr/c}
  (let ([element<%> (interface ())])
    (define element-or-xexpr/c
      (or/c (is-a?/c element<%>)
            string?
            symbol?
            valid-char?
            cdata?
            comment?
            p-i?))
    (define-member-name private-method (generate-member-key))
    (define/contract element%
      (class/c (init [name symbol?]
                     [attributes (listof (list/c symbol? string?))]
                     [body (listof element-or-xexpr/c)]))
      (class* object% ((interface (element<%>)
                         [to-xexpr (->m any-tei-xexpr/c)]
                         [to-plain-text (->m string?)]
                         [get-name (->m symbol?)]
                         [get-attributes (->m (listof (list/c symbol? string?)))]
                         [get-body (->m (listof element-or-xexpr/c))]
                         private-method
                         ))
        (super-new)
        (init [(:name name)]
              [(:attributes attributes) null]
              [(:body body) null])
        (def
          [name :name]
          [attributes :attributes]
          [body :body])
        (define/public (to-xexpr)
          (list* name attributes (for/list ([child (in-list body)])
                                   (if (xexpr? child)
                                       child
                                       (send child to-xexpr)))))
        (define/public (to-plain-text)
          (string-join (map element-or-xexpr->plain-text
                            body)
                       ""))
        (public-final*
         [private-method (λ () (void))]
         [get-name (λ () name)]
         [get-attributes (λ () attributes)]
         [get-body (λ () body)])
        #|END element%|#))
    (values element% element-or-xexpr/c)))

(define/contract (element-or-xexpr->plain-text child)
  (-> element-or-xexpr/c string?)
  (cond [(string? child)
         child]
        [(tei-element? child)
         (send child to-plain-text)]
        [(or (comment? child)
             (p-i? child))
         ""]
        [(cdata? child)
         (cdata->plain-text child)]
        [(valid-char? child)
         (valid-char->plain-text child)]
        [(symbol? child) 
         (string (entity-symbol->char child))]))


(define element<%>
  (class->interface element%))

(define tei-element?
  (is-a?/c element<%>))


;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;           ;;;;                                                    ;;;;           
;             ;;                                                      ;;           
;     ;;;     ;;      ;;;  ; ;; ;;    ;;             ;;;    ;; ;      ;;   ;     ; 
;   ;;   ;    ;;    ;;   ; ;; ;; ;  ;;  ;           ;   ;   ;;; ;     ;;    ;   ;  
;   ;    ;    ;;    ;    ; ;; ;; ;;  ;              ;   ;   ;;  ;;    ;;    ;   ;  
;  ;;;;;;;;   ;;   ;;;;;;;;;; ;; ;;   ;;           ;;   ;;  ;;  ;;    ;;     ;  ;  
;   ;         ;;    ;      ;; ;; ;;     ;;          ;   ;   ;;  ;;    ;;     ; ;   
;   ;;   ;     ;    ;;   ; ;; ;; ;; ;   ;           ;   ;   ;;  ;;     ;     ; ;   
;     ;;;       ;;    ;;;  ;; ;; ;;  ;;;             ;;;    ;;  ;;      ;;    ;    
;                                                                             ;    
;                                                                            ;     
;                                                                          ;;      
;                                                                                  

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
      (delay (filter tei-element? body)))
    (define/public-final (get-body/elements-only)
      (force pr:body/elements-only))))

;                                  
;                                  
;                                  
;                                  
;   ;;                          ;; 
;   ;;                          ;; 
;   ;; ;      ;;;     ;;     ;;;;; 
;   ;;; ;   ;;   ;   ;  ;   ;   ;; 
;   ;;  ;;  ;    ;      ;;  ;   ;; 
;   ;;  ;; ;;;;;;;;   ;;;; ;;   ;; 
;   ;;  ;;  ;        ;  ;;  ;   ;; 
;   ;;  ;;  ;;   ;  ;;  ;;  ;   ;; 
;   ;;  ;;    ;;;    ;;; ;   ;;; ; 
;                                  
;                                  
;                                  
;                                  

(define can-have-id<%>
  (interface ()
    [get-id-or-false (->m (or/c symbol? #f))]))

(define get-title<%>
  (interface ()
    [get-title (->m string?)]
    [get-resp-string (->m symbol? string?)]))

(define get-publication-date<%>
  (interface ()
    [get-publication-date (->m date?)]
    [get-original-publication-date (->m date?)]))

(define get-citation<%>
  (interface (get-publication-date<%>)
    [get-citation (->m string?)]))

(define TEI-info<%>
  (interface (get-title<%> get-citation<%>)
    [get-filename (->m (or/c #f string?))]))

(define teiHeader<%>
  (interface (TEI-info<%> element<%>)))

;                                  
;                                  
;                                  
;                                  
;   ;;                  ;;         
;   ;;                  ;;         
;   ;;;;     ;;;     ;;;;; ;     ; 
;   ;;  ;   ;   ;   ;   ;;  ;   ;  
;   ;;  ;   ;   ;   ;   ;;  ;   ;  
;   ;;  ;; ;;   ;; ;;   ;;   ;  ;  
;   ;;  ;   ;   ;   ;   ;;   ; ;   
;   ;;  ;   ;   ;   ;   ;;   ; ;   
;   ; ;;     ;;;     ;;; ;    ;    
;                             ;    
;                            ;     
;                          ;;      
;                                  

(define get-page-breaks<%>
  (interface (element<%>)
    [get-page-breaks (->m (listof (recursive-contract (is-a?/c pb<%>))))]))

(define pb<%>
  (interface (get-page-breaks<%>)
    [get-page-string (->m (maybe/c string?))]
    [get-kind (->m (or/c 'none 'number 'roman 'other))]
    [get-numeric (->m (maybe/c number?))]
    ))

(define pb?
  (is-a?/c pb<%>))

(define location-stack-entry:div/c
  (list/c 'div
          (or/c "chapter" "part" "section" "dedication"
                "contents" "intro" "bibl" "ack" "index")
          (maybe/c string?)))

(define location-stack-entry:note/c
  (list/c 'note
          (or/c "foot" "end")
          string?
          (or/c "transl" #f)))

(define location-stack-entry/c
  (or/c 'front
        'back
        location-stack-entry:div/c
        location-stack-entry:note/c))

(define TEI-body<%>
  (interface (get-page-breaks<%>)
    [to-pre-segments
     (->i {[this any/c]
           [pre-segment-accumulator? (-> any/c any/c)]
           [call-with-metadata
            (->* {(-> any)}
                 {#:resp #rx"#.+"
                  #:location location-stack-entry/c}
                 any)]
           [accumulator
            {pre-segment-accumulator?}
            ;a better check takes ridiculously long here, so
            ;rely on do-prepare-pre-segments
            pre-segment-accumulator?]
           [init-pb (is-a?/c pb<%>)]}
          (values [_ {pre-segment-accumulator?}
                     pre-segment-accumulator?]
                  [_ (is-a?/c pb<%>)]))]
    [to-pre-segments/add-metadata
     (->i {[this any/c]
           [pre-segment-accumulator? (-> any/c any/c)]
           [call-with-metadata
            (->* {(-> any)}
                 {#:resp #rx"#.+"
                  #:location location-stack-entry/c}
                 any)]
           [thunk
            {pre-segment-accumulator?}
            (-> (values pre-segment-accumulator?
                        (is-a?/c pb<%>)))]}
          (values [_ {pre-segment-accumulator?}
                     pre-segment-accumulator?]
                  [_ (is-a?/c pb<%>)]))]
    ))

(define can-get-page-breaks?
  (is-a?/c get-page-breaks<%>))

(define body-element?
  (is-a?/c TEI-body<%>))

(define not-body-element?
  (not/c body-element?))

(define body-element%
  (class* (class element%
            (super-new)
            (inherit get-attributes get-name)
            (define/pubment (to-pre-segments pred
                                             call-with-metadata
                                             acc
                                             init-pb)
              (to-pre-segments/add-metadata
               pred
               call-with-metadata
               (λ ()
                 (inner (values acc init-pb)
                        to-pre-segments
                        pred
                        call-with-metadata
                        acc
                        init-pb))))
            (define/pubment (to-pre-segments/add-metadata pred
                                                          call-with-metadata
                                                          thunk)
              (define (go)
                (let ([resp (car (dict-ref (get-attributes) 'resp '(#f)))])
                  (if resp
                      (call-with-metadata #:resp resp thunk)
                      (thunk))))
              (inner (go)
                     to-pre-segments/add-metadata
                     pred
                     call-with-metadata
                     go)))
    {TEI-body<%>}
    (super-new)
    (inherit get-attributes get-body)
    (define/augride (to-pre-segments pred
                                     call-with-metadata
                                     acc
                                     init-pb)
      (error 'to-pre-segments "Method must be overriden"))
    (define/public (get-page-breaks) 
      (flatten
       (for/list ([child (in-list (get-body))]
                  #:when (can-get-page-breaks? child))
         (send child get-page-breaks))))))

(define (init-pb+latest-pb->page init-pb latest-pb)
  (if (equal? init-pb latest-pb)
      (send init-pb get-page-string)
      (list (send init-pb get-page-string)
            (send latest-pb get-page-string))))

(define content-containing-element-mixin
  ;; for elements that contain textual data EXCEPT ab% (see below)
  (mixin {TEI-body<%>} {}
    (super-new)
    (inherit get-body)
    (define/override-final (to-pre-segments pred
                                            call-with-metadata
                                            acc
                                            init-pb)
      (let loop ([to-go (get-body)]
                 [this-so-far null] 
                 [acc acc]
                 [init-pb init-pb]
                 [latest-pb init-pb])
        (match to-go
          ['()
           ;; base case: finish up, accumulating the last round from this-so-far
           (values (acc #:body (string-normalize-spaces
                                (string-join (reverse this-so-far) " "))
                        #:page (init-pb+latest-pb->page init-pb latest-pb))
                   latest-pb)]
          [(cons (? body-element? child) more)
           ;; accumulate from this-so-far, dispatch to child element's implementation,
           ;; then continue with new acc and init-pb from child
           (define-values {new-acc new-latest-pb}
             (send child
                   to-pre-segments
                   pred
                   call-with-metadata
                   (acc #:body (string-normalize-spaces
                                (string-join (reverse this-so-far) " "))
                        #:page (init-pb+latest-pb->page init-pb latest-pb))
                   latest-pb))
           (loop more null new-acc new-latest-pb new-latest-pb)]
          [(cons (? pb? latest-pb) more)
           ;; continue with new latest-pb
           (loop more
                 this-so-far
                 acc
                 init-pb
                 latest-pb)]
          [(cons str-or-misc more)
           ;; continue, consing this child on to this-so-far
           (loop more
                 (cons (element-or-xexpr->plain-text str-or-misc)
                       this-so-far)
                 acc
                 init-pb
                 latest-pb)])))))

(define ab-to-pre-segments-mixin
  (mixin {TEI-body<%>} {}
    (super-new)
    (inherit get-body)
    (define/override-final (to-pre-segments pred
                                            call-with-metadata
                                            acc
                                            init-pb)
      ;;;; TODO: How to notify the linter if this method
      ;;;; encounters the ugly case? Logging?
      (let loop ([to-go (get-body)]
                 [acc acc]
                 [init-pb init-pb])
        (match to-go
          ['() 
          ;; finish by returning acc and pb
           (values acc init-pb)]
          [(cons (? body-element? child) more)
           ;; dispatch to child element, continue with returned acc and init-pb
           (define-values {new-acc new-init-pb}
             (send child
                   to-pre-segments
                   pred
                   call-with-metadata
                   acc
                   init-pb))
           (loop more new-acc new-init-pb)]
          [(list-rest (? not-body-element? plain-children) ... more)
           ;; This case handles a bunch of things that are not child TEI-body<%>
           ;; elements, i.e. strings, pb%s, and comments etc, as plain-children.
           ;; We start by counting how many pb%s we see here and keeping track
           ;; of the last (which may be init-pb).
           (for/fold/define ([num-pbs 0]
                             [latest-pb init-pb])
                            ([child (in-list plain-children)]
                             #:when (pb? child))
             (values (add1 num-pbs) child))
           (define new-acc
             (cond
               [[2 . > . num-pbs]
                ;; Good case: there are fewer than two pb%s in plain-children,
                ;; so we treat plain-children as a segment.
                (acc #:body (string-normalize-spaces
                             (string-join
                              (for/list ([child (in-list plain-children)]
                                         #:unless (pb? child))
                                (element-or-xexpr->plain-text child))
                              " "))
                     #:page (init-pb+latest-pb->page init-pb latest-pb))]
               [else
                ;; Ugly case: this handles massive ab%s that have not been segmented.
                ;; In this case, each page is used as a segment.
                (let loop ([acc acc]
                           [the-pb init-pb]
                           [to-go plain-children]
                           [this-so-far '()])
                  (match to-go
                    ['() acc]
                    [(cons (? pb? new-pb) more)
                     (loop (acc #:body (string-normalize-spaces
                                        (string-join
                                         (reverse this-so-far)
                                         " "))
                                #:page (send the-pb get-page-string))
                           new-pb
                           more
                           '())]
                    [(cons child more)
                     (loop acc
                           the-pb
                           more
                           (cons (element-or-xexpr->plain-text child)
                                 this-so-far))]))]))
           ;; Continue with new-acc and latest-pb (which may be init-pb).
           (loop more new-acc latest-pb)])))))







(define guess-paragraphs<%>
  (interface (TEI-body<%>)
    [guess-paragraphs (->i {[this any/c]}
                           {#:mode [mode (or/c 'blank-lines 'line-breaks)]}
                           [_ (this) (is-a?/c (object-interface this))])]))

(define guess-paragraphs?
  (is-a?/c guess-paragraphs<%>))

(define p<%>
  (interface (TEI-body<%>)))

(define ab<%>
  (interface (TEI-body<%>)
    [do-guess-paragraphs (->*m {}
                               {#:mode (or/c 'blank-lines 'line-breaks)}
                               (listof (or/c (is-a?/c pb<%>)
                                             (is-a?/c p<%>))))]))

(define ab?
  (is-a?/c ab<%>))

(define guess-paragraphs-element%
  (class* body-element% {guess-paragraphs<%>}
    (super-new)
    (inherit get-name get-attributes get-body)
    (define/public (guess-paragraphs #:mode [mode 'blank-lines])
      (new this%
           [name (get-name)]
           [attributes (get-attributes)]
           [body (flatten
                  (for/list ([child (in-list (get-body))])
                    (cond
                      [(ab? child)
                       (send child do-guess-paragraphs #:mode mode)]
                      [(guess-paragraphs? child)
                       (send child guess-paragraphs #:mode mode)]
                      [else
                       child])))]))))


(define element:elements-only+guess-paragraphs+to-pre-segments%
  (class (elements-only-mixin guess-paragraphs-element%)
    (super-new)
    (inherit get-body/elements-only)
    (define/override (to-pre-segments pred
                                      call-with-metadata
                                      acc
                                      init-pb)
      (for/fold ([acc acc]
                 [the-pb init-pb])
                ([child (in-list (get-body/elements-only))])
        (cond
          [(pb? child) (values acc child)]
          [else (send child
                      to-pre-segments
                      pred
                      call-with-metadata
                      acc
                      the-pb)])))))

(define-member-name set-filename! (generate-member-key))

(define TEI<%>
  (interface (element<%>
              TEI-info<%>
              TEI-body<%>
              guess-paragraphs<%>
              elements-only<%>)
    [get-md5 (->m string?)]
    [do-prepare-pre-segments
     (->i {[this any/c]
           [pre-segment-accumulator? (-> any/c any/c)]
           [call-with-metadata
            (->* {(-> any)}
                 {#:resp #rx"#.+"
                  #:location location-stack-entry/c}
                 any)]
           [title->pre-segment-accumulator
            {pre-segment-accumulator?}
            (-> string?
                (letrec ([acc/c (and/c pre-segment-accumulator?
                                       (-> #:body string?
                                           #:page (or/c (maybe/c string?)
                                                        (list/c (maybe/c string?)
                                                                (maybe/c string?)))
                                           (recursive-contract acc/c)))])
                  acc/c))]}
          [_ {pre-segment-accumulator?}
             pre-segment-accumulator?])]
    [get-teiHeader (->m (is-a?/c teiHeader<%>))]
    [write-TEI (->*m {} {output-port?} any)]))

