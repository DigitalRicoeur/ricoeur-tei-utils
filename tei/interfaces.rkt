#lang racket

(require xml 
         ricoeur/tei/xml-entity-utils
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
           get-title<%>
           get-publication-date<%>
           get-citation<%>
           body-element%
           guess-paragraphs-element%
           ab?
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
                         [to-xexpr (->m xexpr/c)]
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

(define get-title<%>
  (interface ()
    [get-title (->m string?)]))

(define get-publication-date<%>
  (interface ()
    [get-publication-date (->m (maybe/c date?))]))

(define get-citation<%>
  (interface (get-publication-date<%>)
    [get-citation (->m string?)]))

(define TEI-info<%>
  (interface (get-title<%> get-citation<%>)))

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

(define TEI-body<%>
  (interface (element<%>)
    [smoosh (->m (listof (or/c string? (recursive-contract (is-a?/c pb<%>)))))]
    [get-page-breaks (->m (listof (recursive-contract (is-a?/c pb<%>))))]
    ))

(define pb<%>
  (interface (TEI-body<%>)
    [get-page-string (->m (maybe/c string?))]
    [get-kind (->m (or/c 'none 'number 'roman 'other))]
    [get-numeric (->m (maybe/c number?))]
    ))

(define body-element?
  (is-a?/c TEI-body<%>))

(define body-element%
  (class* element% {TEI-body<%>}
    (super-new)
    (inherit get-attributes get-body)
    (define/public (smoosh) 
      (case (dict-ref (get-attributes) 'resp '("#ricoeur"))
        [(("#ricoeur"))
         (flatten
          (for/list ([child (in-list (get-body))])
            (if (body-element? child)
                (send child smoosh)
                (element-or-xexpr->plain-text child))))]
        [else
         '()]))
    (define/public (get-page-breaks) 
      (flatten
       (for/list ([child (in-list (get-body))]
                  #:when (body-element? child))
         (send child get-page-breaks))))))
    
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






(define TEI<%>
  (interface (element<%>
              TEI-info<%>
              TEI-body<%>
              guess-paragraphs<%>
              elements-only<%>)
    [get-teiHeader (->m (is-a?/c teiHeader<%>))]
    [write-TEI (->*m {} {output-port?} any)]))

