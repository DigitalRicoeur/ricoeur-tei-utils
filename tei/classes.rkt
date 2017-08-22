#lang at-exp racket

(require xml 
         ricoeur/tei/interfaces
         (submod ricoeur/tei/interfaces private)
         roman-numeral
         data/maybe
         (rename-in data/functor
                    [map fmap])
         gregor
         adjutor
         )

(provide TEI%
         ;;;;
         teiHeader%
         fileDesc%
         ;;;;
         titleStmt%
         title%
         author%
         editor%
         ;;;;
         publicationStmt%
         authority%
         availability%
         ;;;;
         sourceDesc%
         bibl%
         date%
         ;;;;
         text%
         body%
         front%
         back%
         div%
         pb%
         list%
         sp%
         ab%
         p%
         head%
         note%
         item%
         )

(define (concrete-element%)
  (class element% (super-new)))

(define get-title-mixin
  (mixin {element<%>} {get-title<%>}
    (super-new)
    (inherit get-body)
    (define target
      (findf (is-a?/c get-title<%>) (get-body)))
    (define/public (get-title)
      (send target get-title))))

(define get-citation-mixin
  (mixin {element<%>} {get-citation<%>}
    (super-new)
    (inherit get-body)
    (define target
      (findf (is-a?/c get-citation<%>) (get-body)))
    (define/public (get-citation)
      (send target get-citation))
    (define/public (get-publication-date)
      (send target get-publication-date))))

(define (TEI-info-mixin %)
  (class* (get-title-mixin (get-citation-mixin %)) {TEI-info<%>}
    (super-new)))

(define TEI%
  (class* (elements-only-mixin (TEI-info-mixin guess-paragraphs-element%)) {TEI<%>}
    (super-new)
    (inherit to-xexpr get-body/elements-only get-title)
    (match-define (list teiHeader text)
      (get-body/elements-only))
    (define/public (get-teiHeader)
      teiHeader)
    (define/override-final (to-pre-segments pred
                                            call-with-metadata
                                            acc
                                            init-pb)
      (send text
            to-pre-segments
            pred
            call-with-metadata
            acc
            init-pb))
    (define/public-final (do-prepare-pre-segments pred
                                                  call-with-metadata
                                                  title->pre-segment-accumulator)
      (to-pre-segments pred
                       call-with-metadata
                       (title->pre-segment-accumulator (get-title))
                       (new pb% [name 'pb])))                          
    (define/override (get-page-breaks)
      (send text get-page-breaks))
    (define/public (write-TEI [out (current-output-port)])
      (displayln @string-append{
 <?xml version="1.0" encoding="utf-8"?>
 <!DOCTYPE TEI SYSTEM "DR-TEI.dtd">}
                 out)
      (write-xexpr (to-xexpr)))))
    
(define teiHeader%
  (class* (TEI-info-mixin (elements-only-mixin element%)) (teiHeader<%>)
    (super-new)))

(define fileDesc%
  (TEI-info-mixin (elements-only-mixin element%)))

;                                                                          
;                                                                          
;                                                                          
;                                                                          
;    ;;        ;     ;;     ;;;;              ;;     ;;              ;;    
;    ;;        ;;    ;;       ;;             ;  ;    ;;              ;;    
;  ;;;;;;;  ;;;;;  ;;;;;;;    ;;      ;;;   ;;     ;;;;;;; ; ;; ;; ;;;;;;; 
;    ;;        ;;    ;;       ;;    ;;   ;   ;;      ;;    ;; ;; ;   ;;    
;    ;;        ;;    ;;       ;;    ;    ;     ;     ;;    ;; ;; ;;  ;;    
;    ;;        ;;    ;;       ;;   ;;;;;;;;     ;    ;;    ;; ;; ;;  ;;    
;    ;;        ;;    ;;       ;;    ;           ;;   ;;    ;; ;; ;;  ;;    
;     ;        ;;     ;        ;    ;;   ;  ;   ;     ;    ;; ;; ;;   ;    
;      ;;;     ;;      ;;;      ;;    ;;;    ;;;       ;;; ;; ;; ;;    ;;; 
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(define titleStmt%
  (class* (elements-only-mixin element%) (get-title<%>)
    (super-new)
    (inherit get-body)
    (define promise:title
      (delay
        (string-join (for/list ([t (in-list (get-body))]
                                #:when ((is-a?/c title%) t))
                       (string-normalize-spaces
                        (string-trim (send t to-plain-text))))
                     ": ")))
    (define/public (get-title)
      (force promise:title))))

(define title%
  (concrete-element%))

(define author%
  (concrete-element%))

(define editor%
  (concrete-element%))

;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;                   ;;      ;;;;       ;                     ;;        ;                   
;                   ;;        ;;       ;;                    ;;        ;;                  
;   ; ;;    ;;  ;;  ;;;;      ;;    ;;;;;      ;;;    ;;   ;;;;;;;  ;;;;;    ;;;    ;; ;   
;   ;;  ;   ;;  ;;  ;;  ;     ;;       ;;    ;;   ;  ;  ;    ;;        ;;   ;   ;   ;;; ;  
;   ;;  ;   ;;  ;;  ;;  ;     ;;       ;;    ;          ;;   ;;        ;;   ;   ;   ;;  ;; 
;   ;;  ;;  ;;  ;;  ;;  ;;    ;;       ;;   ;;        ;;;;   ;;        ;;  ;;   ;;  ;;  ;; 
;   ;;  ;   ;;  ;;  ;;  ;     ;;       ;;    ;       ;  ;;   ;;        ;;   ;   ;   ;;  ;; 
;   ;;  ;    ; ;;;  ;;  ;      ;       ;;    ;;   ; ;;  ;;    ;        ;;   ;   ;   ;;  ;; 
;   ;;;;      ; ;;  ; ;;        ;;     ;;      ;;;   ;;; ;     ;;;     ;;    ;;;    ;;  ;; 
;   ;;                                                                                     
;   ;;                                                                                     
;   ;;                                                                                     
;                                                                                          

(define publicationStmt%
  (elements-only-mixin element%))

(define authority%
  (concrete-element%))

(define availability%
  (elements-only-mixin element%))

;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;                                                   ;;;;                           
;                                                   ;;  ;;                         
;     ;;     ;;;    ;;  ;;  ;; ;;;     ;;;    ;;;   ;;   ;    ;;;     ;;       ;;; 
;   ;;  ;   ;   ;   ;;  ;;  ;;;      ;;   ; ;;   ;  ;;   ;  ;;   ;  ;;  ;    ;;   ;
;    ;      ;   ;   ;;  ;;  ;;       ;      ;    ;  ;;   ;; ;    ;   ;       ;     
;     ;;   ;;   ;;  ;;  ;;  ;;      ;;     ;;;;;;;; ;;   ; ;;;;;;;;   ;;    ;;     
;       ;;  ;   ;   ;;  ;;  ;;       ;      ;       ;;   ;  ;           ;;   ;     
;   ;   ;   ;   ;    ; ;;;  ;;       ;;   ; ;;   ;  ;;  ;;  ;;   ;  ;   ;    ;;   ;
;    ;;;     ;;;      ; ;;  ;;         ;;;    ;;;   ;;;;      ;;;    ;;;       ;;; 
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  

(define sourceDesc%
  (get-citation-mixin
   (elements-only-mixin element%)))

(define bibl%
  (class* element% {get-citation<%>}
    (super-new)
    (inherit to-plain-text)
    (inherit get-body)
    (define/public (get-citation)
      (string-normalize-spaces (to-plain-text)))
    (define pr:maybe-date
      (delay (from-just nothing
                        (fmap (λ (d) (send d get-publication-date))
                              (false->maybe (findf (is-a?/c date%) (get-body)))))))
    (define/public (get-publication-date)
      (force pr:maybe-date))))

(define date%
  (class* element% {get-publication-date<%>}
    (super-new)
    (inherit get-attributes)
    (define maybe-date
      (fmap (compose1 iso8601->date car)
            (false->maybe (dict-ref (get-attributes) 'when #f))))
    (define/public (get-publication-date)
      maybe-date)))


;                                  
;                                  
;                                  
;                                  
;    ;;                      ;;    
;    ;;                      ;;    
;  ;;;;;;;    ;;;  ;;   ;; ;;;;;;; 
;    ;;     ;;   ;   ;  ;    ;;    
;    ;;     ;    ;   ; ;     ;;    
;    ;;    ;;;;;;;;   ;      ;;    
;    ;;     ;        ; ;     ;;    
;     ;     ;;   ;  ;   ;     ;    
;      ;;;    ;;;  ;;   ;;     ;;; 
;                                  
;                                  
;                                  
;                                  

(define text%
  (class element:elements-only+guess-paragraphs+to-pre-segments%
    (super-new)))

(define body% 
  (class element:elements-only+guess-paragraphs+to-pre-segments%
    (super-new)))

(define front% 
  (class element:elements-only+guess-paragraphs+to-pre-segments%
    (super-new)
    (define/augment-final (to-pre-segments/add-metadata pred
                                                        call-with-metadata
                                                        thunk)
      (call-with-metadata #:location 'front thunk))))
      
(define back% 
  (class element:elements-only+guess-paragraphs+to-pre-segments%
    (super-new)
    (define/augment-final (to-pre-segments/add-metadata pred
                                                        call-with-metadata
                                                        thunk)
      (call-with-metadata #:location 'back thunk))))

(define div%
  (class element:elements-only+guess-paragraphs+to-pre-segments%
    (super-new)
    (inherit get-attributes)
    (define n
      (fmap car (false->maybe (dict-ref (get-attributes) 'n #f))))
    (define type
      (car (dict-ref (get-attributes) 'type)))
    (define/augment-final (to-pre-segments/add-metadata pred
                                                        call-with-metadata
                                                        thunk)
      (call-with-metadata #:location (list 'div type n) thunk))
    (define/override-final (to-pre-segments pred
                                            call-with-metadata
                                            acc
                                            init-pb)
      (case (dict-ref (get-attributes) 'type #f)
        [(("contents")("index"))
         (values acc init-pb)]
        [else
         (super to-pre-segments
                pred
                call-with-metadata
                acc
                init-pb)]))))
  
(define pb%
  (class* (elements-only-mixin element%) {pb<%>}
    (super-new)
    (inherit get-attributes)
    (define n
      (fmap car (false->maybe (dict-ref (get-attributes) 'n #f))))
    (define-values {kind num}
      (match (from-just #f n)
        [#f (values 'none nothing)]
        [(app string->number (? number? it))
         (values 'number (just it))]
        [(app (curry exn->maybe exn:fail? roman->number)
              (just it))
         (values 'roman (just it))]
        [(? string? it)
         (values 'other nothing)]))
    (define/public (get-page-string)
      n)
    (define/public (get-kind)
      kind)
    (define/public (get-numeric)
      num)
    (define/override (to-plain-text)
      "\f")
    (define/public (get-page-breaks)
      (list this))
    (define/override (smoosh)
      (list this))))

(define list%
  ;TODO: specialize to-plain-text
  (class element:elements-only+guess-paragraphs+to-pre-segments%
    (super-new)))

(define sp%
  (class element:elements-only+guess-paragraphs+to-pre-segments%
    (super-new)
    (inherit get-attributes)
    (define/augment-final (to-pre-segments/add-metadata pred
                                                        call-with-metadata
                                                        thunk)
      (call-with-metadata #:resp (car (dict-ref (get-attributes) 'who))
                          thunk))))


;                                                          
;                                                          
;                                                          
;                                                          
;                            ;;                      ;;    
;                            ;;                      ;;    
;      ;;;   ;;;    ;; ;   ;;;;;;;    ;;;   ;; ;   ;;;;;;; 
;    ;;   ; ;   ;   ;;; ;    ;;     ;;   ;  ;;; ;    ;;    
;    ;      ;   ;   ;;  ;;   ;;     ;    ;  ;;  ;;   ;;    
;   ;;     ;;   ;;  ;;  ;;   ;;    ;;;;;;;; ;;  ;;   ;;    
;    ;      ;   ;   ;;  ;;   ;;     ;       ;;  ;;   ;;    
;    ;;   ; ;   ;   ;;  ;;    ;     ;;   ;  ;;  ;;    ;    
;      ;;;   ;;;    ;;  ;;     ;;;    ;;;   ;;  ;;     ;;; 
;                                                          
;                                                          
;                                                          
;                                                          


(define ab%
  (let ()
    (struct parbreak ())
    (class* (content-containing-element-mixin body-element%) {ab<%>}
      (super-new)
      (inherit get-body get-page-breaks)
      (define/private (insert-parbreaks #:mode [mode 'blank-lines])
        (define split-pat
          (if (eq? 'blank-lines mode)
              #px"\n[ \t\f\r]*\n|\r[ \t\f]*\r"
              #rx"\n"))
        (flatten
         (for/list ([child (in-list (get-body))])
           (cond
             [(string? child)
              (add-between (regexp-split split-pat child)
                           (parbreak))]
             [else
              child]))))
      (define/private (group-by-parbreaks #:mode [mode 'blank-lines])
        (let loop ([this-group null]
                   [to-go (insert-parbreaks #:mode mode)])
          (match to-go
            ['() (list this-group)]
            [(cons (? parbreak?) more)
             (cons this-group
                   (loop null more))]
            [(cons (pregexp #px"^\\s*$") more)
             (loop this-group more)]
            [(cons this-item more)
             (loop (append this-group
                           (list this-item))
                   more)])))
      (define/public-final (do-guess-paragraphs #:mode [mode 'blank-lines])
        (for/list ([pargroup (in-list (group-by-parbreaks #:mode mode))]
                   #:unless (null? pargroup))
          (match pargroup
            [(list (? (is-a?/c pb<%>) elem))
             elem]
            [_
             (new p%
                  [name 'p]
                  [body pargroup])])))
      (define/override-final (to-pre-segments pred
                                              call-with-metadata
                                              acc
                                              init-pb)
        (cond
          [#f (error 'todo)]
          [else
           (super to-pre-segments
                  pred
                  call-with-metadata
                  acc
                  init-pb)]))
      #|END ab%|#)))

(define p%
  (class* (content-containing-element-mixin guess-paragraphs-element%)
    {p<%>}
    (super-new)
    (inherit get-body)
    (define/override (to-plain-text)
      (define body
        (get-body))
      (cond
        [(null? body)
         ""]
        [else
         (let* ([body (for/list ([child (in-list body)])
                        (if (tei-element? child)
                            child
                            (element-or-xexpr->plain-text child)))]
                [body (if (string? (first body))
                          (cons (string-trim (first body)
                                             #:right? #f)
                                (rest body))
                          body)]
                [body (if (string? (last body))
                          (append (drop-right body 1)
                                  (list (string-trim (last body)
                                                     #:left? #f)))
                          body)]
                [body (for/list ([child (in-list body)])
                        (if (string? child)
                            (string-normalize-spaces child #:trim? #f)
                            (send child to-plain-text)))])
           (string-join body
                        ""
                        #:before-first "\n"
                        #:after-last "\n"))]))))

(define head%
  (content-containing-element-mixin
   guess-paragraphs-element%))

(define note%
  (class (content-containing-element-mixin
          guess-paragraphs-element%)
    (super-new)
    (inherit get-attributes)
    (define/augment-final (to-pre-segments/add-metadata pred
                                                        call-with-metadata
                                                        thunk)
      (let ([n (car (dict-ref (get-attributes) 'n))]
            [place (car (dict-ref (get-attributes) 'place))]
            [transl (car (dict-ref (get-attributes) 'transl '(#f)))])
        (call-with-metadata #:location (list 'note place n transl)
                            thunk)))))
                     
        
(define item%
  ;TODO: specialize to-plain-text
  (content-containing-element-mixin
   guess-paragraphs-element%))
