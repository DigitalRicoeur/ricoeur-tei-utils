#lang at-exp racket

(require ricoeur/tei/oop-kernel
         (only-in xml write-xexpr)
         (only-in ricoeur/tei/normalize-placeholder
                  non-element-body->plain-text)
         roman-numeral
         data/maybe
         (rename-in data/functor
                    [map fmap])
         gregor
         openssl/md5
         adjutor
         ricoeur/tei/oop/interfaces
         )

(provide TEI%
         ;;;;
         teiHeader%
         fileDesc%
         profileDesc%
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
         textClass%
         catRef%
         keywords%
         term%
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
         ;;
         current-filename
         current-full-path
         current-object-modify-seconds
         )

(define current-filename
  (make-parameter #f))

(define current-full-path
  (make-parameter #f))

(define current-object-modify-seconds
  (make-parameter #f))





  
(define-local-member-name update-guess-paragraphs-status)

(define update-guess-paragraphs-status-mixin
  (let ([guess-paragraphs-status?
         (is-a?/c tei-guess-paragraphs-status<%>)])
    (mixin {element<%>} {}
      (super-new)
      (inherit get-name get-attributes get-body)
      (define/public (update-guess-paragraphs-status new-status)
        (new this%
             [name (get-name)]
             [attributes (get-attributes)]
             [body (list/pred/update (get-body)
                                     guess-paragraphs-status?
                                     (λ (target)
                                       (send target
                                             update-guess-paragraphs-status
                                             new-status)))])))))

(define (list/pred/update lst pred? update)
  (map (λ (v)
         (if (pred? v)
             (update v)
             v))
       lst))

(define guess-paragraphs-status-mixin
  (let ([guess-paragraphs-status? (is-a?/c tei-guess-paragraphs-status<%>)])
    (λ (%) 
      (class* (update-guess-paragraphs-status-mixin %)
        {tei-guess-paragraphs-status<%>}
        (super-new)
        (inherit get-body)
        (define target
          (findf guess-paragraphs-status? (get-body)))
        (define/public-final (get-guess-paragraphs-status)
          (send target get-guess-paragraphs-status))))))



(define TEI%
  (class* (update-guess-paragraphs-status-mixin
           (elements-only-mixin
            (guess-paragraphs-mixin
             (get-page-breaks-mixin element%))))
    {TEI<%> TEI-info<%>}
    (super-new)
    (inherit to-xexpr
             get-attributes
             get-body
             get-body/elements-only)
    (match-define (list teiHeader text)
      (get-body/elements-only))
    (define/TEI-info teiHeader)
    (define pr:md5
      (delay/sync
       (define-values (in-from-pipe out-to-pipe)
         (make-pipe))
       (write-TEI out-to-pipe)
       (close-output-port out-to-pipe)
       (md5 in-from-pipe)))
    (define/public-final (get-md5)
      (force pr:md5))
    (define/public-final (get-teiHeader)
      teiHeader)
    (define/override (guess-paragraphs #:mode [mode 'blank-lines])
      (send (super guess-paragraphs #:mode mode)
            update-guess-paragraphs-status
            mode))
    (define/override (get-page-breaks)
      (send text get-page-breaks))
    (define/public-final (write-TEI [out (current-output-port)])
      (parameterize ([current-output-port out])
        (call/prettyprint-xml-out
         (λ () 
           (displayln @string-append{
 <?xml version="1.0" encoding="utf-8"?>
 <!DOCTYPE TEI SYSTEM "DR-TEI.dtd">})
           (write-xexpr (to-xexpr))))))))






(define teiHeader%
  (class* (guess-paragraphs-status-mixin
           (classification-mixin
            (get-title-mixin
             (get-citation-mixin (elements-only-mixin element%)))))
    {teiHeader<%>}
    (super-new)
    (inherit get-attributes get-body)
    (define full-path
      (current-full-path))
    (define filename
      (current-filename))
    (define secs
      (current-object-modify-seconds))
    (define/public-final (get-modify-seconds)
      secs)
    (define/public-final (get-full-path)
      full-path)
    (define/public-final (get-filename)
      filename)
    #|END teiHeader%|#))



(define fileDesc%
  ;; no longer a tei-info<%> 
  (get-title-mixin
   (get-citation-mixin
    (elements-only-mixin element%))))



(define profileDesc%
  (guess-paragraphs-status-mixin
   (classification-mixin
    (elements-only-mixin element%))))

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

(define can-have-id?
  (is-a?/c can-have-id<%>))

(define title%
  (class element%
    (super-new)))

(define title?
  (is-a?/c title%))

(define titleStmt%
  (class* (elements-only-mixin element%) (get-title<%>)
    (super-new)
    (inherit get-body)
    (define promise:title
      (delay
        (string-join (for/list ([t (in-list (get-body))]
                                #:when (title? t))
                       (string-normalize-spaces
                        (string-trim (send t to-plain-text))))
                     ": ")))
    (define promise:resp-table
      (delay (for*/hasheq ([child (in-list (get-body))]
                           #:when (can-have-id? child)
                           [id (in-value (send child get-id-or-false))]
                           #:when id)
               (values id
                       (string-normalize-spaces
                        (string-trim (send child to-plain-text)))))))
    (define/public (get-resp-string resp)
      (or (hash-ref (force promise:resp-table) resp #f)
          (raise-arguments-error 'get-resp-string
                                 "given resp symbol not found"
                                 "given" resp)))
    (define/public (get-title)
      (force promise:title))))

(define can-have-id-mixin
  (mixin {element<%>} {can-have-id<%>}
    (super-new)
    (inherit get-attributes)
    (define promise:id
      (delay
        (let ([v (dict-ref (get-attributes) 'xml:id #f)])
          (and v (string->symbol (car v))))))
    (define/public (get-id-or-false)
      (force promise:id))))

(define author%
  (class (can-have-id-mixin element%)
    (super-new)))

(define editor%
  (class (can-have-id-mixin element%)
    (super-new)))

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
  (class element%
    (super-new)))

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
    (define publication-date
      (for/first ([c (in-list (get-body))]
                         #:when (date-obj? c)
                         #:unless (equal? "original" (send c get-subtype)))
               (send c get-when)))
    (define orig-date
      (or (for/first ([c (in-list (get-body))]
                             #:when (date-obj? c)
                             #:when (equal? "original" (send c get-subtype)))
                   (send c get-when))
          publication-date))
    (define/public (get-publication-date)
      publication-date)
    (define/public (get-original-publication-date)
      orig-date)))

(define-member-name get-subtype (generate-member-key))
(define-member-name get-when (generate-member-key))

(define date%
  (class element% 
    (super-new)
    (inherit get-attributes)
    (match-define (list-no-order (list 'subtype subtype)
                                 (list 'when (app iso8601->date
                                                  when-attr))
                                 _ ...)
      (get-attributes))
    (define/public-final (get-subtype)
      subtype)
    (define/public-final (get-when)
      when-attr)))

(define date-obj?
  (is-a?/c date%))


;                                                                          
;                                                                          
;                                                                          
;                                                                          
;    ;;                      ;;       ;;;   ;;;;                           
;    ;;                      ;;      ;   ;    ;;                           
;  ;;;;;;;    ;;;  ;;   ;; ;;;;;;;  ;         ;;      ;;      ;;      ;;   
;    ;;     ;;   ;   ;  ;    ;;     ;         ;;     ;  ;   ;;  ;   ;;  ;  
;    ;;     ;    ;   ; ;     ;;    ;;         ;;        ;;   ;       ;     
;    ;;    ;;;;;;;;   ;      ;;     ;         ;;      ;;;;    ;;      ;;   
;    ;;     ;        ; ;     ;;     ;         ;;     ;  ;;      ;;      ;; 
;     ;     ;;   ;  ;   ;     ;      ;   ;     ;    ;;  ;;  ;   ;   ;   ;  
;      ;;;    ;;;  ;;   ;;     ;;;    ;;;       ;;   ;;; ;   ;;;     ;;;   
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(define textClass%
  (class* (guess-paragraphs-status-mixin (elements-only-mixin element%))
    {classification<%>}
    (super-new)
    (inherit get-body/elements-only)
    (define book/article
      (for/first ([it (in-list (get-body/elements-only))]
                  #:when (eq? 'catRef (send it get-name)))
        (send it get-book/article)))
    (define/public (get-book/article)
      book/article)))

(define catRef%
  (class (elements-only-mixin element%)
    (super-new)
    (inherit get-attributes)
    (define book/article
      (case (car (dict-ref (get-attributes) 'target))
        [("https://schema.digitalricoeur.org/taxonomy/type#article")
         'article]
        [("https://schema.digitalricoeur.org/taxonomy/type#book")
         'book]))
    (define/public (get-book/article)
      book/article)))

(define keywords%
  (class* (elements-only-mixin element%) {tei-guess-paragraphs-status<%>}
    (super-new)
    (inherit get-attributes get-body get-body/elements-only)
    (define status
      (string->symbol
       (non-element-body->plain-text
        (send (car (get-body/elements-only))
              get-body))))
    (define/public-final (get-guess-paragraphs-status)
      status)
    (define/public-final (update-guess-paragraphs-status new-status)
      (new this%
           [name 'keywords]
           [attributes (get-attributes)]
           [body (list/pred/update
                  (get-body)
                  term?
                  (λ (_)
                    (new term%
                         [name 'term]
                         [attributes '()]
                         [body (list (symbol->string new-status))])))]))))
    
(define term%
  (class element%
    (super-new)))

(define term?
  (is-a?/c term%))

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
  (class (elements-only-mixin
          (guess-paragraphs-mixin
           (get-page-breaks-mixin element%)))
    (super-new)))

(define body% 
  (class (elements-only-mixin
          (guess-paragraphs-mixin
           (get-page-breaks-mixin element%)))
    (super-new)))

(define front% 
  (class (elements-only-mixin
          (guess-paragraphs-mixin
           (get-page-breaks-mixin element%)))
    (super-new)))

(define back% 
  (class (elements-only-mixin
          (guess-paragraphs-mixin
           (get-page-breaks-mixin element%)))
    (super-new)))

(define div%
  (class* (elements-only-mixin
          (guess-paragraphs-mixin 
           (get-page-breaks-mixin element%)))
    {div<%>}
    (super-new)
    (inherit get-attributes)
    (define n
      (fmap car (false->maybe (dict-ref (get-attributes) 'n #f))))
    (define type
      (string->symbol
       (car (dict-ref (get-attributes) 'type))))
    (public*
     [get-n (λ () n)]
     [get-type (λ () type)])
    #|END div%|#))
  
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
      (list this))))

(define list%
  ;TODO: specialize to-plain-text
  (class (elements-only-mixin
          (guess-paragraphs-mixin 
           (get-page-breaks-mixin element%)))
    (super-new)))

(define sp%
  (class (elements-only-mixin
          (guess-paragraphs-mixin
           (get-page-breaks-mixin element%)))
    (super-new)))


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
    (class* (get-page-breaks-mixin element%) {ab<%>}
      (super-new)
      (inherit get-body)
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
      #|END ab%|#)))

(define p%
  (class* (guess-paragraphs-mixin
           (get-page-breaks-mixin element%))
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
  (guess-paragraphs-mixin
   (get-page-breaks-mixin element%)))

(define note%
  (class* (guess-paragraphs-mixin
          (get-page-breaks-mixin element%))
    {get-page-breaks<%>}
    (super-new)
    (inherit get-attributes)
    (define n
      (car (dict-ref (get-attributes) 'n)))
    (define place
      (string->symbol
       (car (dict-ref (get-attributes) 'place))))
    (define transl
      (and (car (dict-ref (get-attributes) 'transl '(#f)))
           'transl))
    (public*
     [get-n (λ () n)]
     [get-place (λ () place)]
     [get-transl? (λ () transl)])))
    
        
(define item%
  ;TODO: specialize to-plain-text
  (guess-paragraphs-mixin
   (get-page-breaks-mixin element%)))








