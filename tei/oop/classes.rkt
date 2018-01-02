#lang at-exp racket

(require xml 
         ricoeur/tei/oop/interfaces
         (submod ricoeur/tei/oop/interfaces private)
         ricoeur/tei/xmllint
         roman-numeral
         data/maybe
         (rename-in data/functor
                    [map fmap])
         gregor
         openssl/md5
         adjutor
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
         )

(define current-filename
  (make-parameter #f))

(define current-full-path
  (make-parameter #f))

(define (concrete-element%)
  (class element% (super-new)))

(define get-title?
  (is-a?/c get-title<%>))

(define get-title-mixin
  (mixin {element<%>} {get-title<%>}
    (super-new)
    (inherit get-body)
    (define target
      (findf get-title? (get-body)))
    (define/public (get-resp-string resp)
      (send target get-resp-string resp))
    (define/public (get-title)
      (send target get-title))))

(define get-citation?
  (is-a?/c get-citation<%>))

(define get-citation-mixin
  (mixin {element<%>} {get-citation<%>}
    (super-new)
    (inherit get-body)
    (define target
      (findf get-citation? (get-body)))
    (define/public (get-citation)
      (send target get-citation))
    (define/public (get-original-publication-date)
      (send target get-original-publication-date))
    (define/public (get-publication-date)
      (send target get-publication-date))))

(define classification?
  (is-a?/c classification<%>))

(define classification-mixin
  (mixin {element<%>} {classification<%>}
    (super-new)
    (inherit get-body)
    (define target
      (findf classification? (get-body)))
    (define/public (get-book/article)
      (send target get-book/article))))

(define (TEI-info-mixin %)
  (class* (classification-mixin (get-title-mixin (get-citation-mixin %)))
    {TEI-info<%>}
    (super-new)
    (define full-path
      (current-full-path))
    (define filename
      (current-filename))
    (define/public-final (get-full-path)
      full-path)
    (define/public-final (get-filename)
      filename)))

(define TEI%
  (class* (elements-only-mixin guess-paragraphs-element%) {TEI<%> TEI-info<%>}
    (super-new)
    (inherit to-xexpr
             get-attributes
             get-body
             get-body/elements-only)
    (match-define (list teiHeader text)
      (get-body/elements-only))
    #|(define/public-final (TMP-add-profileDesc profileDesc-elem)
      (define new-header
        (send teiHeader TMP-add-profileDesc profileDesc-elem))
      (new this%
           [name 'TEI]
           [attributes (get-attributes)]
           [body (let loop ([body (get-body)])
                   (match body
                     [(cons (? classification?) more)
                      (cons new-header more)]
                     [(cons other more)
                      (cons other (loop more))]))]))|#
    (define/TEI-info teiHeader)
    (define pr:md5
      (delay/thread
       (define-values (in-from-pipe out-to-pipe)
         (make-pipe))
       (write-TEI out-to-pipe)
       (close-output-port out-to-pipe)
       (md5 in-from-pipe)))
    (define/public-final (get-md5)
      (force pr:md5))
    (define/public-final (get-teiHeader)
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
      (define-values {acc _}
        (to-pre-segments pred
                         call-with-metadata
                         (title->pre-segment-accumulator (get-title))
                         (new pb% [name 'pb])))
      acc)
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
  (class* (TEI-info-mixin (elements-only-mixin element%)) {teiHeader<%>}
    (super-new)
    (inherit get-attributes get-body)
    #|END teiHeader%|#))

(define fileDesc%
  ;; no longer a tei-info<%> 
  (get-title-mixin (get-citation-mixin (elements-only-mixin element%))))

(define profileDesc%
  (classification-mixin (elements-only-mixin element%)))

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
  (concrete-element%))

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
    (define pr:publication-date
      (delay (for/first ([c (in-list (get-body))]
                         #:when (date-obj? c)
                         #:unless (equal? "original" (send c get-subtype)))
               (send c get-when))))
    (define pr:orig-date
      (delay (or (for/first ([c (in-list (get-body))]
                             #:when (date-obj? c)
                             #:when (equal? "original" (send c get-subtype)))
                   (send c get-when))
                 (force pr:publication-date))))
    (define/public (get-publication-date)
      (force pr:publication-date))
    (define/public (get-original-publication-date)
      (force pr:orig-date))))

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
  (class* (elements-only-mixin element%) {classification<%>}
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
  (elements-only-mixin element%))

(define term%
  (class element%
    (super-new)))

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
    (class* (ab-to-pre-segments-mixin body-element%) {ab<%>}
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
