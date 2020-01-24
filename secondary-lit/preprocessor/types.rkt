#lang typed/racket/no-check ;; FIXME !!!
;; The bug in https://github.com/racket/typed-racket/issues/902
;; occurs even with /no-check in this module.

;; TODO:
;;   - Should we require a word boundary around matches?
;;   - Should we consider matches in any of:
;;       - whole-book abstracts
;;       - whole-book titles
;;       - journal titles (as opposed to article titles)
;;   - Some journal articles seem to have no abstract but lots of
;;     text in mixed-citation elements.

(provide metadata-parsing-store?
         make-metadata-parsing-store
         metadata-parsing-store-insert!
         metadata-parsing-store->archive
         ;;;;;;;;
         parsed-article?
         make-parsed-article
         journal-title?
         make-journal-title
         parsed-article-title?
         make-parsed-article-title
         ;;;;;;;;
         parsed-book?
         make-parsed-book
         parsed-whole-book-meta?
         make-parsed-whole-book-meta
         parsed-book-part?
         make-parsed-book-part
         ;;;;;;;;
         parsed-contrib?
         make-collab-contrib
         make-unstructured-contrib
         make-surname-contrib
         make-given-only-contrib
         ;;;;;;;;
         parsed-pub-date?
         make-parsed-pub-date
         ;;;;;;;;
         parsed-abstract?
         make-parsed-abstract
         ;;;;;;;;
         parsed-phrasing-content?
         make-parsed-phrasing-content
         whitespace-string?
         Strict-Xexpr
         Html-Forest)

(require "../kernel/types.rkt"
         (submod "../kernel/types.rkt"
                 private-for-preprocessor))
    

(struct metadata-parsing-store
  ;; FIXME: could be pure if we used a lower-level API from file/unzip
  ([spec : Topic-Model-Spec]
   [keys : (Listof Symbol)]
   [rxs : (Listof Regexp)]
   [sema : Semaphore] ;; no concurrency for now, but avoid bugs later
   [books-box : (Boxof (Listof whole-book-meta))]
   [items-box : (Boxof (Listof metadata-item))]))

(: metadata-parsing-store->archive
   (-> metadata-parsing-store Metadata-Archive))
(define (metadata-parsing-store->archive store)
  (define books-lst (unbox (metadata-parsing-store-books-box store)))
  (define books-vec : (Immutable-Vectorof whole-book-meta)
    (vector->immutable-vector
     (list->vector (reverse books-lst))))
  (make-metadata-archive-from-preprocessor
   (secondary-lit-metadata-archive
    (metadata-parsing-store-spec store)
    books-vec
    (unbox (metadata-parsing-store-items-box store)))))

(: metadata-parsing-store-insert! (-> metadata-parsing-store
                                      (U parsed-article parsed-book)
                                      Boolean))
(define (metadata-parsing-store-insert! store this)
  (cond
    [(parsed-article? this)
     (call-with-semaphore (metadata-parsing-store-sema store)
       insert-article!/have-lock
       #f
       store
       this)]
    [else
     (call-with-semaphore (metadata-parsing-store-sema store)
       insert-book!/have-lock
       #f
       store
       this)]))

(: insert-article!/have-lock (-> metadata-parsing-store parsed-article Boolean))
(define (insert-article!/have-lock store this)
  (define matches
    (get-matches store this))
  (cond
    [(null? matches)
     #f]
    [else
     (define bx (metadata-parsing-store-items-box store))
     (set-box! bx
               (cons (metadata-item
                      (parsed-article-prefab this)
                      matches)
                     (unbox bx)))
     #t]))


(: insert-book!/have-lock (-> metadata-parsing-store parsed-book Boolean))
(define (insert-book!/have-lock store this)
  ;; Length is amortized constant time, and calculating the id
  ;; this way makes it correct by construction.
  (define books-bx (metadata-parsing-store-books-box store))
  (define id (length (unbox books-bx)))
  (define new-items
    (for*/list : (Listof metadata-item)
      ([pt (in-list (parsed-book-parts this))]
       [matches (in-value (get-matches store pt))]
       #:unless (null? matches))
      (metadata-item (cons (parsed-book-part-prefab pt) id)
                     matches)))
  (cond
    [(null? new-items)
     #f]
    [else
     (set-box! books-bx (cons (parsed-book-whole this) (unbox books-bx)))
     (define items-bx (metadata-parsing-store-items-box store))
     (set-box! items-bx
               (append new-items
                       (unbox items-bx)))
     #t]))

(: get-matches (-> metadata-parsing-store
                   parsed-thing
                   (Listof (Pairof Symbol Positive-Index))))
(define (get-matches store it)
  (define str (parsed-thing-normalized-string it))
  ;; for/lists is needed because TR's for/list doesn't
  ;; support internal #:when clauses, which we need for in-value
  (for/lists ([ret : (Listof (Pairof Symbol Positive-Index))])
             ([k (in-list (metadata-parsing-store-keys store))]
              [rx (in-list (metadata-parsing-store-rxs store))]
              #:when #t
              [num-matches (in-value (regexp-count-matches rx str))]
              #:when num-matches)
    (cons k num-matches)))

(: regexp-count-matches (-> Regexp String (U #f Positive-Index)))
(define (regexp-count-matches pattern input)
  ;; workaround for https://github.com/racket/typed-racket/issues/901
  (define len (length (or (regexp-match-positions* pattern input) null)))
  (if (positive? len)
      len
      #f))


(: make-metadata-parsing-store (-> Topic-Model metadata-parsing-store))
(define (make-metadata-parsing-store model)
  (define spec (topic-model-spec model)) 
  (define keys
    (sort (hash-keys
           (for*/hasheq : (Immutable-HashTable Symbol #t)
             ([rhs (in-immutable-hash-values spec)]
              [word (in-list (cdr rhs))])
             (values word #t)))
          symbol<?))
  (define rxs
    (map (λ ([sym : Symbol])
           (regexp (regexp-quote (symbol->string sym) #f)))
         keys))
  (metadata-parsing-store spec
                          keys
                          rxs
                          (make-semaphore 1)
                          (box null)
                          (box null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct parsed-thing
  ([normalized-string : String]))

(define-type Normalized-Jumble
  ;; elements of these jumbles will be from
  ;; separate `parsed-thing` components,
  ;; so add " " between them
  (U String
     #f
     Null
     (Pairof Normalized-Jumble Normalized-Jumble)))

(: normalized-jumble->string (-> Normalized-Jumble String))
(define (normalized-jumble->string jumble)
  (string->immutable-string
   (string-join (let jumble->list : (Listof String)
                  ([jumble jumble]
                   [tail : (Listof String) null])
                  (cond
                    [(string? jumble)
                     (cons jumble tail)]
                    [(pair? jumble)
                     (jumble->list (car jumble)
                                   (jumble->list (cdr jumble)
                                                 tail))]
                    [else
                     tail]))
                " ")))

;                                    
;               ;;  ;;       ;;      
;               ;;           ;;      
;    ;;    ;; ;;;;;;;;  ;;;  ;;  ;;  
;   ;  ;   ;;;  ;;  ;; ;   ; ;; ;  ; 
;      ;;  ;;   ;;  ;; ;     ;; ;  ; 
;    ;;;;  ;;   ;;  ;;;;     ;;;;;;;;
;   ;  ;;  ;;   ;;  ;; ;     ;; ;    
;  ;;  ;;  ;;    ;  ;; ;   ; ;; ;    
;   ;;; ;  ;;    ;;;;;  ;;;   ;  ;;; 
;                                    

(struct parsed-article parsed-thing
  ([prefab : article]))



(define (make-parsed-article #:journal-title [j-title : (U #f journal-title)]
                             #:publisher [publisher : String]
                             #:id [id : (Pairof Symbol String)]
                             #:self-uri [self-uri : (U #f String)]
                             #:volume [volume : (U #f String)]
                             #:issue [issue : (U #f String)]
                             #:article-title [a-title : (U #f parsed-article-title)]
                             #:contributors [contribs : (Listof parsed-contrib)]
                             #:pub-dates [pub-dates : (Listof parsed-pub-date)]
                             #:abstract [opaque-abstract : (U #f parsed-abstract)])
  : parsed-article
  (define normalized-string
    (normalized-jumble->string
     (list (and a-title (parsed-article-title-jumble a-title))
           #;(and j-title (cons (journal-title-main-title j-title)
                                (journal-title-subtitle j-title)))
           (and opaque-abstract
                (parsed-thing-normalized-string opaque-abstract)))))
  (parsed-article
   normalized-string
   (article j-title
            publisher
            id
            self-uri
            volume
            issue
            (and a-title (parsed-article-title-prefab a-title))
            (map parsed-contrib-prefab contribs)
            (map parsed-pub-date-prefab pub-dates)
            (and opaque-abstract
                 (parsed-abstract-prefab opaque-abstract)))))




(struct parsed-article-title
  ;; nb NOT a parsed-thing
  ([jumble : Normalized-Jumble]
   [prefab : article-title]))



(: make-parsed-article-title (-> parsed-phrasing-content
                                 (Listof parsed-phrasing-content)
                                 parsed-article-title))
(define (make-parsed-article-title main subs)
  (define-values [subs-jumble subs-forests]
    (for/lists ([subs-jumble : (Listof String)]
                [subs-forests : (Listof Html-Forest)])
               ([phrase (in-list subs)])
      (values (parsed-thing-normalized-string phrase)
              (parsed-phrasing-content-forest phrase))))
  (parsed-article-title
   (cons (parsed-thing-normalized-string main)
         subs-jumble)
   (article-title (parsed-phrasing-content-forest main)
                  subs-forests)))
  
;                             
;                             
;   ;;                   ;;   
;   ;;                   ;;   
;   ;;;;    ;;;    ;;;   ;;  ;
;   ;;  ;  ;   ;  ;   ;  ;;  ;
;   ;;  ;  ;   ;  ;   ;  ;; ; 
;   ;;  ;;;;   ;;;;   ;; ;;;; 
;   ;;  ;  ;   ;  ;   ;  ;;  ;
;   ;;  ;  ;   ;  ;   ;  ;;  ;
;   ; ;;    ;;;    ;;;   ;;   
;                             

(struct parsed-book
  ;; n.b. NOT a parsed-thing
  ([whole : whole-book-meta]
   [parts : (Listof parsed-book-part)]))

(: make-parsed-book (-> parsed-whole-book-meta
                        (Listof parsed-book-part)
                        parsed-book))
(define (make-parsed-book whole parts)
  (parsed-book (parsed-whole-book-meta-prefab whole)
               parts))

(struct parsed-whole-book-meta
  ;; n.b. NOT a parsed-thing
  ([prefab : whole-book-meta]))



(define (make-parsed-whole-book-meta #:main-title [main-title : parsed-phrasing-content]
                                     #:subtitles [subtitles : (Listof parsed-phrasing-content)]
                                     #:contribs [contribs : (Listof parsed-contrib)]
                                     #:publishers [publishers : (Listof Book-Publisher-Spec)]
                                     #:pub-dates [pub-dates : (Listof parsed-pub-date)]
                                     #:self-uri [self-uri : String])
  : parsed-whole-book-meta
  (parsed-whole-book-meta
   (whole-book-meta
    (parsed-phrasing-content-forest main-title)
    (map parsed-phrasing-content-forest subtitles)
    (map parsed-contrib-prefab contribs)
    (map (λ ([pr : Book-Publisher-Spec])
           (define loc (cdr pr))
           (book-publisher (parsed-phrasing-content-forest (car pr))
                           (and loc (parsed-phrasing-content-forest loc))))
         publishers)
    (map parsed-pub-date-prefab pub-dates)
    self-uri)))

(define-type Book-Publisher-Spec
  (Pairof parsed-phrasing-content
          (U #f parsed-phrasing-content)))



(struct parsed-book-part parsed-thing
  ([prefab : book-part]))



(define (make-parsed-book-part #:title [title-spec : (U #f (List (U #f parsed-phrasing-content)
                                                                 (U #f parsed-phrasing-content)
                                                                 (Listof parsed-phrasing-content)))]
                               #:contribs [contribs : (Listof parsed-contrib)]
                               #:page-spec [page-spec : (U #f String (Pairof String String))]
                               #:jstor-id [jstor-id : (U #f String)]
                               #:abstract [abstract : (U #f parsed-abstract)])
  : parsed-book-part
  (define-values [title-jumble structured-title]
    (if title-spec
        (let ([lbl (car title-spec)]
              [main (cadr title-spec)]
              [subs (caddr title-spec)])
          (values (list (and lbl (parsed-thing-normalized-string lbl))
                        (and main (parsed-thing-normalized-string main))
                        (map parsed-thing-normalized-string subs))
                  (book-part-title
                   (and lbl (parsed-phrasing-content-forest lbl))
                   (and main (parsed-phrasing-content-forest main))
                   (map parsed-phrasing-content-forest subs))))
        (values #f #f)))
  (define normalized-string
    (normalized-jumble->string
     (cons title-jumble
           (and abstract (parsed-thing-normalized-string abstract)))))
  (parsed-book-part
   normalized-string
   (book-part structured-title
              (map parsed-contrib-prefab contribs)
              page-spec
              jstor-id
              (and abstract (parsed-abstract-prefab abstract)))))

;                                         
;                       ;;       ;; ;;    
;                       ;;          ;;    
;    ;;;   ;;;   ; ;;; ;;;;;;; ; ;; ;;;;  
;   ;   ; ;   ;  ;;  ;  ;;  ;;;  ;; ;;  ; 
;   ;     ;   ;  ;;  ;; ;;  ;;   ;; ;;  ; 
;  ;;    ;;   ;; ;;  ;; ;;  ;;   ;; ;;  ;;
;   ;     ;   ;  ;;  ;; ;;  ;;   ;; ;;  ; 
;   ;   ; ;   ;  ;;  ;;  ;  ;;   ;; ;;  ; 
;    ;;;   ;;;   ;;  ;;  ;;;;;   ;; ; ;;  
;                                         

(struct parsed-contrib
  ;; nb NOT a parsed-thing
  ([prefab : Contributor]))

(: make-collab-contrib (-> parsed-phrasing-content
                           #:contribution-type (U #f Symbol)
                           parsed-contrib))
(define (make-collab-contrib #:contribution-type type
                             phrase)
  (parsed-contrib
   (group-contributor type
                      (phrase->styled-name-part phrase))))

(: make-unstructured-contrib (-> String
                                 #:contribution-type (U #f Symbol)
                                 parsed-contrib))
(define (make-unstructured-contrib #:contribution-type type
                                   str)
  (parsed-contrib
   (unstructured-contributor type str)))

(: make-surname-contrib (-> #:contribution-type (U #f Symbol)
                            #:prefix (U #f parsed-phrasing-content)
                            #:suffix (U #f parsed-phrasing-content)
                            #:style (U 'western 'eastern 'islensk)
                            #:surname parsed-phrasing-content
                            #:given-names (U #f parsed-phrasing-content)
                            parsed-contrib))
(define (make-surname-contrib #:contribution-type type
                              #:prefix prefix
                              #:suffix suffix
                              #:style style
                              #:surname surname
                              #:given-names given-names)
  (parsed-contrib
   (surname-contributor type
                        (and prefix (phrase->styled-name-part prefix))
                        (and suffix (phrase->styled-name-part suffix))
                        style
                        (phrase->styled-name-part surname)
                        (and given-names (phrase->styled-name-part given-names)))))

(: make-given-only-contrib (-> #:contribution-type (U #f Symbol)
                               #:prefix (U #f parsed-phrasing-content)
                               #:suffix (U #f parsed-phrasing-content)
                               #:given-names parsed-phrasing-content
                               parsed-contrib))
(define (make-given-only-contrib #:contribution-type type
                                 #:prefix prefix
                                 #:suffix suffix
                                 #:given-names given-names)
  (parsed-contrib
   (given-only-contributor type
                           (and prefix (phrase->styled-name-part prefix))
                           (and suffix (phrase->styled-name-part suffix))
                           (phrase->styled-name-part given-names))))
                         
     



(: phrase->styled-name-part (-> parsed-phrasing-content styled-name-part))
(define (phrase->styled-name-part phrase)
  (styled-name-part (parsed-thing-normalized-string phrase)
                    (parsed-phrasing-content-forest phrase)))



;                                                   
;                                                   
;                 ;;             ;;        ;;       
;                 ;;             ;;        ;;       
;   ; ;;   ;; ;;  ;;;;        ;;;;;  ;;   ;;;;; ;;  
;   ;;  ;  ;; ;;  ;;  ;      ;   ;; ;  ;   ;;  ;  ; 
;   ;;  ;  ;; ;;  ;;  ;      ;   ;;    ;;  ;;  ;  ; 
;   ;;  ;; ;; ;;  ;;  ;; ;;;;;   ;;  ;;;;  ;; ;;;;;;
;   ;;  ;  ;; ;;  ;;  ;      ;   ;; ;  ;;  ;;  ;    
;   ;;  ;   ; ;;  ;;  ;      ;   ;;;;  ;;   ;  ;    
;   ;;;;    ;;;;  ; ;;        ;;; ; ;;; ;   ;;; ;;; 
;   ;;                                              
;   ;;                                              
;   ;;                                              
;                                                   

(struct parsed-pub-date
  ;; nb NOT a parsed-thing
  ([prefab : publication-date]))



(: make-parsed-pub-date (-> #:publication-format (U #f Symbol)
                            #:publication-event (U #f Symbol)
                            #:ymd YMD
                            #:season (U #f String)
                            #:string-date (U #f String)
                            parsed-pub-date))
(define (make-parsed-pub-date #:publication-format fmt
                              #:publication-event evt
                              #:ymd ymd
                              #:season season
                              #:string-date string-date)
  (parsed-pub-date
   (publication-date fmt evt ymd season string-date)))



;                                               
;                                               
;          ;;          ;;                    ;; 
;          ;;          ;;                    ;; 
;    ;;    ;;;;    ;; ;;;;;;; ;  ;;     ;;; ;;;;
;   ;  ;   ;;  ; ;;  ; ;;  ;;;  ;  ;   ;   ; ;; 
;      ;;  ;;  ;  ;    ;;  ;;      ;;  ;     ;; 
;    ;;;;  ;;  ;;  ;;  ;;  ;;    ;;;; ;;     ;; 
;   ;  ;;  ;;  ;     ;;;;  ;;   ;  ;;  ;     ;; 
;  ;;  ;;  ;;  ; ;   ;  ;  ;;  ;;  ;;  ;   ;  ; 
;   ;;; ;  ; ;;   ;;;   ;;;;;   ;;; ;   ;;;   ;;
;                                               

(struct parsed-abstract parsed-thing
  ([prefab : abstract]))



(: make-parsed-abstract (-> #:normalized-string String
                            #:label (U #f Html-Forest)
                            #:title (U #f Html-Forest)
                            #:body Html-Forest
                            parsed-abstract))
(define (make-parsed-abstract #:normalized-string str
                              #:label label
                              #:title title
                              #:body body)
  (parsed-abstract str (abstract label title body)))

;                                            
;                                         ;; 
;                                         ;; 
;    ;; ;;; ;;  ; ;;   ; ;;    ;;;   ;; ;;;;;
;  ;;  ; ;; ;;  ;;  ;  ;;  ;  ;   ;  ;;;  ;; 
;   ;    ;; ;;  ;;  ;  ;;  ;  ;   ;  ;;   ;; 
;    ;;  ;; ;;  ;;  ;; ;;  ;;;;   ;; ;;   ;; 
;      ;;;; ;;  ;;  ;  ;;  ;  ;   ;  ;;   ;; 
;  ;   ;  ; ;;  ;;  ;  ;;  ;  ;   ;  ;;    ; 
;   ;;;   ;;;;  ;;;;   ;;;;    ;;;   ;;    ;;
;               ;;     ;;                    
;               ;;     ;;                    
;               ;;     ;;                    
;                                            


(struct parsed-phrasing-content parsed-thing
  ([forest : Html-Forest])
  #:extra-constructor-name make-parsed-phrasing-content)

(: whitespace-string? (-> Any Boolean : #:+ String))
(define (whitespace-string? v)
  (and (string? v)
       (regexp-match? #px"^\\s*$" v)))


