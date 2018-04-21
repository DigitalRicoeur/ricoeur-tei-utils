#lang racket

(require adjutor
         data/maybe
         json
         ricoeur/tei/oop/objects
         ricoeur/tei/oop/interfaces
         ricoeur/tei/xexpr/normalize
         "location-stack.rkt"
         (submod "location-stack.rkt" private)
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide pre-segment-meta/c
         (contract-out
          [prepare-pre-segments
           (-> (is-a?/c TEI<%>) (listof pre-segment?))]
          [struct pre-segment ([title string?]
                               [counter natural-number/c]
                               [body string?]
                               [meta pre-segment-meta/c]
                               [resp #rx"^#.+"])
            #:omit-constructor]
          ))

(struct pre-segment (title counter body meta resp))

(define current-title
  (make-parameter #f))

(define current-resp
  (make-parameter "#ricoeur"))

(define page-spec/c
  (or/c (maybe/c string?)
        (list/c (maybe/c string?) (maybe/c string?))))

(define-syntax (eprintf* stx)
  #'(begin))

(define pre-segment-meta/c
  (opt/c
   (and/c jsexpr?
          (let ([page/c (or/c #f
                              string?
                              (list/c (or/c #f string?)
                                      (or/c #f string?)))])
            (hash/dc [k (or/c 'resp 'location-stack 'page)]
                     [v (k) (case k
                              [(resp) #rx"#.+"]
                              [(location-stack) location-stack-jsexpr/c]
                              [(page) page/c]
                              [else none/c])]
                     #:immutable #t
                     #:kind 'flat))
          (λ (hsh)
            (hash-keys-subset? #hasheq([resp . #t]
                                       [location-stack . #t]
                                       [page . #t])
                               hsh)))))

(define/check-args/contract (cons-pre-segment body
                                              #:segs segs 
                                              #:i counter
                                              #:page page)
  (-> string?
      #:segs (listof pre-segment?)
      #:i natural-number/c
      #:page page-spec/c
      (listof pre-segment?))
  (cond
    [(regexp-match? #px"^\\s*$"  body)
     segs]
    [else
     (cons (let ([resp (current-resp)])
             (pre-segment (current-title)
                          counter
                          body
                          (hasheq 'resp resp
                                  'location-stack (location-stack->jsexpr
                                                   (current-location-stack))
                                  'page (match page
                                          [(list a b)
                                           (list (from-just #f a)
                                                 (from-just #f b))]
                                          [it
                                           (from-just #f it)]))
                          resp))
           segs)]))
                       


;                                                          
;                                                          
;                                                          
;                                                          
;                                                          
;                                                          
;  ;      ; ;; ;;;    ;;    ; ;;      ;;;   ;; ;;;    ;;   
;  ;   ;  ; ;;;      ;  ;   ;;  ;   ;;   ;  ;;;     ;;  ;  
;   ; ;; ;  ;;          ;;  ;;  ;   ;    ;  ;;       ;     
;   ; ;; ;  ;;        ;;;;  ;;  ;; ;;;;;;;; ;;        ;;   
;   ; ; ;;  ;;       ;  ;;  ;;  ;   ;       ;;          ;; 
;   ;;  ;;  ;;      ;;  ;;  ;;  ;   ;;   ;  ;;      ;   ;  
;    ;  ;   ;;       ;;; ;  ;;;;      ;;;   ;;       ;;;   
;                           ;;                             
;                           ;;                             
;                           ;;                             
;                                                          


(define (prepare-pre-segments doc)
  (parameterize ([current-title (send doc get-title)])
    (for/fold/define ([segs null]
                      [i 0]
                      [pb (tag->element '(pb))])
                     ([child (in-list
                              (send (last (send doc get-body/elements-only))
                                    get-body/elements-only))])
      (do-prepare-segments child #:segs segs #:i i #:pb pb))
    segs))

(define (get-updated-resp child)
  ; maybe method ?
  (or (car (dict-ref (send child get-attributes) 'resp '(#f)))
      (and (eq? 'sp (send child get-name))
           (car (dict-ref (send child get-attributes) 'who)))
      (current-resp)))

(define (get-updated-location-stack child)
  ;; TODO
  ;; This has nothing to do with whether prepare-pre-segments diverges overall.
  'body
  #;
  (match (send child get-name)
    [(and sym (or 'front 'body 'back))
     (when (current-location-stack)
       (error 'get-updated-location-stack ;; TODO: better message
              "can't add root element to non-empty location stack"))
     sym]
    ['div
     ; should I add methods to div% or
     ; rely on knowing about its attributes?
     #|
    (define/augment-final (to-pre-segments/add-metadata pred
                                                        call-with-metadata
                                                        thunk)
      (call-with-metadata #:location (list 'div type n) thunk))
|#
     (error 'TODO)
     (location-stack-entry:div '|TODO: type|
                               '|TODO: n|
                               (current-location-stack))]
    ['note
     ; should probably add methods to note
     #|
(define/augment-final (to-pre-segments/add-metadata pred
                                                        call-with-metadata
                                                        thunk)
      (let ([n (car (dict-ref (get-attributes) 'n))]
            [place (car (dict-ref (get-attributes) 'place))]
            [transl (car (dict-ref (get-attributes) 'transl '(#f)))])
        (call-with-metadata #:location (list 'note place n transl)
                            thunk)))))|#
     (error 'TODO)
     (location-stack-entry:note '|TODO: place|
                                '|TODO: n|
                                '|TODO: transl?|
                                (current-location-stack))]
    [_
     (current-location-stack)]))


(define-syntax (kw-loop stx)
  (define-splicing-syntax-class clause
    (pattern (~seq kw:keyword [name:id init:expr])))
  (syntax-parse stx
    [(_ loop:id (cl:clause ...) body ...+)
     (define (unwrap stx)
       (apply append (map syntax-e (syntax-e stx))))
     #`(let ()
         (define/check-args (loop #,@(unwrap #'([cl.kw cl.name] ...)))
           body ...)
         (loop #,@(unwrap #'([cl.kw cl.init] ...))))]))


(define (init-pb+latest-pb->page init-pb latest-pb)
  (if (equal? init-pb latest-pb)
      (send init-pb get-page-string)
      (list (send init-pb get-page-string)
            (send latest-pb get-page-string))))

(define (not-body-element? v)
  (or (not (tei-element? v))
      (eq? 'pb (send v get-name))))

;                                  
;                                  
;                                  
;                                  
;                                  
;                                  
;      ;;;   ;;;    ;; ;;;    ;;;  
;    ;;   ; ;   ;   ;;;     ;;   ; 
;    ;      ;   ;   ;;      ;    ; 
;   ;;     ;;   ;;  ;;     ;;;;;;;;
;    ;      ;   ;   ;;      ;      
;    ;;   ; ;   ;   ;;      ;;   ; 
;      ;;;   ;;;    ;;        ;;;  
;                                  
;                                  
;                                  
;                                  


(define/contract (do-prepare-segments child
                                      #:segs [segs null]
                                      #:i [i 0]
                                      #:pb [pb (tag->element '(pb))])
  (->* {tei-element?}
       {#:segs (listof pre-segment?)
        #:i natural-number/c
        #:pb (is-a?/c pb<%>)}
       (values (listof pre-segment?)
               natural-number/c
               (is-a?/c pb<%>)))
  (eprintf* "do-prepare-segments\n")
  (parameterize ([current-resp (get-updated-resp child)]
                 [current-location-stack (get-updated-location-stack child)])
    (define name
      (send child get-name))
    (case name
      [(ab p head note item)
       (eprintf* "\t~v\n" '(ab p head note item))
       (do-prepare-segments/content child #:segs segs #:i i #:pb pb)]
      [(pb)
       (eprintf* "\tpb\n")
       (values segs i child)]
      [else
       (case (and (eq? 'div name) (dict-ref (send child get-attributes) 'type #f))
         [(("contents")("index"))
          (eprintf* "\tignored div\n")
          (values segs i (or (for/last ([pb (in-list (send child get-page-breaks))])
                               pb)
                             pb))]
         [else
          (eprintf* "\telse case\n")
          (for/fold ([segs segs]
                     [i i]
                     [pb pb])
                    ([child (in-list (send child get-body/elements-only))])
            (do-prepare-segments child #:segs segs #:i i #:pb pb))])])))







(define/contract (do-prepare-segments/content child
                                              #:segs [segs null]
                                              #:i [i 0]
                                              #:pb [init-pb (tag->element '(pb))])
  (->* {tei-element?}
       {#:segs (listof pre-segment?)
        #:i natural-number/c
        #:pb (is-a?/c pb<%>)}
       (values (listof pre-segment?)
               natural-number/c
               (is-a?/c pb<%>)))
  (eprintf* "do-prepare-segments/content\n")
  (define name
    (send child get-name))
  (kw-loop prepare-content (#:to-go [to-go (send child get-body)]
                            #:this-so-far [this-so-far null] 
                            #:segs [segs segs]
                            #:i [i i]
                            #:init-pb [init-pb init-pb]
                            #:latest-pb [latest-pb init-pb])
    (eprintf* "prepare-content\n")
    (define (this-so-far->segs)
      (cons-pre-segment (string-normalize-spaces
                         (string-join (reverse this-so-far) " "))
                        #:segs segs 
                        #:i i
                        #:page (init-pb+latest-pb->page init-pb latest-pb)))
    (match to-go
      ;; base case: finish up, accumulating the last round from this-so-far
      ['()
       (eprintf* "\tbase case\n")
       (values (this-so-far->segs)
               (add1 i)
               latest-pb)]
      ;; accumulate from this-so-far, dispatch to child element,
      ;; then continue with new acc, i, and init-pb from child
      [(cons (? tei-element? child) to-go)
       #:when (not (eq? 'pb (send child get-name)))
       (eprintf* "\telement (not pb)\n")
       (define-values {new-segs new-i new-pb}
         (do-prepare-segments child
                              #:segs (this-so-far->segs)
                              #:i (add1 i)
                              #:pb latest-pb))
       (prepare-content #:to-go to-go
                        #:this-so-far null
                        #:segs new-segs
                        #:i new-i
                        #:init-pb new-pb
                        #:latest-pb new-pb)]
      ;; on a pb child of a non-ab,
      ;; continue with new latest-pb
      [(cons (? pb? latest-pb) to-go)
       #:when (not (eq? 'ab name))
       (eprintf* "\tpb (not in ab)\n")
       (prepare-content #:to-go to-go
                        #:this-so-far this-so-far
                        #:segs segs
                        #:i i
                        #:init-pb init-pb
                        #:latest-pb latest-pb)]
      ;; on other child of a non-ab,
      ;; continue, consing this child on to this-so-far
      [(cons str-or-misc to-go)
       #:when (not (eq? 'ab name))
       (eprintf* "\tother child (not in ab)\n")
       (prepare-content #:to-go to-go
                        #:this-so-far (cons (element-or-xexpr->plain-text str-or-misc)
                                            this-so-far)
                        #:segs segs
                        #:i i
                        #:init-pb init-pb
                        #:latest-pb latest-pb)]
      ;; in an ab,
      ;; This case handles a bunch of things that are not child TEI-body<%>
      ;; elements, i.e. strings, pb%s, and comments etc, as plain-children.
      [(list-rest (? not-body-element? plain-children) ... to-go)
       (eprintf* "\tin an ab\n")
       ;; We start by counting how many pb%s we see here and keeping track
       ;; of the last (which may be init-pb).
       (for/fold/define ([num-pbs 0]
                         [latest-pb init-pb])
                        ([child (in-list plain-children)]
                         #:when (pb? child))
         (values (add1 num-pbs) child))
       (define-values {new-segs new-i new-pb}
         (cond
           ;; Good case: there are fewer than two pb%s in plain-children,
           ;; so we treat plain-children as a segment.
           [(infix: 2 > num-pbs)
            (eprintf* "\t\tgood case\n")
            (values (cons-pre-segment
                     (string-normalize-spaces
                      (string-join
                       (for/list ([child (in-list plain-children)]
                                  #:unless (pb? child))
                         (element-or-xexpr->plain-text child))
                       " "))
                     #:segs segs
                     #:i i
                     #:page (init-pb+latest-pb->page init-pb latest-pb))
                    (add1 i)
                    latest-pb)]
           ;; Ugly case: this handles massive ab%s that have not been segmented.
           ;; In this case, each page is used as a segment.
           [else
            (eprintf* "\t\tugly case\n")
            (prepare-content/ugly-ab plain-children
                                     #:segs segs
                                     #:i i
                                     #:pb init-pb)]))
       (prepare-content #:to-go to-go
                        #:this-so-far null
                        #:segs new-segs
                        #:i new-i
                        #:init-pb new-pb
                        #:latest-pb new-pb)])))










(define/contract (prepare-content/ugly-ab to-go ;plain-children
                                          #:segs segs
                                          #:i i
                                          #:pb pb)
  (-> (listof not-body-element?)
      #:segs (listof pre-segment?)
      #:i natural-number/c
      #:pb (is-a?/c pb<%>)
      (values (listof pre-segment?)
              natural-number/c
              (is-a?/c pb<%>)))
  (eprintf* "prepare-content/ugly-ab\n")
  ;; Ugly case: this handles massive ab%s that have not been segmented.
  ;; In this case, each page is used as a segment.
  (kw-loop loop (#:to-go [to-go to-go]
                 #:this-so-far [this-so-far null]
                 #:segs [segs segs]
                 #:pb [pb pb]
                 #:i [i i])
    (eprintf* "loop: (length to-go) = ~v\n" (length to-go))
    (define (this-so-far->segs)
      (cons-pre-segment
       (string-normalize-spaces
        (string-join (reverse this-so-far) " "))
       #:segs segs 
       #:i i
       #:page (send pb get-page-string)))
    (match to-go
      ['()
       (eprintf* "\t'()\n")
       (values (this-so-far->segs)
               (add1 i)
               pb)]
      [(cons (? pb? new-pb) to-go)
       (eprintf* "\tpb\n")
       (loop #:to-go to-go
             #:i (add1 i)
             #:pb new-pb
             #:this-so-far null
             #:segs (this-so-far->segs))]
      [(cons child to-go)
       (eprintf* "\tother child\n")
       (loop #:to-go to-go
             #:this-so-far (cons (element-or-xexpr->plain-text child)
                                 this-so-far)
             #:segs segs
             #:pb pb
             #:i i)])))







#|
(define doc
  (file->TEI
   (build-path
    "/Users/philip/code/ricoeur/texts/debug/"
    "broken_philosophy_Ricoeur_anthology_of_work.xml")))

(prepare-pre-segments doc)
|#





