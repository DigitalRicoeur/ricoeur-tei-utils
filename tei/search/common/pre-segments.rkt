#lang racket

(require adjutor
         data/maybe
         json
         ricoeur/tei/oop/objects
         ricoeur/tei/oop/interfaces
         "location-stack.rkt"
         (submod "location-stack.rkt" private)
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
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

(define-syntax (eprintf* stx)
  #'(void))

(define-syntax-rule (define/no-contract lhs c body ...)
  (define lhs body ...))

(define-syntax define/?contract
  ;; define/contract, define/no-contract, or define/check-args/contract
  (make-rename-transformer #'define/contract))

;; prepare-pre-segments
;; The outer function which sets everything up for a TEI<%> document
(define (prepare-pre-segments doc)
  (parameterize ([current-title (send doc get-title)])
    (for/fold/define ([segs null]
                      [i 0]
                      [pb (tag->element '(pb))])
                     ([child (in-list
                              ;; the children of the text element,
                              ;; i.e. body, perhaps with front and/or back
                              (send (last (send doc get-body/elements-only))
                                    get-body/elements-only))])
      (do-prepare-segments child #:segs segs #:i i #:pb pb))
    segs))


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

;; The functions under "core" actually implement the segmentation logic.
;; All have a common signature:
;; They accept optional keyword arguments #:segs, #:i, and #:pb
;; that cary the pre-segments accumulated so far, the next counter number, and
;; the page-break element BEFORE the segmentation task currently underway,
;; plus one by-position argument for the thing to be segmented.
;; (The type of this argument is function-specific.)
;; They return three values: the accumulated pre-segments,
;; the next counter number, and the last page-break element encountered.



;; do-prepare-segments
;; This is the entry point for the segmentation of any individual element. 
;; It installs metadata, deals with page-breaks and ignored div types,
;; and manages recursion over the children of non-content-containing elements.
;; For content-containing elements, it delegates to do-prepare-segments/content.
(define/?contract (do-prepare-segments child
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
    (match (send child get-name)
      ;; content-containing elements
      [(or 'ab 'p 'head 'note 'item)
       (eprintf* "\t~v\n" '(ab p head note item))
       (do-prepare-segments/content child #:segs segs #:i i #:pb pb)]
      ;; page breaks
      ['pb
       (eprintf* "\tpb\n")
       (values segs i child)]
      ;; the ignored div types
      ['div
       #:when (case (dict-ref (send child get-attributes) 'type #f)
                [(("contents")("index")) #t]
                [else #f])
       (eprintf* "\tignored div\n")
       (values segs i (or (for/last ([pb (in-list (send child get-page-breaks))])
                            pb)
                          pb))]
      ;; all other elements (non-content-containing)
      [_
       (eprintf* "\telse case\n")
       (for/fold ([segs segs]
                  [i i]
                  [pb pb])
                 ([child (in-list (send child get-body/elements-only))])
         (do-prepare-segments child #:segs segs #:i i #:pb pb))])))



;; do-prepare-segments/content
;; Loops over the bodes of content-containing elements.
;; The by-position arg is the element itself, as there are special cases
;; based on the element name (for ab elements).
;; For child elements (that are not page-breaks), recurs via do-prepare-segments.
;; The contents of massive ab elements that have not been broken into
;; further child elements are handled by prepare-content/ugly-ab.
(define/?contract (do-prepare-segments/content child
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
      ;; on a child element that is not a pb,
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
      [(list-rest (? pb-or-non-element? plain-children) ... to-go)
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



;; prepare-content/ugly-ab
;; Used for massive ab elements that haven't been subdivided into
;; child elements.
;; The by-position argument is a list of page-break elements and
;; string, comments, and other non-element x-expressions.
;; (NO elements other than page-breaks.)
(define/?contract (prepare-content/ugly-ab to-go 
                                           #:segs segs
                                           #:i i
                                           #:pb pb)
  (-> (listof pb-or-non-element?)
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
      ;; base case: finish up
      ['()
       (eprintf* "\t'()\n")
       (values (this-so-far->segs)
               (add1 i)
               pb)]
      ;; page break
      [(cons (? pb? new-pb) to-go)
       (eprintf* "\tpb\n")
       (loop #:to-go to-go
             #:i (add1 i)
             #:pb new-pb
             #:this-so-far null
             #:segs (this-so-far->segs))]
      ;; non-element x-expression
      [(cons child to-go)
       (eprintf* "\tother child\n")
       (loop #:to-go to-go
             #:this-so-far (cons (element-or-xexpr->plain-text child)
                                 this-so-far)
             #:segs segs
             #:pb pb
             #:i i)])))



;                                                          
;                                                          
;                                                          
;                                                          
;   ;;              ;;;;                                   
;   ;;                ;;                                   
;   ;; ;      ;;;     ;;    ; ;;      ;;;   ;; ;;;    ;;   
;   ;;; ;   ;;   ;    ;;    ;;  ;   ;;   ;  ;;;     ;;  ;  
;   ;;  ;;  ;    ;    ;;    ;;  ;   ;    ;  ;;       ;     
;   ;;  ;; ;;;;;;;;   ;;    ;;  ;; ;;;;;;;; ;;        ;;   
;   ;;  ;;  ;         ;;    ;;  ;   ;       ;;          ;; 
;   ;;  ;;  ;;   ;     ;    ;;  ;   ;;   ;  ;;      ;   ;  
;   ;;  ;;    ;;;       ;;  ;;;;      ;;;   ;;       ;;;   
;                           ;;                             
;                           ;;                             
;                           ;;                             
;                                                          

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
     (location-stack-entry:div (error '|TODO: type|)
                               (error '|TODO: n|)
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
     (location-stack-entry:note (error '|TODO: place|)
                                (error '|TODO: n|)
                                (error '|TODO: transl?|)
                                (current-location-stack))]
    [_
     (current-location-stack)]))

(define/?contract (cons-pre-segment body
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

(define (pb-or-non-element? v)
  (or (not (tei-element? v))
      (eq? 'pb (send v get-name))))


(define page-spec/c
  (or/c (maybe/c string?)
        (list/c (maybe/c string?) (maybe/c string?))))

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

(module+ main
  (provide (all-defined-out))
  (define doc
    (file->TEI
     (build-path
      "/Users/philip/code/ricoeur/texts/debug/"
      "broken_philosophy_Ricoeur_anthology_of_work.xml")))

  (prepare-pre-segments doc))





