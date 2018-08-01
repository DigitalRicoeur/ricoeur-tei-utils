#lang racket/base

(require ricoeur/tei/kernel
         racket/contract
         racket/list
         racket/string
         racket/match
         json
         "def-from-spec.rkt"
         "get-page-breaks.rkt"
         (submod "segments/location-stack.rkt"
                 private)
         (submod "segments/meta.rkt"
                 private)
         (for-syntax racket/base
                     syntax/parse
                     ))

(require-provide "segments/location-stack.rkt"
                 "segments/meta.rkt"
                 )

(provide (contract-out
          [tei-document-segments
           (-> tei-document? (listof base-segment?))]
          [struct base-segment
            ([meta segment-meta?]
             [body (and/c string-immutable/c
                          (not/c #px"^\\s*$"))])
            #:omit-constructor]
          ))

(struct base-segment (meta body)
  #:transparent
  #:property prop:segment
  (λ (this)
    (base-segment-meta this)))

(define cache
  (make-weak-hasheq))

(define (tei-document-segments doc)
  (hash-ref cache
            doc
            (λ ()
              (define rslt
                (prepare-segments doc))
              (hash-set! cache doc rslt)
              rslt)))

;; prepare-segments
;; The outer function which sets everything up for a TEI<%> document
(define (prepare-segments doc)
  (parameterize ([current-make-segment-meta
                  (tei-document->make-segment-meta doc)])
    (for/fold/define ([segs null]
                      [i 0]
                      [pb the-default-pb])
                     ([child (in-list
                              ;; the children of the text element,
                              ;; i.e. body, perhaps with front and/or back
                              (tei-get-body/elements-only
                               (tei-document-text-element doc)))])
      (do-prepare-segments child #:segs segs #:i i #:pb pb))
    segs))


(define-syntax (eprintf* stx)
  #'(void))

(define-syntax-rule (define/no-contract lhs c body ...)
  (define lhs body ...))

(define-syntax define/?contract
  ;; define/contract, define/no-contract, or define/check-args/contract
  (make-rename-transformer #'define/contract))

(define (pb-or-non-element? v)
  (or (not (tei-element? v))
      (tei-pb? v)))


(define the-default-pb
  (xexpr->element '(pb)))

(define current-location-stack
  (make-parameter #f))

(define current-make-segment-meta
  (make-parameter '|current-make-segment-meta: not initialized|))

(define current-resp
  (make-parameter 'ricoeur))

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
                                       #:pb [pb the-default-pb])
  (->* {tei-element?}
       {#:segs (listof base-segment?)
        #:i natural-number/c
        #:pb tei-pb?}
       (values (listof base-segment?)
               natural-number/c
               tei-pb?))
  (eprintf* "do-prepare-segments\n")
  (parameterize ([current-resp (get-updated-resp child)]
                 [current-location-stack (get-updated-location-stack child)])
    (match child 
      ;; content-containing elements
      [(? content-containing-element?)
       (eprintf* "\t~v\n" '(ab p head note item))
       (do-prepare-segments/content child #:segs segs #:i i #:pb pb)]
      ;; page breaks
      [(tei-element 'pb _ _)
       (eprintf* "\tpb\n")
       (values segs i child)]
      ;; the ignored div types
      [(tei-element 'div attrs _)
       #:when (case (attributes-ref attrs 'type)
                [("contents" "index") #t]
                [else #f])
       (eprintf* "\tignored div\n")
       (values segs i (or (for/last ([pb (in-list (tei-get-page-breaks child))])
                            pb)
                          pb))]
      ;; all other elements (non-content-containing)
      [_
       (eprintf* "\telse case\n")
       (for/fold ([segs segs]
                  [i i]
                  [pb pb])
                 ([child (in-list (tei-get-body/elements-only child))])
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
                                               #:pb [init-pb the-default-pb])
  (->* {content-containing-element?}
       {#:segs (listof base-segment?)
        #:i natural-number/c
        #:pb tei-pb?}
       (values (listof base-segment?)
               natural-number/c
               tei-pb?))
  (eprintf* "do-prepare-segments/content\n")
  (match-define (content-containing-element name _ to-go)
    child)
  (kw-loop prepare-content (#:to-go [to-go to-go]
                            #:this-so-far [this-so-far null] 
                            #:segs [segs segs]
                            #:i [i i]
                            #:init-pb [init-pb init-pb]
                            #:latest-pb [latest-pb init-pb])
    (eprintf* "prepare-content\n")
    (define (this-so-far->segs)
      (cons-base-segment (string-normalize-spaces
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
       #:when (not (tei-pb? child))
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
      [(cons (? tei-pb? latest-pb) to-go)
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
                         #:when (tei-pb? child))
         (values (add1 num-pbs) child))
       (define-values {new-segs new-i new-pb}
         (cond
           ;; Good case: there are fewer than two pb%s in plain-children,
           ;; so we treat plain-children as a segment.
           [(infix: 2 > num-pbs)
            (eprintf* "\t\tgood case\n")
            (values (cons-base-segment
                     (string-normalize-spaces
                      (string-join
                       (for/list ([child (in-list plain-children)]
                                  #:unless (tei-pb? child))
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
      #:segs (listof base-segment?)
      #:i natural-number/c
      #:pb tei-pb?
      (values (listof base-segment?)
              natural-number/c
              tei-pb?))
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
      (cons-base-segment
       (string-normalize-spaces
        (string-join (reverse this-so-far) " "))
       #:segs segs 
       #:i i
       #:page (pb-get-page-string pb)))
    (match to-go
      ;; base case: finish up
      ['()
       (eprintf* "\t'()\n")
       (values (this-so-far->segs)
               (add1 i)
               pb)]
      ;; page break
      [(cons (? tei-pb? new-pb) to-go)
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
  (or (and (tei-element-can-have-resp? child)
           (tei-element-resp child))
      (current-resp)))

(define (get-updated-location-stack child)
  (match child
    [(tei-element (and sym (or 'front 'body 'back)) _ _)
     (when (current-location-stack)
       (error 'get-updated-location-stack
              "can't add root element to non-empty location stack"))
     (location-stack-entry:root sym)]
    [(? div?)
     (location-stack-entry:div (div-get-type child)
                               (div-get-n child)
                               (current-location-stack))]
    [(? tei-note?) 
     (location-stack-entry:note (tei-note-get-place child)
                                (tei-note-get-n child)
                                (tei-note-get-transl? child)
                                (current-location-stack))]
    [_
     (current-location-stack)]))


(define/?contract (cons-base-segment body*
                                     #:segs segs 
                                     #:i counter
                                     #:page page)
  (-> string?
      #:segs (listof base-segment?)
      #:i natural-number/c
      #:page page-spec/c
      (listof base-segment?))
  (define body
    (string->immutable-string body))
  (cond
    [(regexp-match? #px"^\\s*$"  body)
     segs]
    [else
     (cons (base-segment
            ((current-make-segment-meta)
             #:i counter
             #:page page
             #:resp (current-resp)
             #:location (current-location-stack))
            body)
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
      (pb-get-page-string init-pb)
      (list (pb-get-page-string init-pb)
            (pb-get-page-string latest-pb))))





