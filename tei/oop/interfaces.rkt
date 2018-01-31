#lang racket

(require xml 
         ricoeur/tei/xexpr/normalize
         ricoeur/tei/xexpr/tei-xexpr-contracts
         data/maybe
         gregor
         adjutor
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     ))

(require-provide "interfaces/element.rkt"
                 "interfaces/elements-only.rkt"
                 "interfaces/info-interfaces.rkt"
                 "interfaces/pb.rkt"
                 "interfaces/guess-paragraphs.rkt"
                 )

(provide teiHeader<%> 
         TEI<%>
         )                       

(define teiHeader<%>
  (interface (TEI-info<%> element<%>)))

(define TEI<%>
  (interface (element<%>
              TEI-info<%>
              get-page-breaks<%>
              guess-paragraphs<%>
              elements-only<%>)
    [get-md5 (->m string?)]
    [get-teiHeader (->m (is-a?/c teiHeader<%>))]
    [write-TEI (->*m {} {output-port?} any)]
    #|[do-prepare-pre-segments
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
             pre-segment-accumulator?])]|#
    ))

;                                                  
;                                                  
;                                                  
;                                                  
;                                           ;;     
;                                           ;;     
;     ;;      ;;;     ;;    ;; ;;;     ;;;  ;; ;   
;   ;;  ;   ;;   ;   ;  ;   ;;;      ;;   ; ;;; ;  
;    ;      ;    ;      ;;  ;;       ;      ;;  ;; 
;     ;;   ;;;;;;;;   ;;;;  ;;      ;;      ;;  ;; 
;       ;;  ;        ;  ;;  ;;       ;      ;;  ;; 
;   ;   ;   ;;   ;  ;;  ;;  ;;       ;;   ; ;;  ;; 
;    ;;;      ;;;    ;;; ;  ;;         ;;;  ;;  ;; 
;                                                  
;                                                  
;                                                  
;                                                  

#|
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
|#


#;
(define body-element?
  (is-a?/c TEI-body<%>))
#;
(define not-body-element?
  (not/c body-element?))
#;
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
#;
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
#;
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
               [(infix: 2 > num-pbs)
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

#;
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





















