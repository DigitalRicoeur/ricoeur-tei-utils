#lang racket

(require ricoeur/tei/oop-kernel 
         data/maybe
         gregor
         adjutor
         "element.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     ))

(provide define/TEI-info
         TEI-info<%>
         ;; Try to avoid these:
         can-have-id<%>
         get-title<%>
         get-publication-date<%>
         get-citation<%>
         classification<%>
         tei-guess-paragraphs-status<%>
         ;;;;
         get-title-mixin
         get-citation-mixin
         classification-mixin
         )



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

(define classification<%>
  (interface ()
    [get-book/article (->m (or/c 'book 'article))]))

;(define-local-member-name update-guess-paragraphs-status)

(define tei-guess-paragraphs-status<%>
  (interface ()
    [get-guess-paragraphs-status
     (->m (or/c 'todo
                'line-breaks
                'blank-lines
                'done
                'skip))]))

(define TEI-info<%>
  (interface {get-title<%>
              get-citation<%>
              classification<%>
              tei-guess-paragraphs-status<%>}
    [get-modify-seconds (->m (or/c #f exact-integer?))]
    [get-full-path (->m (or/c #f (and/c path-string? absolute-path?)))]
    [get-filename (->m (or/c #f string?))]))

;                                                  
;                                                  
;                                                  
;                                                  
;              ;               ;                   
;              ;;              ;;                  
;  ; ;; ;;  ;;;;;  ;;   ;;  ;;;;;   ;; ;      ;;   
;  ;; ;; ;     ;;    ;  ;      ;;   ;;; ;   ;;  ;  
;  ;; ;; ;;    ;;    ; ;       ;;   ;;  ;;   ;     
;  ;; ;; ;;    ;;     ;        ;;   ;;  ;;    ;;   
;  ;; ;; ;;    ;;    ; ;       ;;   ;;  ;;      ;; 
;  ;; ;; ;;    ;;   ;   ;      ;;   ;;  ;;  ;   ;  
;  ;; ;; ;;    ;;  ;;   ;;     ;;   ;;  ;;   ;;;   
;                                                  
;                                                  
;                                                  
;                                                  

(define get-title-mixin
  (let ([get-title? (is-a?/c get-title<%>)])
    (mixin {element<%>} {get-title<%>}
      (super-new)
      (inherit get-body)
      (define target
        (findf get-title? (get-body)))
      (define/public (get-resp-string resp)
        (send target get-resp-string resp))
      (define/public (get-title)
        (send target get-title)))))

(define get-citation-mixin
  (let ([get-citation? (is-a?/c get-citation<%>)])
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
      (send target get-publication-date)))))

(define classification-mixin
  (let ([classification? (is-a?/c classification<%>)])
  (mixin {element<%>} {classification<%>}
    (super-new)
    (inherit get-body)
    (define target
      (findf classification? (get-body)))
    (define/public (get-book/article)
      (send target get-book/article)))))

;                                          
;                                          
;                                          
;                                          
;                                          
;                                          
;  ; ;; ;;    ;;       ;;;  ;; ;;;   ;;;   
;  ;; ;; ;   ;  ;    ;;   ; ;;;     ;   ;  
;  ;; ;; ;;     ;;   ;      ;;      ;   ;  
;  ;; ;; ;;   ;;;;  ;;      ;;     ;;   ;; 
;  ;; ;; ;;  ;  ;;   ;      ;;      ;   ;  
;  ;; ;; ;; ;;  ;;   ;;   ; ;;      ;   ;  
;  ;; ;; ;;  ;;; ;     ;;;  ;;       ;;;   
;                                          
;                                          
;                                          
;                                          

(define-syntax defgenerics
  (syntax-parser
    [(_ if:expr name:id)
     #:with gen-name (format-id #'name "gen:~a" #'name #:source #'name)
     #`(define gen-name (generic if name))]
    [(_ if:expr name:id more:id ...+)
     #'(begin (defgenerics if name)
              (defgenerics if more ...))]))

(defgenerics
 TEI-info<%>
 get-title
 get-resp-string ;1 arg
 get-publication-date
 get-original-publication-date
 get-citation
 get-book/article
 get-filename
 get-full-path
 get-modify-seconds
 get-guess-paragraphs-status
 )

(define-for-syntax (make-methods-syntaxes target-stx context-stx)
  (with-syntax ([name target-stx]
                [(get-title
                  get-resp-string ;1 arg
                  get-publication-date
                  get-original-publication-date
                  get-citation
                  get-book/article
                  get-guess-paragraphs-status
                  get-full-path
                  get-modify-seconds
                  get-filename)
                 (map (λ (it) (datum->syntax context-stx it))
                      '(get-title
                        get-resp-string ;1 arg
                        get-publication-date
                        get-original-publication-date
                        get-citation
                        get-book/article
                        get-guess-paragraphs-status
                        get-full-path
                        get-modify-seconds
                        get-filename))])
    (syntax-e
     #'((define/public (get-title)
          (send-generic name gen:get-title))
        (define/public (get-resp-string arg)
          (send-generic name gen:get-resp-string arg))
        (define/public (get-publication-date)
          (send-generic name gen:get-publication-date))
        (define/public (get-original-publication-date)
          (send-generic name gen:get-original-publication-date))
        (define/public (get-citation)
          (send-generic name gen:get-citation))
        (define/public (get-guess-paragraphs-status)
          (send-generic name gen:get-guess-paragraphs-status))
        (define/public (get-book/article)
          (send-generic name gen:get-book/article))
        (define/public (get-full-path)
          (send-generic name gen:get-full-path))
        (define/public (get-modify-seconds)
          (send-generic name gen:get-modify-seconds))
        (define/public (get-filename)
          (send-generic name gen:get-filename))))))

(define-syntax define/TEI-info 
  (syntax-parser
    [(_ name:id val-expr)
     #:declare val-expr (expr/c #'(is-a?/c TEI-info<%>)
                                #:name "TEI-info<%> expression")
     #`(begin
         (define real-name val-expr.c)
         (define-syntax name
           (make-set!-transformer
            (syntax-parser
              #:literals {set!}
              [(set! _ v)
               #:declare v (expr/c #'(is-a?/c TEI-info<%>)
                                   #:name "TEI-info<%> expression")
               #'(set! real-name v.c)]
              [_:id
               #'real-name])))
         #,@(make-methods-syntaxes #'real-name #'name))]
    [(_ val-expr)
     #:declare val-expr (expr/c #'(is-a?/c TEI-info<%>)
                                #:name "TEI-info<%> expression")
     #:with (name) (generate-temporaries '(TEI-info))
     #`(begin (define name val-expr.c)
              #,@(make-methods-syntaxes #'name #'val-expr))]
    [(_ #:each-time val-expr)
     #:declare val-expr (expr/c #'(is-a?/c TEI-info<%>)
                                #:name "TEI-info<%> expression")
     #`(begin #,@(make-methods-syntaxes #'val-expr.c #'val-expr))]))

