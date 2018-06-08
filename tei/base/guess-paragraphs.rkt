#lang racket

(require ricoeur/tei/kernel
         ricoeur/tei/base/def-from-spec
         syntax/parse/define
         )

(provide (contract-out
          [tei-document-skip-guess-paragraphs
           (update-guess-paragraphs-proc/c 'todo 'skip)]
          [tei-document-unskip-guess-paragraphs
           (update-guess-paragraphs-proc/c 'skip 'todo)]
          [tei-document-guess-paragraphs
           (->i #:chaperone
                ([doc (document/paragraphs-status/c
                       (or/c 'todo 'skip))])
                (#:mode [mode (or/c 'line-breaks 'blank-lines)])
                [_ (mode)
                   (document/paragraphs-status/c
                    (case mode
                      [(line-breaks) 'line-breaks]
                      [else 'blank-lines]))])]
          ))
           
(module+ test
  (require rackunit
           (submod "..")))

(define/final-prop (document/paragraphs-status/c status/c)
  (and/c tei-document?
         (has-tei-document-paragraphs-status/c status/c)))

(define (update-guess-paragraphs-proc/c from/c to/c)
  (-> (document/paragraphs-status/c from/c)
      (document/paragraphs-status/c to/c)))

(define (guess-paragraphs-status->term status)
  (case status
    [(todo) '(term () "todo")]
    [(line-breaks) '(term () "line-breaks")]
    [(blank-lines) '(term () "blank-lines")]
    [(done) '(term () "done")]
    [(skip) '(term () "skip")]))


(define/contract (make-document-paragraphs-status-updater
                  update-teiHeader
                  [update-text
                   tei-element->xexpr])
  (->* {(-> teiHeader? (static-tei-xexpr/c teiHeader))}
       {(-> tei-text-element? (static-tei-xexpr/c text))}
       (-> tei-document? tei-document?))
  (λ (e)
    (xexpr->element
     ((make-tei-element-updater/xexpr
       [teiHeader? update-teiHeader]
       [tei-text-element? update-text])
      e))))


(define update-proc/c
  (->i #:chaperone
       {[elem tei-element?]}
       [_ (elem) (tei-xexpr/c
                  (tei-element-get-name elem))]))


(define predicate/c
  (-> any/c any/c))

(define (tei-element-update/xexpr* e update-body)
  ;; This is hopeless to contract b/c there isn't a way
  ;; right now to write a contract for a properly-structured
  ;; body for some element type.
  (match e
    [(tei-element name attrs body)
     (list* name attrs (update-body body))]))

(define-syntax-parser make-tei-element-updater/xexpr
  [(_ [pred updater] ...)
   #:declare pred (expr/c #'predicate/c
                          #:name "predicate expression")
   #:declare updater (expr/c #'update-proc/c
                             #:name "update proc expression")
   #:with (pred* ...) (generate-temporaries
                       (syntax->list #'(pred ...)))
   #:with (updater* ...) (generate-temporaries
                          (syntax->list #'(updater ...)))
   #`(let ([pred* pred.c] ...
           [updater* updater.c] ...)
       (λ (e)
         (tei-element-update/xexpr*
          e
          (λ (body)
            (for/list ([child (in-list body)])
              (cond
                [(pred* child)
                 (updater* child)]
                ...
                [else
                 (tei-element->xexpr* child)]))))))])



(define/contract (tei-element-update-1-child/xexpr e pred updater)
  (->i #:chaperone
       {[elem tei-element?]
        [pred predicate/c]
        [update update-proc/c ]}
       [_ (elem) (tei-xexpr/c
                  (tei-element-get-name elem))])
  (tei-element-update/xexpr*
   e
   (λ (body)
     (let loop ([body body])
       (match body
         ['()
          (error 'tei-element-update-1-child/xexpr
                 "~a\n  predicate: ~e\n  element: ~e"
                 "no child element satisfied the given predicate"
                 pred
                 e)]
         [(cons (? pred (app updater new))
                body)
          (cons new (map tei-element->xexpr* body))]
         [(cons (app tei-element->xexpr* this)
                body)
          (cons this (loop body))])))))





(define/contract ((make-teiHeader-update-par-status status) e)
  (-> guess-paragraphs-status/c
      (-> (and/c teiHeader? (has-tei-document-paragraphs-status/c
                             (or/c 'todo 'skip)))
          (static-tei-xexpr/c teiHeader)))
  (tei-element-update-1-child/xexpr
   e
   profileDesc?
   (λ (e)
     (tei-element-update-1-child/xexpr
      e
      textClass?
      (λ (e)
        (tei-element-update-1-child/xexpr
         e
         tei-keywords?
         (λ (e)
           (tei-element-update-1-child/xexpr
            e
            tei-element? ; there's only the one term
            (λ (e)
              (guess-paragraphs-status->term status))))))))))


(define tei-document-skip-guess-paragraphs
  (make-document-paragraphs-status-updater
   (make-teiHeader-update-par-status 'skip)))


(define tei-document-unskip-guess-paragraphs
  (make-document-paragraphs-status-updater
   (make-teiHeader-update-par-status 'todo)))

(define/contract (make-tei-document-guess-paragraphs mode)
  (->i #:chaperone
       ([mode (or/c 'line-breaks 'blank-lines)])
       [_ (mode)
          (update-guess-paragraphs-proc/c (or/c 'todo 'skip)
                                          mode)])
  (make-document-paragraphs-status-updater
   (make-teiHeader-update-par-status mode)
   (make-text-guess-paragraphs mode)))

(define* (tei-document-guess-paragraphs elem #:mode [mode 'blank-lines])
  #:with [(define do-line-breaks
            (make-tei-document-guess-paragraphs 'line-breaks))
          (define do-blank-lines
            (make-tei-document-guess-paragraphs 'blank-lines))]
  (case mode
    [(line-breaks)
     (do-line-breaks elem)]
    [else
     (do-blank-lines elem)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((make-text-guess-paragraphs mode) elem)
  (other-element-guess-paragraphs elem #:mode mode))

;; tei-element? #:mode (or/c 'line-breaks 'blank-lines)
;;   -> xexpr/c
(define (other-element-guess-paragraphs elem #:mode [mode 'blank-lines])
  (tei-element-update/xexpr*
   elem
   (λ (body)
     (apply append
            (for/list ([child (in-list body)])
              (child-do-guess-paragraphs child #:mode mode))))))

;; (or/c tei-element? normalized-xexpr-atom/c
;; #:mode (or/c 'line-breaks 'blank-lines)
;;   -> (listof xexpr/c)
(define (child-do-guess-paragraphs this #:mode [mode 'blank-lines])
  (cond
    [(tei-ab? this)
     (ab-do-guess-paragraphs this #:mode mode)]
    [(tei-element? this)
     (list (other-element-guess-paragraphs this #:mode mode))]
    [else
     this]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-adjacent-strings lst)
  (define (merge/after-string so-far lst)
    (match lst
      [(cons (? string? this) lst)
       (merge/after-string (string-append so-far this) lst)]
      [lst
       (cons so-far (merge-adjacent-strings lst))]))
  (match lst
    ['() '()]
    [(cons (? string? this) lst)
     (merge/after-string this lst)]
    [(cons this lst)
     (cons this (merge-adjacent-strings lst))]))
             

(struct parbreak ())

(define (insert-parbreaks elem #:mode [mode 'blank-lines])
  (define split-pat
    (if (eq? 'blank-lines mode)
        #px"\n[ \t\f\r]*\n|\r[ \t\f]*\r"
        #rx"\n"))
  (flatten
   (for/list ([child (in-list (merge-adjacent-strings
                               (tei-element-get-body elem)))])
     (cond
       [(string? child)
        (add-between (regexp-split split-pat child)
                     (parbreak))]
       [else
        child]))))

(define (group-by-parbreaks elem #:mode [mode 'blank-lines])
  (let loop ([this-group null]
             [to-go (insert-parbreaks elem #:mode mode)])
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

;; tei-ab? #:mode (or/c 'line-breaks 'blank-lines)
;;   -> (listof xexpr/c)
(define (ab-do-guess-paragraphs elem #:mode [mode 'blank-lines])
  (for/list ([pargroup (in-list (group-by-parbreaks elem #:mode mode))]
             #:unless (null? pargroup))
    (match pargroup
      [(list (? tei-pb? elem))
       (tei-element->xexpr elem)]
      [_
       `(p () ,@(for/list ([child (in-list pargroup)])
                  (if (tei-element? child)
                      (other-element-guess-paragraphs child #:mode mode)
                      child)))])))

