#lang racket

(require ricoeur/tei/kernel
         ricoeur/tei/base/def-from-spec
         syntax/parse/define
         )

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
      (-> (and/c teiHeader? (tei-document-with-paragraphs-status/c
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


(define/contract tei-document-skip-guess-paragraphs
  (-> (and/c tei-document? (tei-document-with-paragraphs-status/c 'todo))
      (and/c tei-document? (tei-document-with-paragraphs-status/c 'skip)))
  (make-document-paragraphs-status-updater
   (make-teiHeader-update-par-status 'skip)))


(define/contract tei-document-unskip-guess-paragraphs
  (-> (and/c tei-document? (tei-document-with-paragraphs-status/c 'skip))
      (and/c tei-document? (tei-document-with-paragraphs-status/c 'todo)))
  (make-document-paragraphs-status-updater
   (make-teiHeader-update-par-status 'todo)))








