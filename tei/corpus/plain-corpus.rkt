#lang racket/base

(require racket/contract
         racket/class
         racket/set
         racket/promise
         ricoeur/tei/base
         )

(provide (contract-out
          [plain-corpus%
           (class/c (init [docs (instance-set/c tei-document?)]))]
          [corpus-get-instance-info-set
           (-> (is-a?/c plain-corpus%)
               (instance-set/c))]
          [corpus-get-checksum-table
           (-> (is-a?/c plain-corpus%)
               (hash/c symbol?
                       symbol?
                       #:immutable #t))]
          [make-corpus-mixin
           (let ([corpus-mixin/c (make-mixin-contract plain-corpus%)]
                 [initialize-this/c (->m (instance-set/c tei-document?)
                                         any)])
             (->i {[k member-name-key?]}
                  [_ (k)
                     (let ()
                       (define-member-name initialize-this k)
                       (and/c corpus-mixin/c
                              (-> (class/c
                                   (absent initialize-this))
                                  (class/c
                                   (override
                                     [initialize-this
                                      initialize-this/c])))))]))]
          ))

(define-member-name on-initialize (generate-member-key))

(define plain-corpus%
  (class* object% {(interface ()
                     [get-instance-info-set
                      (->m (instance-set/c))]
                     [get-checksum-table
                      (->m (hash/c symbol?
                                   symbol?
                                   #:immutable #t))]
                     on-initialize)}
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Initialization:
    (init [docs (instance-set)])
    (define info
      (instance-set
       (set-map (instance-set) get-plain-instance-info)))
    (define pr:checksum-table
      (delay/thread
       (for/hasheq ([doc (in-instance-set docs)])
         (values (instance-title/symbol doc)
                 (tei-document-checksum doc)))))
    ;; Must come at the end of initialization:
    (on-initialize docs)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods:
    (define/pubment (on-initialize docs)
      (inner (void) on-initialize docs))
    (define/public-final (get-instance-info-set)
      info)
    (define/public-final (get-checksum-table)
      (force pr:checksum-table))
    #|END class plain-corpus%|#))



(define (make-corpus-mixin initialize-this-method-key)
  (define-member-name initialize-this initialize-this-method-key)
  (mixin {(class->interface plain-corpus%)} {}
    (super-new)
    (inspect #f)
    (abstract initialize-this)
    (define/augment (on-initialize docs)
      (initialize-this docs)
      (inner (void) on-initialize docs))))


(define gen:get-instance-info-set
  (generic plain-corpus% get-instance-info-set))
(define (corpus-get-instance-info-set c)
  (send-generic c gen:get-instance-info-set))

(define gen:get-checksum-table
  (generic plain-corpus% get-checksum-table))
(define (corpus-get-checksum-table c)
  (send-generic c gen:get-checksum-table))

