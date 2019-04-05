#lang racket/base

(require racket/contract
         racket/class
         racket/set
         racket/promise
         ricoeur/tei/base
         syntax/parse/define
         racket/stxparam
         racket/splicing
         (for-syntax racket/base
                     syntax/transformer))

(provide checksum-table/c
         corpus-mixin
         super-docs
         (contract-out
          [plain-corpus%
           (class/c (init [docs (instance-set/c tei-document?)]))]
          [corpus-get-instance-info-set
           (-> (is-a?/c plain-corpus%)
               (instance-set/c))]
          [corpus-get-checksum-table
           (-> (is-a?/c plain-corpus%)
               checksum-table/c)]
          ))

(define/final-prop checksum-table/c
  (hash/c symbol? symbol?
          #:immutable #t))

(define-local-member-name initialize)

(define plain-corpus%
  (class* object% {(interface ()
                     [get-instance-info-set
                      (->m (instance-set/c))]
                     [get-checksum-table
                      (->m checksum-table/c)]
                     initialize)}
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Initialization:
    (init [docs (instance-set)])
    (define info
      (instance-set
       (set-map docs get-plain-instance-info)))
    (define pr:checksum-table
      (delay/thread
       (for/hasheq ([doc (in-instance-set docs)])
         (values (instance-title/symbol doc)
                 (tei-document-checksum doc)))))
    ;; Must come at the end of initialization:
    (initialize docs)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods:
    (define/public-final (get-instance-info-set)
      info)
    (define/public-final (get-checksum-table)
      (force pr:checksum-table))
    (define/pubment (initialize docs)
      (inner (void) initialize docs))
    #|END class plain-corpus%|#))

(define plain-corpus<%>
  (class->interface plain-corpus%))


(define gen:get-instance-info-set
  (generic plain-corpus<%> get-instance-info-set))
(define (corpus-get-instance-info-set c)
  (send-generic c gen:get-instance-info-set))

(define gen:get-checksum-table
  (generic plain-corpus<%> get-checksum-table))
(define (corpus-get-checksum-table c)
  (send-generic c gen:get-checksum-table))

;                                          
;                                          
;                                          
;                                          
;              ;               ;           
;              ;;              ;;          
;  ; ;; ;;  ;;;;;  ;;   ;;  ;;;;;   ;; ;   
;  ;; ;; ;     ;;    ;  ;      ;;   ;;; ;  
;  ;; ;; ;;    ;;    ; ;       ;;   ;;  ;; 
;  ;; ;; ;;    ;;     ;        ;;   ;;  ;; 
;  ;; ;; ;;    ;;    ; ;       ;;   ;;  ;; 
;  ;; ;; ;;    ;;   ;   ;      ;;   ;;  ;; 
;  ;; ;; ;;    ;;  ;;   ;;     ;;   ;;  ;; 
;                                          
;                                          
;                                          
;                                          

(define-syntax (super-docs stx)
  (raise-syntax-error
   #f "only allowed inside a corpus-mixin expression" stx))

(define-rename-transformer-parameter local-internal-name
  (make-rename-transformer #'no-local-internal-name))

(define-syntax no-local-internal-name #f)

(define (do-super-docs-ref bx)
  (or (unbox bx)
      (raise (exn:fail:contract:variable
              (string-append
               "super-docs: undefined;\n"
               " cannot access documents before super-class initialization")
              (current-continuation-marks)
              'super-docs))))

(define-syntax-parser corpus-mixin
  #:track-literals
  [(_ (~describe "\"from\" interfaces" [from<%>:expr ...])
      (~describe "\"to\" interfaces" [to<%>:expr ...])
      (~describe "#:avoid-bug clause"
                 [#:avoid-bug (~and src-super-docs (~literal super-docs))])
      (~describe "class clause" src-class-clause:expr)
      ...+)
   ;; this would be much nicer with a fix for any of
   ;;   - https://github.com/racket/racket/issues/2578
   ;;   - https://github.com/racket/racket/issues/1898
   ;;   - https://github.com/racket/racket/issues/2580
   ;;   - https://github.com/racket/racket/issues/2579
   #`(let ([src-super-docs 'tmp]) ;; this is so it doesn't get local-expanded
       (define-local-member-name private-field private-init)
       (mixin [plain-corpus<%> from<%> ...] [to<%> ...]
         (field [private-field (box #f)])
         (init [(src-super-docs private-init)
                (λ () (do-super-docs-ref private-field))])
         (define/augment (initialize docs)
           (set-box! private-field docs)
           (inner (void) initialize docs))
         src-class-clause ...
         (set! private-field #f)))])

#|
(define-syntax-parser super-docs
  [(_)
   #:fail-unless (syntax-local-value #'local-internal-name)
   "only allowed inside a corpus-mixin expression"
   #'(do-super-docs-ref local-internal-name)])

(define-rename-transformer-parameter local-internal-name
  (make-rename-transformer #'no-local-internal-name))

(define-syntax no-local-internal-name #f)

(define-syntax-parser corpus-mixin
  [(_ (~describe "\"from\" interfaces" [from<%>:expr ...])
      (~describe "\"to\" interfaces" [to<%>:expr ...])
      (~describe "class clause" class-clause:expr)
      ...+)
   #'(let ()
       (define-local-member-name private-field private-init)
         (mixin [plain-corpus<%> from<%> ...] [to<%> ...]
           ;; or, pass it to the superclass via splicing-parameterize ...
           (field [private-field (box #f)])
           (init [private-init
                  ;; technically, a by-position arg could displace this :(
                  private-field])
           (define/augment (initialize docs)
             (set-box! private-field docs)
             (inner (void) initialize docs))
           (splicing-syntax-parameterize ([local-internal-name
                                           (make-rename-transformer
                                            #'private-init)])
             class-clause ...)
           (set! private-field #f)))])
|#
#|
(define (make-corpus-mixin initialize-this-method-key)
  (define-member-name initialize-this initialize-this-method-key)
  (mixin {(class->interface plain-corpus%)} {}
    (super-new)
    (inspect #f)
    (abstract initialize-this)
    (define/augment (on-initialize docs)
      (initialize-this docs)
      (inner (void) on-initialize docs))))
|#
