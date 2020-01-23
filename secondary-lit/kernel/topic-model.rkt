#lang racket/base

;; In its own file for fastest script startup time.

(provide (all-from-out (submod "." typed))
         topic-model)

;; Is there a meaning to the order of the assosciated words?

(module typed typed/racket/base
  (provide Topic-Model
           Topic-Model-Spec
           topic-model?
           topic-model-spec
           Nonempty-Listof)
  (module+ private
    (provide make-topic-model))
  (define-type (Nonempty-Listof A)
    (Pairof A (Listof A)))
  (define-type Topic-Model-Spec
    (Immutable-HashTable Symbol (Pairof Positive-Float (Nonempty-Listof Symbol))))
  (struct topic-model
    ([spec : Topic-Model-Spec])
    #:constructor-name make-topic-model
    #:type-name Topic-Model
    #:transparent))

(require (submod "." typed)
         (submod "." typed private)
         (only-in typed/racket/base
                  ann
                  with-type)
         (for-syntax racket/base
                     syntax/parse
                     (only-in typed/untyped-utils
                              syntax-local-typed-context?)
                     (only-in racket/list
                              check-duplicates)))

(module* test racket/base
  (require rackunit
           racket/runtime-path
           (for-syntax racket/base)
           (submod ".."))
  (check-pred topic-model?
              (topic-model)
              "(topic-model) should work in untyped context")
  (module typed-test typed/racket/base
    (require (submod ".." ".."))
    (define ignored (topic-model)))
  (define-runtime-module-path-index typed-test-mpi
    '(submod "." typed-test))
  (check-not-exn
   (λ () (dynamic-require typed-test-mpi #f))
   "(topic-model) should work in typed context"))

(define-syntax (topic-model stx)
  (define-syntax-class topic-name
    #:description "topic name"
    (pattern (~and x:id (~not (~datum ||)))
             #:do [(define str (symbol->string (syntax->datum #'x)))]
             #:fail-unless (char-upper-case? (string-ref str 0))
             "expected an identifier beginning with an upper-case letter"))
  (define-syntax-class prob
    #:description #f ;; FIXME
    (pattern x:expr
             #:do [(define n (syntax->datum #'x))]
             #:fail-when (and (number? n) (exact? n))
             "expected an inexact number"
             #:fail-unless (and (flonum? n)
                                (< 0 n 1))
             "expected a number between 0.0 and 1.0, exclusive"))
  (syntax-parse stx
    [(_ (~describe "topic declaration"
                   [name:topic-name n:prob ((~describe "assosciated word" word:id)
                                            ...+)])
        ...)
     #:fail-when (check-duplicates (syntax->list #'(name ...))
                                   eq?
                                   #:key syntax-e)
     "duplicate topic name"
     (define src-datum
       (syntax->datum #'([name . (n . (word ...))] ...)))
     (define (add-tooltip stx id str/proc)
       (define start-pos (sub1 (syntax-position id)))
       (define end-pos (+ start-pos (syntax-span id)))
       (define vec (vector-immutable id start-pos end-pos str/proc))
       (syntax-property stx
                        'mouse-over-tooltips
                        (cons vec (syntax-property stx 'mouse-over-tooltips))))
     (define make-word-tooltip-thunk
       (let ()
         ;; list topics in source order in the tooltip
         (define table/proc
           (λ ()
             (define gathered
               (for*/fold ([hsh #hasheq()])
                          ([clause (in-list src-datum)]
                           [name (in-value (car clause))]
                           [word (in-list (cddr clause))])
                 (hash-set hsh word (cons name (hash-ref hsh word null)))))
             (define table
               (for/hasheq ([{word reversed-names} (in-immutable-hash gathered)])
                 (values word (if (null? (cdr reversed-names))
                                  (format "assosciated word for topic: ~a"
                                          (car reversed-names))
                                  (apply string-append
                                         "assosciated word for topics:"
                                         (map (λ (name)
                                                (format "\n  - ~a" name))
                                              (reverse reversed-names)))))))
             (set! table/proc table)))
             (define (make-word-tooltip-thunk id)
               (define sym (syntax-e id))
               (λ ()
                 (when (procedure? table/proc)
                   (table/proc))
                 (hash-ref table/proc sym)))
         make-word-tooltip-thunk))
     (let* ([result (datum->syntax #f (make-immutable-hasheq src-datum))]
            [result #`(make-topic-model
                       (ann (quote #,result)
                            Topic-Model-Spec))]
            [result (if (syntax-local-typed-context?)
                        result
                        #`(with-type
                           #:result Topic-Model        
                           #,result))]
            [result (for/fold ([result result])
                              ([id (in-list (attribute name))])
                      (add-tooltip result id "topic name"))]
            [result (for/fold ([result result])
                              ([id (in-list (syntax->list #'((~@ word ...) ...)))])
                      (add-tooltip result id (make-word-tooltip-thunk id)))])
       result)]))

     
