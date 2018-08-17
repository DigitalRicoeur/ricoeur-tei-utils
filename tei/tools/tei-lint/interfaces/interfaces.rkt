#lang racket/gui

(require ricoeur/tei/base
         )

(provide lint-status/c
         lint-status<%>
         directory-frame<%>
         get-dir-frame/false<%>
         dir-frame/false/c
         tei-document-frame<%>
         document-frame-component?
         (contract-out
          [lint-status-more-urgent?
           (-> lint-status/c lint-status/c any/c)]
          ;;;;
          [document-frame-component
           (-> (-> tei-document?
                   (values lint-status/c
                           initialize-proc/c))
               document-frame-component?)]
          [document-frame-component/ok
           (-> (-> tei-document? initialize-proc/c)
               document-frame-component?)]
          [constant-document-frame-component
           (-> initialize-proc/c
               document-frame-component?)]
          [document-frame-component-append
           (-> document-frame-component? ...
               document-frame-component?)]
          [document-frame-component-run
           (-> document-frame-component?
               tei-document?
               (values lint-status/c
                       initialize-proc/c))]
          ))

(define lint-status/c
  (or/c 'error 'warning 'ok))

(define lint-status<%>
  (interface ()
    [get-lint-status (->m lint-status/c)]))

(define (lint-status-more-urgent? a b)
  (case a
    [(ok) #f]
    [(error) (not (eq? 'error b))]
    [(warning) (eq? 'ok b)]))

(define frame<%>
  (class->interface frame%))

(define directory-frame<%>
  (interface (frame<%>)
    [open-additional (->m any)]
    [call-in-directory-context (->m (-> any) any)]
    [refresh-directory! (->m any)]))

(define/final-prop dir-frame/false/c
  (or/c #f (is-a?/c directory-frame<%>)))

(define get-dir-frame/false<%>
  (interface ()
    [get-dir-frame/false (->m dir-frame/false/c)]))

(define tei-document-frame<%>
  (interface (frame<%> lint-status<%> get-dir-frame/false<%>)
    [get-old-modify-seconds (->m real?)]
    [get-path (->m path-string?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define initialize-proc/c
  (-> (is-a?/c tei-document-frame<%>) any))

(struct document-frame-component (proc))

(define (document-frame-component/ok simple-proc)
  (document-frame-component
   (λ (doc)
     (values 'ok
             (simple-proc doc)))))

(define (constant-document-frame-component initialize)
  (document-frame-component
   (λ (doc)
     (values 'ok initialize))))

(define (document-frame-component-run dfc doc)
  ((document-frame-component-proc dfc) doc))

(define (document-frame-component-append . dfcs)
  (document-frame-component
   (λ (doc)
     (for/lists/define (l-status l-initialize)
                       ([dfc (in-list dfcs)])
       (document-frame-component-run dfc doc))
     (values (let loop ([so-far 'ok]
                        [l-status l-status])
               (match l-status
                 ['()
                  so-far]
                 [(cons 'error _)
                  'error]
                 [(cons 'warning l-status)
                  (loop 'warning l-status)]
                 [(cons _ l-status)
                  (loop so-far l-status)]))
             (λ (f)
               (for ([initialize (in-list l-initialize)])
                 (initialize f)))))))

