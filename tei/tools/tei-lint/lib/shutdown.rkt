#lang racket/gui

(require racket/async-channel)

(provide wait-to-implicitly-exit
         make+register-eventspace
         (contract-out
          [call/disable-implicit-exit
           (-> (-> any) any)]
          ))

(define registered-eventspaces
  (weak-set (current-eventspace)))

(define blockers-ach
  (make-async-channel))

(define (wait-to-implicitly-exit)
  (define (clear-blockers)
    (define maybe-evt
      (async-channel-try-get blockers-ach))
    (when maybe-evt
      (yield maybe-evt)
      (wait-to-implicitly-exit)))
  (define (clear-eventspaces)
    (define live-eventspaces
      (for/list ([es (in-weak-set registered-eventspaces)]
                 #:unless (sync/timeout 0 es))
        es))
    (unless (null? live-eventspaces)
      (yield (apply choice-evt live-eventspaces))
      (wait-to-implicitly-exit)))
  ;; Call (clear-blockers) before checking eventspaces
  ;; to prevent a backlog in blockers-ach
  (clear-blockers)
  (clear-eventspaces)
  (clear-blockers))

(define (make+register-eventspace)
  (define it
    (make-eventspace))
  (set-add! registered-eventspaces it)
  it)

(define (call/disable-implicit-exit thunk)
  (define sema
    (make-semaphore))
  ;; (choice-evt sema (current-custodian)) ???
  ;; continuation barrier
  (async-channel-put blockers-ach sema)
  (dynamic-wind
   void
   thunk
   (λ () (semaphore-post sema))))

  