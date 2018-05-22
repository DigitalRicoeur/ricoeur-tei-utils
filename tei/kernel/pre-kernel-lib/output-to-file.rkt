#lang racket/base

(require racket/file
         racket/port
         racket/contract
         )

(module+ test
  (require rackunit
           (submod "..")))

(provide (contract-out
          [with-output-to-file/unless-exn
              (->* {path-string?
                    (-> any/c)}
                   {#:mode (or/c 'binary 'text)
                    #:exists (or/c 'error 'append 'update
                                   'replace 'truncate 'truncate/replace)
                    #:buffer (or/c 'memory 'file)}
                   any)]
          ))

;; with-output-to-file may overwrite an existing
;; file even if an exception is raised

(define (with-output-to-file/unless-exn pth	 	 	 	 
          thunk
          #:buffer [buffer-type 'memory]
          #:mode [mode-flag 'binary]	 	 
          #:exists [exists-flag 'error])
  (case buffer-type
    [(memory)
     (define-values {in-from-pipe out-to-pipe}
       (make-pipe))
     (define rslt
       (parameterize ([current-output-port out-to-pipe])
         (thunk)))
     (close-output-port out-to-pipe)
     (call-with-output-file* pth
       #:mode mode-flag
       #:exists exists-flag
       (λ (out)
         (copy-port in-from-pipe out)))
     rslt]
    [(file)
     (define tmp
       (make-temporary-file))
     (dynamic-wind
      void
      (λ ()
        (define rslt
          (with-output-to-file tmp
            #:exists 'truncate/replace
            thunk))
        (call-with-input-file* tmp
          (λ (in-from-tmp)
            (call-with-output-file* pth
              #:mode mode-flag
              #:exists exists-flag
              (λ (out)
                (copy-port in-from-tmp out)))))
        rslt)
      (λ ()
        (delete-file tmp)))]))






(module+ test

  (define (do-with-output-to-file-tests pth buffer)
    (with-check-info (['|#:buffer argument| buffer])
      (check-not-exn
       (λ ()
         (with-output-to-file pth
           #:exists 'truncate/replace
           (λ () (write 'orig))))
       "should be able to write to pth normally")

      (check-exn #rx"example"
                 (λ ()
                   (with-output-to-file/unless-exn pth
                     #:buffer buffer
                     #:exists 'truncate/replace
                     (λ () (error 'example))))
                 "should raise exn:fail of 'example")

      (check-eq? (file->value pth)
                 'orig
                 "orig value should still be in file")

      (check-eq? (with-output-to-file/unless-exn pth
                   #:buffer buffer
                   #:exists 'truncate/replace
                   (λ () (write 'new) 'done))
                 'done
                 "should write new value without exn and return 'done")

      (check-eq? (file->value pth)
                 'new
                 "new value should be in file")))
  
  (define pth
    (make-temporary-file))

  (test-case
   "with-output-to-file/unless-exn"
   (do-with-output-to-file-tests pth 'memory)
   (do-with-output-to-file-tests pth 'file))
     
  (delete-file pth))




