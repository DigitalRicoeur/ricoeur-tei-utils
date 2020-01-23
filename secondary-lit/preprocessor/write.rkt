#lang racket/base

(require "read.rkt"
         "../kernel/types.rkt"
         racket/contract)

(provide (contract-out
          [build/write-metadata-archive
           (->* [(or/c path-string? (listof path-string?))
                 #:model topic-model?]
                [#:show-progress? any/c
                 #:output (or/c output-port? path-string?)
                 #:exists (or/c 'error 'append 'update
                                'replace 'truncate 'truncate/replace)]
                void?)]
          ))

(define (build/write-metadata-archive args
                                      #:model model
                                      #:output [out (current-output-port)]
                                      #:exists [exists-flag 'error]
                                      #:show-progress? [show-progress? #f])
  (define archive
    (read-jstor-metadata-files args
                               #:model model
                               #:show-progress? show-progress?))
  (when show-progress?
    (eprintf "writing binary archive...\n"))
  (if (output-port? out)
      (write-metadata-archive archive out)
      (call-with-output-file* out
        #:exists exists-flag
        (λ (out) (write-metadata-archive archive out))))
  (when show-progress?
    (eprintf "finished\n")))
