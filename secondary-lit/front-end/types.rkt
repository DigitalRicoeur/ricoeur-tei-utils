#lang typed/racket/no-check ;; FIXME !!!
;; See https://github.com/racket/typed-racket/issues/902

(provide read-metadata-archive)

(require "../kernel/types.rkt"
         (submod "../kernel/types.rkt"
                 private-for-frontend))

(define-metadata-archive-subtype deserialized-metadata-archive
  #:transparent
  #:accessor-name metadata-archive->internal
  #:reader-name read-metadata-archive
  #:internal-representation
  (struct metadata
    () ;; TODO
    #:transparent
    #:type-name Internal-Metadata)
  #:build (λ (prefab)
            ;; TODO
            (values)))

metadata-archive->internal deserialized-metadata-archive? metadata?
