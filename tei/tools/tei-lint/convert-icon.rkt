#lang racket/base

(require racket/runtime-path
         pict
         file/ico
         icns)

(define-runtime-path png-path
  "tei-lint.png")
(define-runtime-path icns-path
  "tei-lint.icns")
(define-runtime-path ico-path
  "tei-lint.ico")

(define icon
  (bitmap png-path))

(write-icos
 (for/list ([size (in-list '(256 128 64 48 32 16))])
   (argb->ico size size
              (pict->argb-pixels
               (cc-superimpose (blank size size)
                               (scale-to-fit icon size size)))))
 ico-path
 #:exists 'replace)

(with-output-to-file icns-path
  #:exists 'replace
  (λ ()
    (void (write-bytes
           (pict->icns-bytes icon)))))

