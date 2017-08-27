#lang racket

(require racket/runtime-path
         pict
         file/ico
         )

(define-runtime-path png-path
  "tei-lint.png")
(define icns-path
  "tei-lint.icns")
(define ico-path
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

(define sips
  (find-executable-path "sips"))

(when sips
  (void
   (parameterize ([current-output-port (open-output-nowhere)])
       (system* sips
                "-s" "format" "icns"
                ico-path
                "--out" icns-path))))


