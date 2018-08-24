#lang racket/gui

(require framework
         ricoeur/tei/base
         "lib.rkt"
         "toolkit.rkt"
         "document.rkt"
         )

(provide (contract-out
          [file->proto-frame
           (->* {path-string?}
                {#:dir-valid? any/c
                 #:dir-frame (or/c #f (is-a?/c directory-frame<%>))}
                (is-a?/c proto-frame<%>))]
          ))

(define (file->proto-frame pth
                           #:dir-frame [dir-frame #f]
                           #:dir-valid? [dir-valid? #f])
  (define xmllint-out
    (open-output-string))
  (define old-modify-seconds
    (file-or-directory-modify-seconds pth))
  (define (handle-error str/exn)
    (new error-proto-frame%
         [path pth]
         [dir-frame dir-frame]
         [val str/exn]))
  (cond
    [(not (or dir-valid?
              (parameterize ([current-output-port xmllint-out]
                             [current-error-port xmllint-out])
                (valid-xml-file? #:quiet? #f pth))))
     (handle-error (get-output-string xmllint-out))]
    [else
     (define doc/exn
       (with-handlers ([exn:fail? values])
         (file->tei-document pth)))
     (if (exn:fail? doc/exn)
         (handle-error doc/exn)
         (new tei-document-proto-frame%
              [path pth]
              [dir-frame dir-frame]
              [doc doc/exn]
              [old-modify-seconds old-modify-seconds]))]))





