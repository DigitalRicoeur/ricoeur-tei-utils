#lang racket

(provide directory-validate-xml
         (contract-out
          [valid-xml-file?
           (->* {path-string?}
                {#:quiet? any/c}
                any/c)]
          ))

(define xmllint
  (find-executable-path "xmllint"))
  
(define (valid-xml-file? pth #:quiet? [quiet? #t])
  (parameterize ([current-error-port (if quiet?
                                         (open-output-nowhere)
                                         (current-error-port))])
    (system* xmllint
             "--valid"
             "--noout"
             pth)))

(define/contract (directory-validate-xml dir)
  (-> (and/c path-string? directory-exists?) any)
  (for ([pth (in-directory dir)]
        #:when (regexp-match? #rx"\\.xml$" pth))
    (valid-xml-file? pth #:quiet? #f)))