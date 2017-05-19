#lang racket

(provide directory-validate-xml
         (contract-out
          [valid-xml-file?
           (->* {path-string?}
                {#:quiet? any/c}
                #:rest (listof path-string?)
                boolean?)]
          ))

(define xmllint
  (find-executable-path "xmllint"))

(unless xmllint
  (log-warning "xmllint not found")
  (displayln "xmllint not found" (current-error-port)))

(define (valid-xml-file? #:quiet? [quiet? #t] . l-pths)
  (or (not xmllint)
      (parameterize ([current-error-port
                      (if quiet?
                          (open-output-nowhere)
                          (current-error-port))])
        (apply system*
               xmllint
               "--valid"
               "--noout"
               l-pths))))

(define/contract (directory-validate-xml dir
                                         #:quiet? [quiet? #f])
  (->* {(and/c path-string? directory-exists?)}
       {#:quiet? any/c}
       boolean?)
  (define pths
    (for/list ([pth (in-directory dir)]
               #:when (regexp-match? #rx"\\.xml$" pth))
      pth))
  (or (null? pths)
      (apply valid-xml-file?
             #:quiet? quiet?
             pths)))
