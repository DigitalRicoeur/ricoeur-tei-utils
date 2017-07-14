#lang racket/base

(require racket/path
         racket/port
         racket/system
         racket/contract
         )

(provide directory-validate-xml
         xmllint-available?
         (contract-out
          [xml-path?
           (-> path-string? any)]
          [valid-xml-file?
           (->* {path-string?}
                {#:quiet? any/c}
                #:rest (listof path-string?)
                boolean?)]
          ))

(define xmllint
  (find-executable-path "xmllint"))

(unless xmllint
  (log-warning "xmllint not found"))

(define (xmllint-available?)
  xmllint)

(define (xml-path? pth)
  (let ([ext (path-get-extension pth)])
    (and ext (regexp-match? #rx"^(?i:\\.xml)$" ext))))

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
               #:when (xml-path? pth))
      pth))
  (or (null? pths)
      (apply valid-xml-file?
             #:quiet? quiet?
             pths)))
