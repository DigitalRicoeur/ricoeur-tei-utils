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
          [call/prettyprint-xml-out
           (-> (-> any/c) any/c)]
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


(define (call/prettyprint-xml-out thunk)
  (cond
    [xmllint
     (define-values {in-from-pipe out-to-pipe}
       (make-pipe))
     (define xmllint-stderr
       (open-output-string))
     (define rslt
       (parameterize ([current-output-port out-to-pipe])
         (thunk)))
     (close-output-port out-to-pipe)
     (if (parameterize ([current-input-port in-from-pipe]
                        [current-error-port xmllint-stderr])
           (system* xmllint "--pretty" "1" "-"))
         rslt
         (error 'call/prettyprint-xml-out
                (format "xmllint encountered an error\n  given: ~e\n  message...: ~e"
                        thunk
                        (get-output-string xmllint-stderr))))]
    [else
     (thunk)]))


;(call/prettyprint-xml-out (λ () (displayln "<p><b>Thin</b><i>other</i></p>")))
;(call/prettyprint-xml-out (λ () (displayln "twljkenvqlf<wbrbvwef"))))

