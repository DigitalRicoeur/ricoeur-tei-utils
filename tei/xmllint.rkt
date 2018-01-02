#lang racket/base

(require racket/path
         racket/port
         racket/system
         racket/contract
         setup/matching-platform
         ricoeur/lib/xml-path
         )

(provide xmllint-available?
         xml-path?
         (contract-out
          [valid-xml-file?
           (->* {path-string?}
                {#:quiet? any/c}
                #:rest (listof path-string?)
                boolean?)]
          [directory-validate-xml
           (->* {(and/c path-string? directory-exists?)}
                {#:quiet? any/c}
                boolean?)]
          [call/prettyprint-xml-out
           (-> (-> any/c) any/c)]
          ))

(define xmllint
  (or (find-executable-path "xmllint")
      (and (matching-platform? "win32\\x86_64")
           (collection-file-path "xmllint.exe"
                                 "xmllint-win32-x86_64"
                                 #:fail not))))

(unless xmllint
  (log-warning "xmllint not found"))

(define (xmllint-available?)
  xmllint)

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

(define (directory-validate-xml dir
                                #:quiet? [quiet? #f])
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
                "~a\n  given: ~e\n  result: ~e\n  message...:\n   ~e"
                "xmllint encountered an error"
                thunk
                rslt
                (get-output-string xmllint-stderr)))]
    [else
     (thunk)]))


;(call/prettyprint-xml-out (λ () (displayln "<p><b>Thin</b><i>other</i></p>")))
;(call/prettyprint-xml-out (λ () (displayln "twljkenvqlf<wbrbvwef")))

