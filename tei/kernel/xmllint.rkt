#lang racket/base

(require racket/path
         racket/port
         racket/system
         racket/contract
         racket/file
         setup/matching-platform
         racket/runtime-path
         ricoeur/kernel/pre-kernel-lib
         "schema.rkt"
         (for-syntax racket/base
                     setup/matching-platform
                     ))

(provide xmllint-available?
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

(define-runtime-path-list list:xmllint-win32-x86_64
  (if (matching-platform? "win32\\x86_64" #:cross? #t)
      '((module xmllint-win32-x86_64 #f))
      null))

(define xmllint
  (let ([pth
         (or (find-executable-path "xmllint")
             (and (not (null? list:xmllint-win32-x86_64))
                  (dynamic-require (car list:xmllint-win32-x86_64)
                                   'xmllint
                                   (λ () #f))))]
        [null-out (open-output-nowhere)]
        [null-in (open-input-string "")])
    (define (check-can-run pth)
      (parameterize ([current-output-port null-out]
                     [current-error-port null-out]
                     [current-input-port null-in])
        (with-handlers ([exn:fail? (λ (e) #f)])
          (system* pth #"--version"))))
    (cond
      [(and pth
            (file-exists? pth)
            (check-can-run pth))
       pth]
      [(and (file-exists? "/opt/local/bin/xmllint")
            (check-can-run (check-can-run "/opt/local/bin/xmllint")))
       (eprintf "~a;\n ~a\n  broken: ~e\n  using: ~e"
                "warning: xmllint: couldn't run found executable"
                "falling back to system version"
                pth
                "/opt/local/bin/xmllint")
       "/opt/local/bin/xmllint"]
      [pth
       (eprintf "warning: xmllint not executable\n  path: ~e"
                pth)
       #f]
      [else
       (log-warning "xmllint not found")
       #f])))

(define xmllint-available?
  (let ([? (not (not xmllint))])
    (λ () ?)))

(define empty-input-port
  (open-input-string ""))

(define (valid-xml-file? #:quiet? [quiet? #t] . l-pths)
  (or (not xmllint)
      (parameterize ([current-input-port empty-input-port]
                     [current-error-port
                      (if quiet?
                          (open-output-nowhere)
                          (current-error-port))])
        (apply system*
               xmllint
               "--dtdvalid" DR-TEI.dtd ;; path->url ????
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
     (TODO/void call/prettyprint-xml-out #:
                replace w/ pipe that does newline
                transformation right on Windows.)
     (define tmp
       (make-temporary-file))
     (dynamic-wind
      void
      (λ ()
        (cond
          [(call-with-output-file* tmp
             #:mode 'binary
             #:exists 'truncate/replace
             (λ (tmp-file-out) 
               (parameterize ([current-input-port in-from-pipe]
                              [current-output-port tmp-file-out]
                              [current-error-port xmllint-stderr])
                 (system* xmllint "--pretty" "1" "-"))))
           (call-with-input-file* tmp
             #:mode 'text
             (λ (tmp-file-in)
               (copy-port tmp-file-in (current-output-port))))
           rslt]
          [else
           (error 'call/prettyprint-xml-out
                "~a\n  given: ~e\n  result: ~e\n  message...:\n   ~e"
                "xmllint encountered an error"
                thunk
                rslt
                (get-output-string xmllint-stderr))]))
      (λ ()
        (when (file-exists? tmp)
          (delete-file tmp))))]
    [else
     (thunk)]))


;(call/prettyprint-xml-out (λ () (displayln "<p><b>Thin</b><i>other</i></p>")))
;(call/prettyprint-xml-out (λ () (displayln "twljkenvqlf<wbrbvwef")))

