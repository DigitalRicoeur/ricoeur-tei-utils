#lang racket/gui

(require "new-tei-document-frame.rkt"
         ricoeur/tei/tools/tei-lint/lib
         ricoeur/tei/base
         framework
         adjutor
         )

(provide (contract-out
          [create-new-tei-document
           (->* {}
                {(or/c #f (is-a?/c frame%) (is-a?/c dialog%))
                 #:after-frame-show (-> any)}
                any)]
          ))

(module+ main
  (create-new-tei-document))

(define (get-txt-path [parent #f])
  (define pth
    (get-file
     "To begin, select a plain text file."
     parent
     #f
     #f
     #f
     null
     '(("Plain Text" "*.txt"))))
  (let ([pth (if (string? pth)
                 (string->immutable-string pth)
                 pth)])
    (and pth
         (file-exists? pth)
         pth)))

(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))
  
(define (file->text-string pth)
  (string->immutable-string
   (call-with-input-file* pth
     #:mode 'text
     (λ (in)
       (discard-bom in)
       (port->string in)))))

(define (contains-escaped-entities? str)
  ;; check that the file doesn't contain &amp; or &lt; already
  (define amp?
    (regexp-match? #rx"&amp;" str))
  (define lt?
    (regexp-match? #rx"&lt;" str))
  (cond
    [(and amp? lt?)
     'both]
    [amp? 'amp]
    [lt? 'lt]
    [else #f]))

(define (no-escaped-entity-problems? maybe-pth str [parent #f])
  (define which-entities
    (contains-escaped-entities? str))
  (cond
    [which-entities
     (case (message-box/custom
            "Warning! - TEI Lint"
            (string-append
             "Warning! File contains XML entities.\n\n"
             "The plain-text file you selected (\""
             (if maybe-pth
                 (path->string* maybe-pth)
                 "actually not a file")
             "\") already contains the XML entity escape sequence"
             (case which-entities
               [(both) "s \"&amp;\" and \"&lt;\"."]
               [(amp) " \"&amp;\"."]
               [(lt) " \"&lt;\"."])
             "\n\nThis probably means that the file has already"
             " been encoded and should not be used.")
            "Use Anyway"
            "Cancel"
            #f
            parent
            '(caution no-default))
       [(1) 'ignore-problems]
       [else #f])]
    [else
     'no-problems]))

(define (create-new-tei-document [parent #f]
                                 #:after-frame-show [after-show-callback void])
  (TODO/void block auto shutdown)
  (define pth
    (get-txt-path parent))
  (when pth
    (create-new-tei-document/from-file pth
                                       parent
                                       #:after-frame-show after-show-callback)))

(define (create-new-tei-document/from-file pth
                                           [parent #f]
                                           #:after-frame-show [after-show-callback void])
  (create-new-tei-document/from-string pth
                                       (file->text-string pth)
                                       parent
                                       #:after-frame-show after-show-callback))

(define (create-new-tei-document/from-string maybe-pth
                                             str
                                             [parent #f]
                                             #:after-frame-show [after-show-callback void])
  (when (no-escaped-entity-problems? maybe-pth str parent)
    (define f
      (new new-tei-document-frame%
           [string str]
           [path maybe-pth]))
    (send f show #t)
    (after-show-callback)
    f))





