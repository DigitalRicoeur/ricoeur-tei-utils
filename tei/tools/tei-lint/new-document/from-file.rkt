#lang racket/gui

(require "new-tei-document-frame.rkt"
         ricoeur/tei/tools/tei-lint/lib
         ricoeur/tei
         framework
         adjutor
         )

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
  (if (and amp? lt?)
      'both
      (or amp? lt?)))

(define (no-escaped-entity-problems? maybe-pth str [parent #f])
  (TODO/void no-escaped-entity-problems?))

(define (create-new-tei-document [parent #f])
  (TODO/void block auto shutdown)
  (define pth
    (get-txt-path parent))
  (when pth
    (create-new-tei-document/from-file pth parent)))

(define (create-new-tei-document/from-file pth [parent #f])
  (create-new-tei-document/from-string pth
                                       (file->text-string pth)
                                       parent))

(define (create-new-tei-document/from-string maybe-pth str [parent #f])
  (when (no-escaped-entity-problems? maybe-pth str parent)
    (define f
      (new new-tei-document-frame%
           [string str]
           [path maybe-pth]))
    (send f show #t)
    f))





