#lang s-exp syntax/module-reader
ricoeur/tei/kernel/lang/specification-lang
#:whole-body-readers? #t
#:read read*
#:read-syntax read-syntax*
#:info
(λ (key default-val default-filter-proc)
  (define (default)
    ((scribble-base-info) key default-val default-filter-proc))
  (define (try-dynamic-require lib export)
    (dynamic-require lib export default))
  (case key
    [(color-lexer)
     (try-dynamic-require 'ricoeur/tei/kernel/lang/color-lexer 'color-lexer)]
    [(drracket:default-extension) "rkt"]
    [else
     (default)]))
                     

(require scribble/reader
         (only-in scribble/base/reader
                  ;; not documented, but used by #lang scribble/lp2
                  scribble-base-info
                  ))

(define read-syntax*
  (make-at-reader #:syntax? #t
                  #:inside? #t
                  #:command-char #\ƒ))

(define (read* in)
  (syntax->datum 
   (read-syntax* #f in)))



  
