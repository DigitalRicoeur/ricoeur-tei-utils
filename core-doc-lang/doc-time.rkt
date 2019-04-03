#lang racket

;; This intentionally uses #lang racket as the base lang
;; for documentation-time.

(require "begin-for-runtime.rkt"
         (only-in scribble/manual/lang
                  [#%top-interaction scribble-top-interaction]
                  [#%module-begin scribble-module-begin])
         scribble/manual
         scribble/decode
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     syntax/flatten-begin
                     ))

(provide (except-out (all-from-out racket)
                     #%top-interaction
                     #%module-begin)
         (all-from-out "begin-for-runtime.rkt")
         (all-from-out scribble/manual)
         (all-from-out syntax/parse/define)
         require-self-for-label
         (rename-out
          [module-begin #%module-begin]
          [scribble-top-interaction #%top-interaction])
         (for-syntax (all-from-out racket/base)
                     (all-from-out syntax/parse)))

(define-syntax-parser module-begin
  [(_ (~optional (~and #:fake fake))
      doc:id body:expr ...)
   #`(#%module-begin
      (expand-and-collect doc
                          #:real? #,(if (attribute fake)
                                        #'#f
                                        #'#t)
                          #:collected ()
                          #:body (body ...)))])

(define-for-syntax stop-list
  (list #'begin ;; it's implicitly added, but let's be clear
        ;; Need to not try to expand these:
        #'#%require #'require
        #'#%provide #'provide
        #'define-values
        #'define-syntaxes
        #'module #'module* #'module+))

(define-for-syntax currently-expanding-for-real?
  (make-parameter #t))

(define-syntax-parser expand-and-collect
  ;; TODO: see note in scribble/doclang about not doing
  ;; every string constant individually
  [(_ doc:id
      #:real? real?:boolean
      #:collected (collected:expr ...)
      #:body ())
   #'(begin (define doc
              (decode (list collected ...)))
            (provide doc))]
  [(_ doc:id
      #:real? real?:boolean
      #:collected (collected:expr ...)
      #:body (this:expr to-go:expr ...))
   (syntax-parse (parameterize ([currently-expanding-for-real?
                                 (syntax->datum #'real?)])
                   (local-expand #'this 'module stop-list))
     #:literals {begin}
     [(begin inner:expr ...)
      #:with (flattened:expr ...)
      (flatten-all-begins
       #'(begin inner ...))
      #'(expand-and-collect doc
                            #:real? real?
                            #:collected (collected ...)
                            #:body (flattened ... to-go ...))]
     [(~and definition (form:id _ ...))
      #:fail-unless (member #'form stop-list free-identifier=?)
      "not a definition"
      #'(begin definition
               (expand-and-collect doc
                                   #:real? real?
                                   #:collected (collected ...)
                                   #:body (to-go ...)))]
     [new
      #'(expand-and-collect doc
                            #:real? real?
                            #:collected (collected ... new)
                            #:body (to-go ...))])])



(define-syntax-parser require-self-for-label
  ;; thanks to https://groups.google.com/d/msg/racket-users/7OrQFTOGBaw/gVlotkaYBwAJ
  [(_)
   #:fail-unless (eq? 'module (syntax-local-context))
   "only allowed in at the module level"
   (syntax-local-introduce 
   ;; Detect whether we've in the `doc` submodule:
    (if (currently-expanding-for-real?)
        #'(require (for-label (submod "..")))
        #'(require)))])


