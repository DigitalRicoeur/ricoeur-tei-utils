#lang at-exp racket

(require scribble/manual
         scribble/core
         syntax/parse/define
         (except-in "../../for-manual.rkt"
                    #%module-begin)
         )

(require-provide (provide-only "../../for-manual.rkt")
                 (for-label ricoeur/tei/kernel/lang/specification-lang
                            (submod ricoeur/tei/kernel private)
                            (submod ricoeur/tei/kernel doc)
                            scribble/base
                            (except-in scribble/manual
                                       link)
                            ))
                 

(provide spec-lang-mod
         hash-lang-kernel
         deftogether/indent
         )


(define (spec-lang-mod)
  (racketmodlink ricoeur/tei/kernel/lang/specification-lang
                 (racketmodfont "ricoeur/tei/kernel")))

(define (hash-lang-kernel)
  @elem{@(hash-lang) @(spec-lang-mod)})


(define leftindent-style
  ; what defsubform does
  (style "leftindent" '()))

(define-syntax-parser deftogether/indent
  [(_ body ...)
   #`(nested #:style leftindent-style
             (deftogether body ...))])
