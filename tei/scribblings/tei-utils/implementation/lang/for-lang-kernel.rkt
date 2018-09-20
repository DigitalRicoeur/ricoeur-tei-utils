#lang at-exp racket

(require scribble/manual
         scribble/core
         syntax/parse/define
         (except-in "../../for-manual.rkt"
                    module+
                    module*
                    #%module-begin)
         )

(require-provide (provide-only "../../for-manual.rkt")
                 (for-label (except-in ricoeur/tei/kernel/lang/specification-lang
                                       #%module-begin)
                            (only-in racket/base #%module-begin)
                            (submod ricoeur/tei/kernel private)
                            (submod ricoeur/tei/kernel doc)
                            scribble/base
                            (except-in scribble/manual
                                       link)
                            scribble/decode
                            (only-in ricoeur/tei/kernel/lang/doc-lang
                                     define-element
                                     define-elements-together
                                     begin-for-runtime
                                     begin-for-test
                                     begin-for-runtime/derived
                                     )))
                 

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
