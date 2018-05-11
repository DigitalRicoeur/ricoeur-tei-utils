#lang racket/base

(provide color-lexer
         )

(require syntax-color/scribble-lexer
         )

(define color-lexer
  (make-scribble-inside-lexer #:command-char #\ƒ))


