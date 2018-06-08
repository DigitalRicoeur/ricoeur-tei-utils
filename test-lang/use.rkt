#lang ricoeur/test-lang/runtime-lang

(+ 1 2)

;(show 'a)

(begin-for-runtime
  (show 'b)
  (+ 3 4))
