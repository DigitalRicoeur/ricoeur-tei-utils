#lang ricoeur/core-doc-lang

ƒ(begin-for-runtime
   (define a 1)
   (module+ private
     (provide a)))
