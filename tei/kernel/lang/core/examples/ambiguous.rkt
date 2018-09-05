#lang ricoeur/tei/kernel/lang/core

ƒ(begin-for-runtime
   (define a 1)
   (module+ private
     (provide a)))
