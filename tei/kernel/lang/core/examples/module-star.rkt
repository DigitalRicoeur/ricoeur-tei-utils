#lang ricoeur/tei/kernel/lang/core

ƒ(begin-for-runtime
   (define a 1)
   (module* private #f
     ;; it's trying to use the ricoeur-doc-lang module-begin
     ;; for the private submodule
     (provide a)))
