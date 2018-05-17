#lang ricoeur/tei/kernel

ƒ[#:spec this-spec]

ƒ(begin-for-runtime
   (require "test3.rkt"
         "test4.rkt"
         "test-duplicate.rkt"
         ricoeur/tei/kernel
         (submod ricoeur/tei/kernel private)
         ricoeur/tei/kernel/lang/define
         (submod ricoeur/tei/kernel/lang/define
                 private)
         ))

ƒ(extend-specifications custom-spec spec)

