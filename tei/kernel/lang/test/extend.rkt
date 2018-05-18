#lang ricoeur/tei/kernel

ƒ[#:spec this-spec
  #:extends custom-spec ;spec ;duplicate-spec ;; missing needs to be ok here
  #:with-local local-spec]

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



