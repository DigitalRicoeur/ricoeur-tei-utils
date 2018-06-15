#lang ricoeur/tei/kernel

ƒ;[#:spec this-spec]

ƒ(begin-for-runtime
   (require "test3.rkt"
            "test4.rkt"
            "test-duplicate.rkt"
            ricoeur/tei/kernel
            (submod ricoeur/tei/kernel private)
            ricoeur/tei/kernel/lang/link
            (submod ricoeur/tei/kernel/lang/link
                    private)
            ))

ƒ(begin-for-runtime
   (test))

