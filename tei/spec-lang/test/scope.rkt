#lang ricoeur/tei/spec-lang

ƒ;[#:spec this-spec]

ƒ(begin-for-runtime
   (require "test3.rkt"
            "test4.rkt"
            "test-duplicate.rkt"
            ricoeur/tei/kernel
            (submod ricoeur/tei/kernel private)
            ricoeur/tei/spec-lang/link
            (submod ricoeur/tei/spec-lang/link
                    private)))

ƒ(begin-for-runtime
   (test))

