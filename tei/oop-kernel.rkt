#lang racket/base

(require adjutor)

(require-provide "kernel/xmllint.rkt"
                 "kernel/pre-kernel-lib.rkt"
                 "kernel/xexpr/plain-contracts.rkt"
                 "kernel/xexpr/normalize.rkt"
                 (only-in "base/def-from-spec.rkt"
                          tei-xexpr/c
                          static-tei-xexpr/c
                          any-tei-xexpr/c
                          tei-element-name/c
                          ))
