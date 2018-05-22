#lang racket

(module server racket
  (require ricoeur/tei/kernel/substruct-contracts)
  (provide (contract-out
            [struct:base (substruct-of/c
                          base
                          [num number?])]
            ))
  (struct base (num)
    #:transparent))

(require 'server)

(struct derived ()
  #:super struct:base
  #:transparent)

(derived "not a number") ;; blames server
