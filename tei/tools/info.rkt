#lang info

(define raco-commands '(("ricoeur/tei"
                         (submod ricoeur/tei/tools/raco/raco main)
                         "run Digital Ricoeur TEI commands"
                         50)
                        ))


(define gracket-launcher-names
  '("TEI Lint"
    ))
(define gracket-launcher-libraries
  '("tei-lint/tei-lint.rkt"
    ))
