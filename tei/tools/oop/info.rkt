#lang info
#|
(define raco-commands '(("teiOOP"
                         (submod ricoeur/tei/tools/oop/raco/raco-tei main)
                         "run Digital Ricoeur TEI commands [deprecated version]"
                         50)
                        ))


(define gracket-launcher-names
  '("TEI Lint OOP"
    ))
(define gracket-launcher-libraries
  '("tei-lint/tei-lint.rkt"
    ))

(define racket-launcher-names
  (list "tei-guess-paragraphsOOP"
        ))
(define racket-launcher-libraries
  (list "tei-guess-paragraphs.rkt"
        ))
|#
