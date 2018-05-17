#lang info

(define raco-commands '(("tei"
                         (submod ricoeur/tei/tools/raco/raco-tei main)
                         "run Digital Ricoeur TEI commands"
                         50)
                        ))

;"DR Migration Assistant" is managed by "migration/info.rkt"
(define gracket-launcher-names
  '("TEI Lint"
    ))
(define gracket-launcher-libraries
  '("tei-lint/tei-lint.rkt"
    ))

(define racket-launcher-names
  (list "tei-guess-paragraphs"
        "encode-xml-entities"
        ))
(define racket-launcher-libraries
  (list "tei-guess-paragraphs.rkt"
        "encode-xml-entities.rkt"
        ))
