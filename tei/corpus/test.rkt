#lang racket

(require ricoeur/tei/corpus/plain-corpus)
#;
(new ((corpus-mixin [] []
        (super-new)
        (println (super-docs))
        (println (super-docs-evt)))
      plain-corpus%))

(new ((corpus-mixin [] []
        ;(println (super-docs-evt))
        (super-new))
      plain-corpus%))

(new ((corpus-mixin [] []
        (println (super-docs))
        (super-new))
      plain-corpus%))

(corpus-mixin [] []
  (define/public (bad) (super-docs)))