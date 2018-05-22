#lang racket

(require db/base
         )

(provide postgresql-data-source/c
         )

(define postgresql-data-source/c
  (make-flat-contract
   #:name 'postgresql-data-source/c
   #:first-order
   (match-lambda
     [(data-source 'postgresql
                   (list-no-order '#:database '#:user _ ...)
                   _)
      #t]
     [_ #f])
   #:late-neg-projection
   (λ (blame)
     (λ (val neg-party)
       (unless (data-source? val)
         (raise-blame-error 
          blame #:missing-party neg-party
          val
          '(expected: "data-source?" given: "~e")
          val))
       (match-define (data-source connector args extensions)
         val)
       (unless (eq? 'postgresql connector)
         (raise-blame-error
          (blame-add-context blame
                             "the data-source-connector of")
          #:missing-party neg-party
          val
          '(expected: "~e" given: "~e")
          'postgresql
          connector))
       (for ([kw (in-list '(#:database #:user))]
             #:unless (memq kw args))
         (raise-blame-error
          (blame-add-context blame
                             "the data-source-args of")
          #:missing-party neg-party
          val
          '(expected: "a list containing the keyword ~e"
            given: "~e")
          kw
          args))
       val))))