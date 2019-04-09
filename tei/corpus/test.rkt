#lang racket/base

(require "plain-corpus.rkt"
         racket/class)

(module+ test
  (require syntax/macro-testing
           rackunit)

  (check-not-exn (λ () 
                   (new ((corpus-mixin [] []
                           (super-new)
                           (super-docs)
                           (void (super-docs-evt)))
                         plain-corpus%)))
                 "all is good after super-new")

  (check-not-exn (λ () 
                   (new ((corpus-mixin [] []
                           (void (super-docs-evt))
                           (super-new))
                         plain-corpus%)))
                 "super-docs-evt ok before super-new")

  (check-exn exn:fail:contract:variable?
             (λ ()
               (new ((corpus-mixin [] []
                       (void (super-docs))
                       (super-new))
                     plain-corpus%)))
             "super-docs banned before super-new")

  (check-exn #rx"^super-docs: not allowed in a method body"
             (λ ()
               (convert-syntax-error
                (corpus-mixin [] []
                  (define/public (bad) (super-docs)))))
             "super-docs banned in methods")

  #|END module+ test|#)
