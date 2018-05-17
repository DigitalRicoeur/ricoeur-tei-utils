#lang racket

(require ricoeur/tei/kernel
         ricoeur/tei/kernel/lang/adt
         (submod ricoeur/tei/kernel/lang/adt test)
         )


;(field/derived (applesauce "It's great!") resp #:check (λ (v) #t))

;(lift-property "a" "a")


(define-element-struct [#:elements-only
                        #:element-name example
                        #:wrapped-constructor-name make-example]
  #:predicate tei-example?
  #:constructor
  [#;(define-fields
     [zap "ok"]
     [(resp #:check (λ (v) #t)) "#ricoeur"])
   (define tmp 1)
   ]
  #||#)

(make-example 'example null null)
