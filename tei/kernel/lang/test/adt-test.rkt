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
  [(define-fields
     [zap "ok"]
     [(resp #|#:check none/c|#) "#ricoeur"])
   (define tmp 1)
   (define list vector)
   (field list)
   ]
  #:property prop:procedure (λ (this) (get-field resp this))
  #||#)

(define it
  (make-example 'example null null))

(it)




