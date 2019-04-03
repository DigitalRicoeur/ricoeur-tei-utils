#lang racket

(require ricoeur/tei/kernel
         ricoeur/tei/spec-lang/adt
         (submod ricoeur/tei/spec-lang/adt test)
         syntax/parse/define)


;(field/derived (applesauce "It's great!") resp #:check (λ (v) #t))

;(lift-property "a" "a")

#|
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
   (lift-property prop:procedure
                  (λ (this) (get-field resp this)))
   ]
  ;#:property prop:procedure (λ (this) (get-field resp this))
  #||#)
|#

(define-syntax-parser resp+proc
  [(_)
   #'(begin (define/field resp "macro-resp")
            (lift-property prop:procedure
                           (λ (this) (get-field resp this))))])

(define-element-struct [#:elements-only
                        #:element-name example
                        #:wrapped-constructor-name make-example]
  #:predicate tei-example?
  #:constructor
  [(define-fields
     [zap "ok"]
     [(resp #:accessor get-resp) "#ricoeur"])
   (define tmp 1)

   (resp+proc)
   
   ]
  ;#:property prop:procedure (λ (this) (get-field resp this))
  #||#)

(define it
  (make-example 'example null null))

(it)




