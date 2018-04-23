#lang at-exp racket

(require scribble/manual)

(provide guidelines-doc
         guidelines-secref
         )

(define guidelines
  '(lib "ricoeur/tei/scribblings/guidelines/guidelines.scrbl"))

(define (guidelines-doc)
  @other-doc[guidelines])

(define (guidelines-secref sec)
  @secref[sec #:doc guidelines])
