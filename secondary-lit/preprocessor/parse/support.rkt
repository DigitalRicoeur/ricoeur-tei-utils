#lang racket/base

(require racket/match
         racket/string
         net/url-string
         net/url-structs
         racket/contract)

(provide drop-non-elements
         element-xexpr-named?
         find-element-named
         extract-element-text
         apply-when
         element/mixed-body->string
         mixed-body->string
         (contract-out
          [prepare-href-for-html
           (-> (or/c #f string?)
               (or/c #f (and/c string? immutable?)))]
          ))

(define (prepare-href-for-html str)
  (and str
       (string->immutable-string
        (let ([u (string->url str)])
          (if (url-scheme u)
              str
              (url->string
               (struct-copy url u [scheme "http"])))))))

(define drop-non-elements
  (match-lambda
    [(cons this xs)
     #:when (not (pair? this))
     (drop-non-elements xs)]
    [xs
     xs]))

(define (element-xexpr-named? name v)
  (and (pair? v) (eq? name (car v))))

(define (find-element-named name xs)
  (for/first ([elem (in-list xs)]
              #:when (element-xexpr-named? name elem))
    elem))

(define (extract-element-text elem #:preserve-whitespace? [keep-ws? #f])
  (match-define `(,_ ,_ . ,body) elem)
  (string->immutable-string
   ((if keep-ws? values string-normalize-spaces)
    (string-append* body))))

(define (apply-when proc v)
  (and v (proc v)))

(define (element/mixed-body->string elem)
  (mixed-body->string (cddr elem)))

(define mixed-body->string
  (let ()
    (define (flatten-text/body body tail)
      (match body
        ['()
         tail]
        [(cons this body)
         (flatten-text/element this (flatten-text/body body tail))]))
    (define (flatten-text/element this tail)
      (match this
        [`(,_ ,_ . ,body)
         (flatten-text/body body tail)]
        [(? string? this)
         (cons this tail)]))
    (define (mixed-body->string body)
      (string->immutable-string
       (string-normalize-spaces
        (string-append*
         (flatten-text/body body null)))))
    mixed-body->string))


