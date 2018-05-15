#lang racket/base

(provide (all-from-out 'typed)
         non-element-xexpr/c
         )

(module typed typed/racket/base
  (require racket/match
           racket/string
           )
  (provide non-element-xexpr->plain-text
           non-element-body->plain-text
           nondestructive-normalize-xexpr-body-once
           )
  (require/typed
   xml
   [#:opaque Location-Struct location?]
   [#:struct source ([start : (U Location-Struct
                                 Symbol
                                 False)]
                     [stop : (U Location-Struct
                                Symbol
                                False)])]
   [#:struct (cdata source) ([string : String])]
   [#:opaque Comment comment?]
   [#:opaque P-I p-i?]
   )

  (require/typed
   "entity-utils.rkt"
   [entity-symbol->char (-> Symbol Char)])

  (define-type Non-Element-Xexpr
    (U String Symbol Exact-Nonnegative-Integer
       cdata Comment P-I))

  (define-type (Xexpr-Element-Of A)
    (Pairof Symbol
            (U (Listof A)
               (Pairof (Listof (List Symbol String))
                       (Listof A)))))

  (define-type Xexpr
    (U Non-Element-Xexpr (Xexpr-Element-Of Xexpr)))

  (define-type Nondestructive-Normalized-Xexpr
    (U String Comment P-I (Xexpr-Element-Of Nondestructive-Normalized-Xexpr)))

  (define-type Normalized-Xexpr
    (U String (Xexpr-Element-Of Normalized-Xexpr)))

  (: non-element-xexpr->plain-text (-> Non-Element-Xexpr String))
  (define (non-element-xexpr->plain-text child)
    (cond
      [(string? child)
       child]
      [(or (comment? child)
           (p-i? child))
       ""]
      [(cdata? child)
       (cdata->plain-text child)]
      [(integer? child)
       (valid-char->plain-text child)]
      [else
       (string (entity-symbol->char child))]))

  (: non-element-body->plain-text (-> (Listof Non-Element-Xexpr) String))
  (define (non-element-body->plain-text body)
    (string-join (map non-element-xexpr->plain-text
                      body)
                 ""))

  (: cdata->plain-text (-> cdata String))
  (define (cdata->plain-text it)
    (match (cdata-string it)
      [(regexp #rx"<!\\[CDATA\\[(.*)\\]\\]>" (list _ content))
       (or content "")]
      [content content]))

  (: valid-char->plain-text (-> Exact-Nonnegative-Integer String))
  (define (valid-char->plain-text num)
    (string (integer->char num)))

  (: non-list? (-> Any Boolean :
                   #:+ (! (Listof Any))
                   #:- (Listof Any)))
  (define (non-list? x)
    (not (list? x)))

  (: nondestructive-normalize-xexpr-body-once
     (-> (Listof Xexpr)
         (Listof (U String Comment P-I (Xexpr-Element-Of Xexpr)))))
  (define (nondestructive-normalize-xexpr-body-once body)
    (match body
      ['() '()]
      [(cons this body)
       (cond
         [(or (list? this)
              (comment? this)
              (p-i? this))
          (cons this 
                (nondestructive-normalize-xexpr-body-once body))]
         [else
          (let normalize-textual-data ([l-str-so-far
                                        (list (non-element-xexpr->plain-text
                                               this))]
                                       [body body])
            (match body
              [(cons this body)
               #:when (not (or (list? this)
                               (comment? this)
                               (p-i? this)))
               (normalize-textual-data
                (cons (non-element-xexpr->plain-text this)
                      l-str-so-far)
                body)]
              [_
               (cons (string-join (reverse l-str-so-far) "")
                     (nondestructive-normalize-xexpr-body-once body))]))])]))
  #|END module typed|#)

(require 'typed
         racket/contract
         "entity-utils.rkt"
         xml
         )

(define non-element-xexpr/c
  (or/c string?
        entity-symbol/c
        valid-char?
        cdata?
        comment?
        p-i?))

