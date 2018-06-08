#lang racket/base

(require data/maybe
         racket/contract
         racket/match
         json
         ricoeur/tei/base/def-from-spec
         )

(provide location-stack-entry?
         (contract-out
          [location-stack->strings
           (-> location-stack-entry?
               (listof string?))]
          ))

(module+ private
  (provide location-stack-jsexpr/c
           location-stack-entry:div
           location-stack-entry:div?
           location-stack-entry:div-type
           location-stack-entry:div-n
           location-stack-entry:div-rest
           location-stack-entry:note
           location-stack-entry:note?
           location-stack-entry:note-place
           location-stack-entry:note-n
           location-stack-entry:note-transl?
           location-stack-entry:note-rest
           (contract-out
            [current-location-stack
             (parameter/c (or/c #f location-stack-entry?))]
            [jsexpr->location-stack
             (-> location-stack-jsexpr/c location-stack-entry?)]
            [location-stack->jsexpr
             (-> location-stack-entry? location-stack-jsexpr/c)]
            )))

(module adt racket/base
  (require data/maybe
           racket/contract
           ricoeur/tei/base/def-from-spec
           )
  (provide location-stack-entry?
           location-stack-entry:root?
           (contract-out
            [struct location-stack-entry:div
              ([type div-type/c]
               [n (maybe/c string?)]
               [rest (or/c location-stack-entry:div?
                           location-stack-entry:root?)])]
            [struct location-stack-entry:note
              ([place (or/c 'foot 'end)]
               [n string?]
               [transl? (or/c #f 'transl)]
               [rest (or/c location-stack-entry:root? ;; <note> tags might be added before <div> tags
                           location-stack-entry:note? 
                           location-stack-entry:div?)])]
            ))
                                               
  (define location-stack-entry:root?
    (or/c 'front 'back 'body))

  (struct location-stack-entry:div (type n rest)
    #:transparent)

  (struct location-stack-entry:note (place n transl? rest)
    #:transparent)
                    
  (define location-stack-entry?
    (or/c location-stack-entry:root?
          location-stack-entry:div?
          location-stack-entry:note?))
  #|END module adt|#)
(require 'adt)


(define current-location-stack
  (make-parameter #f))

;; consider opt/c when
;; https://github.com/racket/racket/commit/8393f0b2f6a650ffe3ebf20a20883ca5af879ae7
;; is live (Racket 7)

(define location-stack-jsexpr:root/c
  (or/c "front" "body" "back"))

(define location-stack-jsexpr:div/c
  (and/c jsexpr?
         (flat-rec-contract location-stack-jsexpr:div/c
           (list/c "div"
                   div-type-string/c
                   (or/c #f string?)
                   (or/c location-stack-jsexpr:root/c
                         location-stack-jsexpr:div/c)))))

(define location-stack-jsexpr:note/c
  (and/c jsexpr?
         (flat-rec-contract location-stack-jsexpr:note/c
           (list/c "note"
                   (or/c "foot" "end")
                   string?
                   (or/c "transl" #f)
                   (or/c location-stack-jsexpr:root/c
                         location-stack-jsexpr:note/c ;; ? do we really want to support nested notes ?
                         location-stack-jsexpr:div/c)))))

(define location-stack-jsexpr/c
  (or/c location-stack-jsexpr:root/c
        location-stack-jsexpr:div/c
        location-stack-jsexpr:note/c))


(define location-stack->jsexpr
  (match-lambda
    ['front "front"]
    ['back "back"]
    ['body "body"]
    [(location-stack-entry:div type n rest)
     (list "div"
           (symbol->string type)
           (from-just #f n)
           (location-stack->jsexpr rest))]
    [(location-stack-entry:note place n transl? rest)
     (list "note"
           (symbol->string place)
           n
           (and transl? "transl")
           (location-stack->jsexpr rest))]))

(define jsexpr->location-stack
  (match-lambda
    ["front" 'front]
    ["back" 'back]
    ["body" 'body]
    [(list "div" type n rest)
     (location-stack-entry:div (string->symbol type)
                               (false->maybe n)
                               (jsexpr->location-stack rest))]
    [(list "note" place n transl? rest)
     (location-stack-entry:note (string->symbol place)
                                n
                                (and transl? 'transl)
                                (jsexpr->location-stack rest))]))


(define (location-stack->strings stk)
  (reverse
   (let to-strings ([stk stk])
     (match stk
       ['body null]
       ['front '("Front-matter")]
       ['back '("Back-matter")]
       [(location-stack-entry:div type n rest)
        (cons (string-append (case type
                               [(chapter) "Chapter"]
                               [(part) "Part"]
                               [(section) "Section"]
                               [(dedication) "Dedication"]
                               [(contents) "Table of Contents"]
                               [(intro) "Introduction"]
                               [(bibl) "Bibliography"]
                               [(ack) "Acknowledgements"]
                               [(index) "Index"]
                               [else (error 'location-stack->string
                                            (format "unknown div type ~e"
                                                    type))])
                             (maybe ""
                                    (λ (n) (string-append " " n))
                                    n))
              (to-strings rest))]
       [(location-stack-entry:note place n transl? rest)
        (cons (string-append (if transl?
                                 "Translator's "
                                 "")
                             (case place
                               [(foot) "Footnote "]
                               [(end) "Endnote "]
                               [else (error 'location-stack->string
                                            (format "unknown note place ~e"
                                                    place))])
                             n)
              (to-strings rest))]))))


