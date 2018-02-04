#lang racket

(require data/maybe
         json
         )

;; Should this be improved ? e.g. w/ more symbols?
;; If there were symbols, this could be in typed/racket

(provide location-stack-entry/c
         location-stack/c
         )

(module+ private
  (provide location-stack-jsexpr/c
           (contract-out
            [current-location-stack
             (parameter/c location-stack/c)]
            [jsexpr->location-stack
             (-> location-stack-jsexpr/c location-stack/c)]
            [location-stack->jsexpr
             (-> location-stack/c location-stack-jsexpr/c)]
            )))
             

(define location-stack-entry:div/c
  (list/c 'div
          (or/c "chapter" "part" "section" "dedication"
                "contents" "intro" "bibl" "ack" "index")
          (maybe/c string?)))

(define location-stack-entry:note/c
  (list/c 'note
          (or/c "foot" "end")
          string?
          (or/c "transl" #f)))

(define location-stack-entry/c
  (or/c 'front
        'back
        location-stack-entry:div/c
        location-stack-entry:note/c))

(define location-stack/c
  (opt/c
   (flat-murec-contract ([tail/c (or/c (list/c)
                                       (list/c 'front)
                                       (list/c 'back))]
                         [with-divs/c
                          (or/c tail/c
                                (cons/c location-stack-entry:div/c
                                        with-divs/c))]
                         [location-stack/c
                          (or/c with-divs/c
                                (cons/c location-stack-entry:note/c
                                        location-stack/c))])
                        location-stack/c)))

(define location-stack-jsexpr/c
  (opt/c
   (flat-murec-contract
    ([tail/c (or/c (list/c)
                   (list/c "front")
                   (list/c "back"))]
     [with-divs/c
      (or/c tail/c
            (cons/c (list/c "div"
                            (or/c "chapter" "part" "section" "dedication"
                                  "contents" "intro" "bibl" "ack" "index")
                            (or/c #f string?))
                    with-divs/c))]
     [location-stack-js/c
      (or/c with-divs/c
            (cons/c (list/c "note" (or/c "foot" "end") string? (or/c "transl" #f))
                    location-stack-js/c))])
    (and/c jsexpr? location-stack-js/c))))


(define current-location-stack
  (make-parameter '()))


(define (location-stack->jsexpr stack)
  (map (match-lambda
         ['front "front"]
         ['back "back"]
         [(list 'div type n)
          (list "div" type (from-just #f n))]
         [(list 'note place n transl)
          (list "note" place n transl)])
       stack))

(define (jsexpr->location-stack stack)
  (map (match-lambda
         ["front" 'front]
         ["back" 'back]
         [(list "div" type n)
          (list 'div type (false->maybe n))]
         [(list "note" place n transl)
          (list 'note place n transl)])
       stack))





