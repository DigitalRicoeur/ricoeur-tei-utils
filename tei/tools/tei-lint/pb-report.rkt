#lang racket

(require ricoeur/tei/base)

(module+ test
  (require rackunit))

(provide (contract-out
          [tei-document-pb-report
           (-> tei-document?
               pb-report?)]
          [struct pb-report
            ([messages (listof string?)]
             [warning? boolean?])
            #:omit-constructor]
          ))

(define (tei-document-pb-report doc)
  (analyze-page-breaks
   (tei-get-page-breaks doc)))

#|
Page break warnings:
  - there must be some pb elements
  - after the first pb with an arabic numeral,
    anything but an arabic numeral is an error
  - if there is an n value, it must be either a roman
    or an arabic numeral
  - missing or out-of-order page numbers (roman or arabic)
    trigger a warning
|#

(struct pb-report (messages warning?)
  #:transparent)

(define-struct/contract pb-group ([kind (or/c 'none 'other 'number 'roman)]
                                  [lst (non-empty-listof tei-pb?)])
  #:transparent)

(define/contract (analyze-page-breaks lst)
  (-> (listof tei-pb?) pb-report?)
  (define l-groups
    (group-page-breaks lst))
  (define l-kinds
    (map pb-group-kind l-groups))
  (pb-report
   (map pb-group->string l-groups)
   (any->boolean
    (or
     ;; warning if there isn't at least one pb element
     (null? l-kinds)
     ;; warning if any n value can't be parsed
     (memq 'other l-kinds)
     ;; warning for missing or out-of-order page roman numerals
     ;; (there should be 0 or 1 'roman groups)
     (case (length (filter (λ (k) (eq? 'roman k)) l-kinds))
       [(0 1) #f]
       [else #t])
     ;; warning for missing or out-of-order page arabic numerals
     ;; also warning if anything other than arabic numerals comes after
     (match (memq 'number l-kinds)
       [(or #f '(number)) #f]
       [_ #t])))))

(TODO/void analyze-page-breaks: Add tests)
             
(define/contract (group-page-breaks lst)
  (-> (listof tei-pb?) (listof pb-group?))
  (match lst
    ['() null]
    [(cons this lst)
     (define this-kind
       (pb-get-kind this))
     (define-values {like-this more}
       (splitf-at lst
                  (λ (it)
                    (eq? this-kind (pb-get-kind it)))))
     (append (make-pb-groups this-kind (cons this like-this))
             (group-page-breaks more))]))


(define/contract (make-pb-groups kind lst) ; lst is a non-empty list
  (-> (or/c 'none 'other 'number 'roman)
      (non-empty-listof tei-pb?)
      (non-empty-listof pb-group?))
  (case kind
    [(roman number)
     (group-sequential-pb-numbers kind lst)]
    [else
     (list (pb-group kind lst))]))


(define/contract (group-sequential-pb-numbers kind lst)
  (-> (or/c 'number 'roman)
      (non-empty-listof tei-pb?)
      (non-empty-listof pb-group?))
  ;; lst is a non-empty list externally; null for base case internally
  (match lst
    ['() null]
    [(cons this lst)
     (let loop ([lst lst]
                [group-so-far (list this)] 
                [expect-num (add1 (from-just! (pb-get-numeric this)))])
       (match lst
         [(cons this lst)
          #:when (= expect-num (from-just! (pb-get-numeric this)))
          (loop lst
                (cons this group-so-far)
                (add1 expect-num))]
         [lst
          (cons (pb-group kind (reverse group-so-far))
                (group-sequential-pb-numbers kind lst))]))]))

                                                                 
;                            ;;                ;                   
;           ;                ;;                ;;                  
;            ;        ;;   ;;;;;;;  ;; ;;;  ;;;;;   ;; ;      ;;;;;
;             ;;    ;;  ;    ;;     ;;;        ;;   ;;; ;    ;  ;  
;   ;;;;;;      ;    ;       ;;     ;;         ;;   ;;  ;;  ;;  ;; 
;               ;;    ;;     ;;     ;;         ;;   ;;  ;;   ;  ;  
;             ;;        ;;   ;;     ;;         ;;   ;;  ;;    ;;   
;            ;      ;   ;     ;     ;;         ;;   ;;  ;;  ;;     
;           ;        ;;;       ;;;  ;;         ;;   ;;  ;;   ;;;;; 
;                                                           ;    ;;
;                                                          ;;    ; 
;                                                            ;;;;  
;                                                                  
      
(define pb-group->string
  (match-lambda
    ;; 'none
    [(pb-group 'none lst)
     (define len
       (length lst))
     (format "~a non-numbered page~a"
             len
             (if (= 1 len) "" "s"))]
    ;; 'other
    [(pb-group 'other (list (app pb-get-page-string
                                 (just unparsed-str))))
     (format "1 page with the unreadable number ~v"
             unparsed-str)]
    [(pb-group 'other lst)
     (format "~a pages with unreadable numbers"
             (length lst))]
    ;; 'number 'roman
    [(pb-group (and kind (or 'number 'roman))
               (list (app pb-get-page-string
                          (just unparsed-str))))
     (format (case kind
               [(roman) "1 page with Roman numeral ~v"]
               [else "1 page numbered ~v"])
             unparsed-str)]
    [(pb-group (and kind (or 'number 'roman)) lst)
     (format (case kind
               [(roman) "~a pages with Roman numerals from ~v to ~v"]
               [else "~a pages numbered from ~v to ~v"])
             (length lst)
             (from-just! (pb-get-page-string (first lst)))
             (from-just! (pb-get-page-string (last lst))))]))

(module+ test
  (define (xs->pb-group xs)
    (match (group-page-breaks (map xexpr->element xs))
      [(list it) it]))
  (check-equal? (pb-group->string
                 (xs->pb-group '{(pb)}))
                "1 non-numbered page"
                "pb-group->string: 'none ; singular")
  (check-equal? (pb-group->string
                 (xs->pb-group '{(pb)(pb)(pb)}))
                "3 non-numbered pages"
                "pb-group->string: 'none ; plural")
  (check-equal? (pb-group->string
                 (xs->pb-group '{(pb ([n "*"]))}))
                "1 page with the unreadable number \"*\""
                "pb-group->string: 'other ; singular")
  (check-equal? (pb-group->string
                 (xs->pb-group '{(pb ([n "*"]))
                                 (pb ([n "foo"]))
                                 (pb ([n "l3"]))}))
                "3 pages with unreadable numbers"
                "pb-group->string: 'other ; plural")
  (check-equal? (pb-group->string
                 (xs->pb-group '{(pb ([n "x"]))}))
                "1 page with Roman numeral \"x\""
                "pb-group->string: 'roman ; singular")
  (check-equal? (pb-group->string
                 (xs->pb-group '{(pb ([n "II"]))
                                 (pb ([n "III"]))
                                 (pb ([n "IV"]))
                                 (pb ([n "V"]))}))
                "4 pages with Roman numerals from \"II\" to \"V\""
                "pb-group->string: 'roman ; plural")
  (check-equal? (pb-group->string
                 (xs->pb-group '{(pb ([n "42"]))}))
                "1 page numbered \"42\""
                "pb-group->string: 'number ; singular")
  (check-equal? (pb-group->string
                 (xs->pb-group '{(pb ([n "9"]))
                                 (pb ([n "10"]))}))
                "2 pages numbered from \"9\" to \"10\""
                "pb-group->string: 'number ; plural")
  #|END module+ test|#)


