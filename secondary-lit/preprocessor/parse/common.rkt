#lang racket/base

(require "support.rkt"
         "nanopass.rkt"
         "../types.rkt"
         (only-in ricoeur/kernel/utils
                  attributes-ref)
         (only-in gregor day-of-month/c)
         (only-in adjutor list-when)
         racket/contract
         racket/match
         racket/list
         racket/dict
         syntax/parse/define
         (for-syntax racket/base))

(provide find/parse-pub-dates
         find/parse-contribs
         find/parse-abstract)

(define (find/parse-pub-dates forest)
  ;; http://jats.nlm.nih.gov/archiving/tag-library/1.2/element/pub-date.html
  (for/list ([elem (in-list forest)]
             #:when (element-xexpr-named? 'pub-date elem))
    (match-define `(pub-date ,attrs . ,body) elem)
    (define (coerce-int str #:field field)
      (define n (string->number str))
      (if (exact-positive-integer? n)
          n
          (raise-arguments-error
           'find/parse-pub-dates
           "could not convert string to exact positive integer"
           "given" str
           "field" field
           "element" elem)))
    (define assocs
      (filter-map
       (match-lambda
         [(and `(day ,_ . ,_)
               (app extract-element-text str))
          ;; schema guarantees numeric 2-digit
          (cons 'day (coerce-int str #:field 'day))]
         [(and `(month ,_ . ,_)
               (app extract-element-text str))
          ;; schema says numeric best practice, but not required
          (cons 'month (parse-month-string str))]
         [(and `(year ,_ . ,_)
               (app extract-element-text str))
          ;; schema says "should be" numeric "when possible"
          (cons 'year (coerce-int str #:field 'year))]
         [(and `(season ,_ . ,_)
               (app extract-element-text str))
          ;; apparently common
          (cons 'season str)]
         [`(string-date ,_ . ,(app mixed-body->string str))
          (cons 'string-date str)]
         [`(era ,_ . ,_)
          ;; e.g. "昭和" (i.e. Heisei 25)
          ;; skip for now
          #f]
         [_
          #f])
       body))
    (define ymd-from-iso8601
      (apply-when iso8601->ymd
                  (attributes-ref attrs 'iso-8601-date)))
    (define ymd-from-assocs
      (match* {(dict-ref assocs 'year #f)
               (dict-ref assocs 'month #f)
               (dict-ref assocs 'day #f)}
        ;; sadly, some of these are nonsense
        [{y m d}
         #:when (and y m d)
         (list y m d)]
        [{y m _}
         #:when (and y m)
         (list y m)]
        [{y _ _}
         #:when y
         (list y)]
        [{#f _ _}
         #f]))
    (when (and ymd-from-iso8601
               ymd-from-assocs)
      ;; ok for ymd-from-iso8601 to be more specific
      (unless (for/and ([a (in-list ymd-from-iso8601)]
                        [b (in-list ymd-from-assocs)])
                (eqv? a b))
        (raise-arguments-error
         'find/parse-pub-dates
         "inconsistent ymd values"
         "from iso8601" ymd-from-iso8601
         "from assocs" ymd-from-assocs
         "element" elem)))
    (define ymd
      (or ymd-from-iso8601 ;; prefer this one
          ymd-from-assocs
          null))
    (unless (null? ymd)
      (define (error! what expected given)
        (raise-arguments-error 'find/parse-pub-dates
                               (string-append "invalid " what)
                               "expected" expected
                               "given" given
                               "element" elem))
      (match-define (cons y md) ymd)
      (unless (exact-positive-integer? y)
        (error! "year"
                (unquoted-printing-string "exact-positive-integer?")
                y))
      (unless (null? md)
        (match-define (cons m ?d) md)
        (unless (month/c m)
          (error! "month" month/c m))
        (unless (null? ?d)
          (match-define (list d) ?d)
          (define day/c (day-of-month/c y m))
          (unless (day/c d)
            (error! "day" day/c d)))))
    (make-parsed-pub-date
     #:ymd ymd
     #:season (dict-ref assocs 'season #f)
     #:string-date (dict-ref assocs 'string-date #f)
     #:publication-format
     (apply-when string->symbol
                 (attributes-ref attrs 'publication-format))
     #:publication-event
     (apply-when string->symbol
                 (attributes-ref attrs 'date-type)))))

(define/final-prop month/c
  (integer-in 1 12))
      
(define iso8601->ymd
  ;; don't use iso8601->date b/c it defaults absent fields to 1
  (match-lambda
    [(pregexp #px"^(\\d{4})(?:-(\\d{2}))?(?:-(\\d{2}))?$"
              (list _ y ?m ?d))
     (cons (string->number y)
           (list-when ?m
             (cons (string->number ?m)
                   (list-when ?d
                     (list (string->number ?d))))))]))
        


(define parse-month-string
  (let ()
    (define-simple-macro (match-lambda/month-string)
      #:with ([pattern:regexp i:exact-positive-integer] ...+)
      (for/list ([i (in-naturals 1)]
                 [spec (in-list '([jan uary]
                                  [feb ruary]
                                  [mar ch]
                                  [apr il]
                                  [may #f]
                                  [jun e]
                                  [jul y]
                                  [aug ust]
                                  [sep tember]
                                  [oct ober]
                                  [nov ember]
                                  [dec ember]))])
        (define base (car spec))
        (define suffix (cadr spec))
        (list (pregexp (format "^\\s*(?i:~a~a)\\s*$"
                               base
                               (if suffix
                                   (format "(?:~a)?" suffix)
                                   "")))
              i))
      (let ([parse-month-string
             (match-lambda
               [(app string->number n)
                #:when (and n (<= 1 n 12))
                n]
               [(pregexp 'pattern)
                'i]
               ...)])
        parse-month-string))
  (match-lambda/month-string)))

;                                                                                            
;                       ;;       ;; ;;         
;                       ;;          ;;         
;    ;;;   ;;;   ; ;;; ;;;;;;; ; ;; ;;;;    ;; 
;   ;   ; ;   ;  ;;  ;  ;;  ;;;  ;; ;;  ; ;;  ;
;   ;     ;   ;  ;;  ;; ;;  ;;   ;; ;;  ;  ;   
;  ;;    ;;   ;; ;;  ;; ;;  ;;   ;; ;;  ;;  ;; 
;   ;     ;   ;  ;;  ;; ;;  ;;   ;; ;;  ;     ;
;   ;   ; ;   ;  ;;  ;;  ;  ;;   ;; ;;  ; ;   ;
;    ;;;   ;;;   ;;  ;;  ;;;;;   ;; ; ;;   ;;; 
;                                              
;                                              

(define (find/parse-contribs forest)
  ;; shared for both book & article
  (for*/list ([group (in-list forest)]
              #:when (element-xexpr-named? 'contrib-group group)
              [contrib-elem (in-list (cddr group))]
              #:when (element-xexpr-named? 'contrib contrib-elem))
    (parse-contrib-elem contrib-elem)))

(define (get-name-style attrs)
  ;; http://jats.nlm.nih.gov/extensions/bits/tag-library/2.0/attribute/name-style.html
  ;; '(eastern given-only islensk western) default: western
  (match (attributes-ref attrs 'name-style)
    [#f
     ;; 'western is default, but we want to recognize this case
     #f]
    ["western"
     'western]
    ["eastern"
     'eastern]
    ["given-only"
     'given-only]
    ["islensk"
     'islensk]))

(define (parse-contrib-elem contrib-elem)
  (match-define `(contrib ,attrs . ,contrib-body) contrib-elem)
  (define contribution-type
    ;; not controlled vocab, but examples include
    ;; '(author editor translator)
    (apply-when string->symbol
                (attributes-ref attrs 'contrib-type)))
  (match (or (find-element-named 'name contrib-body)
             (find-element-named 'string-name contrib-body)
             (find-element-named 'collab contrib-body))
    [`(collab ,_ . ,body)
     #:when (pair? body)
     (make-collab-contrib #:contribution-type contribution-type
                          (parse-phrasing-content body))]
    [(list-rest (and (or 'name 'string-name) elem-format)
                (app get-name-style name-style)
                body)
     (define (find/parse-children kind)
       (match (filter-map (match-lambda
                            [(list-rest n _ body)
                             #:when (and (eq? n kind) (pair? body))
                             (parse-phrasing-content body)]
                            [_
                             #f])
                          body)
         ['()
          #f]
         [(list parsed)
          parsed]))
     (define prefix* (find/parse-children 'prefix))
     (define suffix* (find/parse-children 'suffix))
     (define surname* (find/parse-children 'surname))
     (define given* (find/parse-children 'given-names))
     (match* {name-style given* surname*}
       [{_ #f #f}
        #:when (eq? 'string-name elem-format)
        (make-unstructured-contrib
         #:contribution-type contribution-type
         (mixed-body->string body))]
       [{(or #f 'given-only) (not #f) #f}
        (make-given-only-contrib #:contribution-type contribution-type
                                 #:prefix prefix*
                                 #:suffix suffix*
                                 #:given-names given*)]
       [{(or #f 'western 'eastern 'islensk)
         _
         (not #f)}
        (make-surname-contrib
         #:contribution-type contribution-type
         #:style (or name-style 'western)
         #:prefix prefix*
         #:given-names given*
         #:surname surname*
         #:suffix suffix*)])]
    [#f
     (raise-argument-error
      'parse-contrib-elem
      "a name, string-name, or collab element in the body"
      contrib-elem)]))

;                                               
;                                               
;          ;;          ;;                    ;; 
;          ;;          ;;                    ;; 
;    ;;    ;;;;    ;; ;;;;;;; ;  ;;     ;;; ;;;;
;   ;  ;   ;;  ; ;;  ; ;;  ;;;  ;  ;   ;   ; ;; 
;      ;;  ;;  ;  ;    ;;  ;;      ;;  ;     ;; 
;    ;;;;  ;;  ;;  ;;  ;;  ;;    ;;;; ;;     ;; 
;   ;  ;;  ;;  ;     ;;;;  ;;   ;  ;;  ;     ;; 
;  ;;  ;;  ;;  ; ;   ;  ;  ;;  ;;  ;;  ;   ;  ; 
;   ;;; ;  ; ;;   ;;;   ;;;;;   ;;; ;   ;;;   ;;
;                                               
;                                               

(define (find/parse-abstract forest)
  ;; can have many: see @specific-use and @abstract-type
  ;; http://jats.nlm.nih.gov/archiving/tag-library/1.2/element/abstract.html
  ;;;;;;
  ;; JSTOR says for book-part:
  ;;   The abstract-type attribute, when present, designates
  ;;   what kind of text is in the abstract.
  ;;   "Extract" means that the text is just the first 100 words.
  ;;;;;;
  ;; In practice:
  ;;   - Nothing has an abstract-type.
  ;;   - There is only 1 journal article with more than 1
  ;;     abstract: it has one with xml:lang="fre" [sic]
  ;;     and one with xml:lang="eng".
  ;;     No other abstracts have an xml:lang attribute.
  ;; 0-1 label
  ;; 0-1 title
  ;; 0+ p
  ;; where p, label, and title can contain any of
  ;;   '(bold italic sc sup xref)
  (for/first ([elem (in-list forest)]
              #:when
              (match elem
                [(list* 'abstract
                        (app (λ (v) (attributes-ref v 'xml:lang))
                             (or #f "eng"))
                        _)
                 #true]
                [_
                 #false]))
    (parse-abstract elem)))

