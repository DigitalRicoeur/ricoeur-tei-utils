#lang racket/base

(require "support.rkt"
         "nanopass.rkt"
         "common.rkt"
         (only-in ricoeur/kernel/utils
                  attributes-ref)
         racket/match
         racket/list
         racket/contract
         "../types.rkt")

(provide (contract-out
          [parse-book-forest
           (-> list? parsed-book?)]
          ))

(define (parse-book-forest bk-forest)
  ;; TODO collection-meta
  (define whole-book-info
    (match (find-element-named 'book-meta bk-forest)
      ;; JSTOR says it will be present
      [`(book-meta ,_ . ,forest)
       (parse-whole-book-meta forest)]))
  (make-parsed-book whole-book-info
                    (find/parse-book-parts bk-forest)))


(define find/parse-book-parts
  (let ()
    ;; FIXME: It would be better to let the structure of
    ;; the schema guide us, but it is annoyingly inconsistent.
    (define (look-everywhere-for-book-part-meta forest)
      (let loop ([forest forest]
                 [acc null])
        (match forest
          ['()
           acc]
          [(cons `(book-part-meta ,_ . ,inside)
                 forest)
           (loop forest (cons (parse-book-part-meta inside) acc))]
          [(cons `(,_ ,_ . ,inside) forest)
           (loop forest (loop inside acc))]
          [(cons (? string?) forest)
           (loop forest acc)])))
    (define (find/parse-book-parts bk-forest)
      (look-everywhere-for-book-part-meta
       (match (or (find-element-named 'book-body bk-forest)
                  (find-element-named 'body bk-forest))
         ;; JSTOR says 'book-body will be present
         ;; but sometimes it is actually 'body
         [(or `(book-body ,_ . ,forest)
              `(body ,_ . ,forest))
          forest])))
    find/parse-book-parts))


(define (parse-book-part-meta forest)
  (make-parsed-book-part
   #:contribs (find/parse-contribs forest)
   #:abstract (find/parse-abstract forest)
   #:title (match (find-element-named 'title-group forest)
             [`(title-group ,_ . ,forest)
              (parse-book-part-title forest)]
             [_
              #f])
   #:jstor-id (for/first ([elem (in-list forest)]
                          #:when (and (element-xexpr-named? 'book-part-id elem)
                                      (equal? "jstor"
                                              (attributes-ref (cadr elem)
                                                              'book-part-id-type))))
                (extract-element-text elem))
   #:page-spec (cond
                 [(find-element-named 'fpage forest)
                  =>
                  (λ (fpage-elem)
                    (define fpage (extract-element-text fpage-elem))
                    (cond
                      [(find-element-named 'lpage forest)
                       => (λ (lpage-elem)
                            (cons fpage (extract-element-text lpage-elem)))]
                      [else
                       fpage]))]
                 [else
                  #f])))


(define (parse-book-part-title forest)
  ;; different than the others b/c label
  (define label
    (match (find-element-named 'label forest)
      [`(label ,attrs . ,body)
       #:when (pair? body)
       (parse-book-part-label (attributes-ref attrs 'alt) ;; for screen readers
                              body)]
      [_
       #f]))
  (define title
    (apply-when (λ (e) (parse-phrasing-content (cddr e)))
                (find-element-named 'title forest)))
  (define subtitles
    (filter-map (match-lambda
                  [`(subtitle ,_ . ,body)
                   #:when (pair? body)
                   (parse-phrasing-content body)]
                  [_
                   #f])
                forest))
  (list label title subtitles))

;                                  
;                                  
;            ;;            ;;      
;            ;;            ;;      
;  ;   ;   ; ;;;;;   ;;;   ;;  ;;  
;  ;  ; ;  ; ;;  ;  ;   ;  ;; ;  ; 
;   ; ; ; ;  ;;  ;; ;   ;  ;; ;  ; 
;   ; ; ; ;  ;;  ;;;;   ;; ;;;;;;;;
;   ; ; ; ;  ;;  ;; ;   ;  ;; ;    
;   ; ; ; ;  ;;  ;; ;   ;  ;; ;    
;    ;   ;   ;;  ;;  ;;;    ;  ;;; 
;                                  
;                                  

(define (parse-whole-book-meta forest)
  ;; JSTOR says these are reliably present:
  ;;   - book-id@book-id-type="jstor"
  ;;   - subject-group containing subject elements
  ;;     with @content-type including '(call-number lcsh discipline)
  ;;   - book-title-group containing book-title
  ;;   - contrib-group containing contrib, often w/ name elements
  ;;   - pub-date
  ;;   - isbn 1+
  ;;   - publisher w/ publisher-name & publisher-loc
  ;;   - permissions
  ;;   - self-uri
  ;;   - counts
  ;;   - custom-meta-group "for languages present"
  ;; Also interesting
  ;;   - book-id
  ;;   - abstract
  (define-values [title subtitles]
    (find/parse-book-title forest))
  (make-parsed-whole-book-meta
   #:main-title title
   #:subtitles subtitles
   #:contribs (find/parse-contribs forest)
   #:publishers (find/parse-book-publishers forest)
   #:pub-dates (find/parse-pub-dates forest)
   #:self-uri (attributes-ref (cadr (find-element-named 'self-uri forest))
                              'xlink:href)))



(define find/parse-book-publishers
  (let ()
    (define (find-inside forest)
      (match (drop-non-elements forest)
        ['()
         '()]
        [(cons `(publisher-name ,_ . ,(app parse-phrasing-content name))
               (app drop-non-elements forest))
         (let-values ([{maybe-location forest}
                       (match forest
                         [(cons `(publisher-loc ,_ . ,body)
                                forest)
                          (values (and (pair? body)
                                       (parse-phrasing-content body))
                                  forest)]
                         [_
                          (values #f forest)])])
           (cons (cons name maybe-location)
                 (find-inside forest)))]))
    (define (find/parse-book-publishers forest)
      (append-map (match-lambda
                    [`(publisher ,_ . ,forest)
                     (find-inside forest)]
                    [_
                     '()])
                  forest))
    find/parse-book-publishers))



(define (find/parse-book-title outer-forest)
  (match-define `(book-title-group ,_ . ,forest)
    (find-element-named 'book-title-group outer-forest))
  (values (parse-phrasing-content
           (cddr
            ;; JSTOR says always present
            (find-element-named 'book-title forest)))
          (filter-map (match-lambda
                        [`(subtitle ,_ . ,body)
                         #:when (pair? body)
                         (parse-phrasing-content body)]
                        [_
                         #f])
                      forest)))

