#lang racket/base

(require "support.rkt"
         "common.rkt"
         "nanopass.rkt"
         (only-in ricoeur/kernel/utils
                  attributes-ref)
         racket/match
         racket/contract
         racket/list
         "../types.rkt")

(provide (contract-out
          [parse-article-forest
           (-> list? parsed-article?)]
          ))

(define (parse-article-forest forest)
  (match-let* ([(cons `(front ,_ . ,forest) _)
                (drop-non-elements forest)]
               [(cons `(journal-meta ,_ . ,journal-meta-body)
                      (app drop-non-elements
                           (cons `(article-meta ,_ . ,article-meta-body)
                                 _)))
                (drop-non-elements forest)])
    (parse-article-meta-body
     #:publisher (find/parse-publisher-name journal-meta-body)
     #:journal-title (find/parse-journal-title journal-meta-body)
     article-meta-body)))


(define (parse-article-meta-body #:journal-title parsed-journal-title
                                 #:publisher publisher-name
                                 article-meta-body)
  ;; JSTOR says these are "reliably present":
  ;;   article-id self-uri contrib-group pub-date(1+)
  ;; Desirable but not always present:
  ;;   title-group(0-1) volume(0-1) issue(0-1) abstract(0+)
  ;; TODO:
  ;; http://jats.nlm.nih.gov/archiving/tag-library/1.2/element/volume-issue-group.html
  ;; interesting re bibliography
  (make-parsed-article
   #:journal-title parsed-journal-title
   #:publisher publisher-name
   #:article-title (find/parse-article-title article-meta-body)
   #:contributors (find/parse-contribs article-meta-body)
   #:pub-dates (find/parse-pub-dates article-meta-body)
   #:abstract (find/parse-abstract article-meta-body)
   #:volume (apply-when element/mixed-body->string
                        (find-element-named 'volume article-meta-body))
   #:issue (apply-when element/mixed-body->string
                       (find-element-named 'issue article-meta-body))
   #:self-uri (match (find-element-named 'self-uri article-meta-body)
                [`(self-uri ,attrs . ,_)
                 (attributes-ref attrs 'xlink:href)]
                [#f
                 ;; self-uri is not actually always present:
                 ;; vid. e.g. #"jstor_metadata/journal-article-10.2307_26240146.xml"
                 #f])
   #:id
   (for*/fold ([article-id #f])
              ([elem (in-list article-meta-body)]
               #:when (element-xexpr-named? 'article-id elem)
               [type (in-value
                      (string->symbol
                       (attributes-ref (cadr elem) 'pub-id-type)))]
               #:final (eq? type 'doi))
     (cons type (extract-element-text elem)))))


(define (find/parse-publisher-name xs)
  ;; JSTOR says should be present
  (match (find-element-named 'publisher xs)
    [`(publisher ,_ . ,xs)
     (extract-element-text
      (find-element-named 'publisher-name xs))]))

(define (find/parse-journal-title xs)
  (match (find-element-named 'journal-title-group xs)
    ;; JSTOR says should be present
    ;; BUT it isn't for (at least)
    ;; #"jstor_metadata/journal-article-10.2979_pft.2009.29.3.362.xml"
    ;; (which has an abstract and everything)
    [#f
     ;; FIXME: try ISSN?
     #f]
    [`(journal-title-group ,_ . ,body)
     ;; technically, there could be 0 or more of
     ;; each of these elements, but the semantics
     ;; of having more than one in the same group
     ;; doesn't seem to be specified.
     (make-journal-title
      (extract-element-text ;; JSTOR says should be present
       (find-element-named 'journal-title body))
      (apply-when extract-element-text
                  (find-element-named 'journal-subtitle body))
      (apply-when extract-element-text
                  (find-element-named 'abbrev-journal-title body)))]))


(define (find/parse-article-title article-meta-body)
  (match (find-element-named 'title-group article-meta-body)
    [#f
     #f]
    [(list* 'title-group _
            (app drop-non-elements
                 (cons `(article-title ,_ . ,main-title-body)
                       (app drop-non-elements more))))
     ;; http://jats.nlm.nih.gov/archiving/tag-library/1.2/pe/article-title-elements.html
     ;; http://jats.nlm.nih.gov/archiving/tag-library/1.2/pe/subtitle-elements.html
     (define main
       (and (pair? main-title-body)
            (parse-phrasing-content main-title-body)))
     (define subtitles
       (filter-map (match-lambda
                     [`(subtitle ,_ . ,a-sub-body)
                      #:when (pair? a-sub-body)
                      (parse-phrasing-content a-sub-body)]
                     [_
                      #f])
                   more))
     (cond
       [main
        (make-parsed-article-title main
                                   subtitles)]
       [(pair? subtitles)
        (raise-argument-error 'find/parse-article-title
                              "non-empty main title with these subtitles"
                              subtitles)]
       [else
        #f])]))


