#lang racket/base

(require racket/contract
         racket/class
         ricoeur/tei/base
         ricoeur/tei/search
         "plain-corpus.rkt")

(provide corpus<%>
         (contract-out
          [corpus-search-mixin
           (and/c (make-mixin-contract plain-corpus%)
                  (-> (class/c (absent term-search))
                      (and/c (implementation?/c corpus<%>)
                             (class/c (init [search-backend
                                             search-backend/c])))))]
          [corpus-do-term-search
           (->* {(is-a?/c corpus<%>) 
                 term/c}
                {#:ricoeur-only? any/c
                 #:languages search-languages/c
                 #:book/article (or/c 'any 'book 'article)
                 #:exact? any/c}
                (instance-set/c document-search-results?))]
          ))

(define-values {corpus<%> corpus-search-mixin}
  (let* ()
    (define-local-member-name search-corpus-tag-method)
    (define corpus<%>
      (interface {plain-corpus<%>}
        [term-search (->*m [term/c]
                           [#:ricoeur-only? any/c
                            #:languages search-languages/c
                            #:book/article (or/c 'any 'book 'article)
                            #:exact? any/c]
                           (instance-set/c document-search-results?))]
        search-corpus-tag-method))
    (define corpus-search-mixin
      (corpus-mixin [] [corpus<%>]
        (init [search-backend '(eager noop)])
        (super-new)
        (define searchable-document-set
          (initialize-search-backend search-backend (super-docs)))
        (define/public-final (search-corpus-tag-method) (void))
        (define/public-final (term-search term
                                          #:ricoeur-only? [ricoeur-only? #t]
                                          #:languages [langs 'any]
                                          #:book/article [book/article 'any]
                                          #:exact? [exact? #f])
          (searchable-document-set-do-term-search
           searchable-document-set term
           #:ricoeur-only? ricoeur-only?
           #:languages langs
           #:book/article book/article
           #:exact? exact?))))
    (values corpus<%>
            corpus-search-mixin)))

(define gen:term-search
  (generic corpus<%> term-search))

(define (corpus-do-term-search corpus term
                               #:ricoeur-only? [ricoeur-only? #t]
                               #:languages [langs 'any]
                               #:book/article [book/article 'any]
                               #:exact? [exact? #f])
  (send-generic corpus
                gen:term-search
                term
                #:ricoeur-only? ricoeur-only?
                #:languages langs
                #:book/article book/article
                #:exact? exact?))



