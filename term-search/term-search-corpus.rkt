#lang racket/base

(require racket/contract
         racket/class
         ricoeur/tei
         "backend.rkt")

(provide term-search-corpus<%>
         (contract-out
          [term-search-corpus-mixin
           (and/c (make-mixin-contract corpus%)
                  (-> (class/c
                       (absent term-search))
                      (and/c (implementation?/c term-search-corpus<%>)
                             (class/c
                              (init [search-backend
                                     search-backend/c])))))]
          [corpus-do-term-search
           (->* {(is-a?/c term-search-corpus<%>) 
                 term/c}
                {#:ricoeur-only? any/c
                 #:languages search-languages/c
                 #:book/article (or/c 'any 'book 'article)
                 #:exact? any/c}
                (instance-set/c document-search-results?))]
          [term-search
           (->* {term/c}
                {#:ricoeur-only? any/c
                 #:languages search-languages/c
                 #:book/article (or/c 'any 'book 'article)
                 #:exact? any/c}
                (instance-set/c document-search-results?))]
          ))


(define-corpus-mixin+interface term-search-corpus
  [] []
  (interface ()
    [term-search (->*m [term/c]
                       [#:ricoeur-only? any/c
                        #:languages search-languages/c
                        #:book/article (or/c 'any 'book 'article)
                        #:exact? any/c]
                       (instance-set/c document-search-results?))])
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
     #:exact? exact?)))












(define gen:term-search
  (generic term-search-corpus<%> term-search))

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


(define (term-search term
                     #:ricoeur-only? [ricoeur-only? #t]
                     #:languages [langs 'any]
                     #:book/article [book/article 'any]
                     #:exact? [exact? #f])
  (define c (current-corpus))
  (if (is-a? c term-search-corpus<%>)
      (corpus-do-term-search term
                             #:ricoeur-only? ricoeur-only?
                             #:languages langs
                             #:book/article book/article
                             #:exact? exact?)
      (instance-set)))

