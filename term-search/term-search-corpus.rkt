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


(define-corpus-mixin+interface term-search-corpus [] []
  (interface ()
    [#:proc corpus-do-term-search
     #:with-current/infer #:else [(instance-set)]
     #:contract
     (->*m [term/c]
           [#:ricoeur-only? any/c
            #:languages search-languages/c
            #:book/article (or/c 'any 'book 'article)
            #:exact? any/c]
           (instance-set/c document-search-results?))
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
        #:exact? exact?))])
  (init [search-backend '(eager noop)])
  (super-new)
  (define searchable-document-set
    (initialize-search-backend search-backend (super-docs))))

