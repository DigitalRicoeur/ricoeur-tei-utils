#lang racket/base

(require racket/contract
         racket/class
         ricoeur/tei/base
         ricoeur/tei/search
         "plain-corpus.rkt"
         )

(provide corpus<%>
         (contract-out
          [corpus-search-mixin
           (and/c (make-mixin-contract plain-corpus%)
                  (-> (class/c (absent term-search))
                      (and/c (implementation?/c corpus<%>)
                             (class/c (init [search-backend
                                             search-backend/c])))))]
          [corpus-do-term-search
           (->* {(is-a?/c plain-corpus%) ;; or just support corpus<%> ??
                 term/c}
                {#:ricoeur-only? any/c
                 #:book/article (or/c 'any 'book 'article)
                 #:exact? any/c}
                (instance-set/c document-search-results?))]
          ))

(define-values {corpus<%> corpus-search-mixin}
  (let* ([initialize-search-method-key
          (generate-member-key)]
         [pre-mixin 
          (make-corpus-mixin initialize-search-method-key)])
    (define-member-name initialize-this initialize-search-method-key)
    (define corpus<%>
      (interface {(class->interface plain-corpus%)}
        [term-search (->*m {term/c}
                           {#:ricoeur-only? any/c
                            #:book/article (or/c 'any 'book 'article)
                            #:exact? any/c}
                           (instance-set/c document-search-results?))]
        initialize-this))
    (define (corpus-search-mixin %)
      (class* (pre-mixin %) {corpus<%>}
        (init [(_search-backend search-backend) '(eager noop)])
        (define search-backend _search-backend)
        (define searchable-document-set #f)
        (super-new)
        (define/override-final (initialize-this docs)
          (set! searchable-document-set
                (initialize-search-backend search-backend docs)))
        (define/public-final (term-search term
                                          #:ricoeur-only? [ricoeur-only? #t]
                                          #:book/article [book/article 'any]
                                          #:exact? [exact? #f])
          (unless searchable-document-set
            (error '|method term-search of corpus-search-mixin|
                   "attempt to use method before initialization"))
          (searchable-document-set-do-term-search
           searchable-document-set term
           #:ricoeur-only? ricoeur-only?
           #:book/article book/article
           #:exact? exact?))))
    (values corpus<%>
            corpus-search-mixin)))

(define gen:term-search
  (generic corpus<%> term-search))

(define (corpus-do-term-search corpus term
                               #:ricoeur-only? [ricoeur-only? #t]
                               #:book/article [book/article 'any]
                               #:exact? [exact? #f])
  (if (infix: corpus is-a? corpus<%>)
      (send-generic corpus gen:term-search
                    term
                    #:ricoeur-only? ricoeur-only?
                    #:book/article book/article
                    #:exact? exact?)
      (instance-set)))


