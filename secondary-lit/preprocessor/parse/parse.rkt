#lang racket/base

(require racket/match
         racket/contract
         "../types.rkt"
         "article.rkt"
         "book.rkt")

(provide (contract-out
          [parse-book/article-xexpr
           (-> list? (or/c parsed-article?
                           parsed-book?))]
          ))

(define parse-book/article-xexpr
  ;; https://www.jstor.org/dfr/about/technical-specifications
  (match-lambda
    [`(article ,_ . ,forest)
     (parse-article-forest forest)]
    [`(book ,_ . ,forest)
     (parse-book-forest forest)]))

(module+ main
  (require ricoeur/tei/base)
  (parse-book/article-xexpr
   (call-with-input-file* "/Users/philip/code/ricoeur/scratch/secondary-lit/secondary-lit-dev/small-demo/book-chapter-10.1163_j.ctt1w76ttq.11.xml"
     read-xexpr/standardized)))
