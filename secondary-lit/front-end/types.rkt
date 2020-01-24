#lang typed/racket/no-check ;; FIXME !!!
;; See https://github.com/racket/typed-racket/issues/902

(provide read-metadata-archive)

(module* tmp #f
  (provide metadata-demo-xexpr
           metadata-archive->internal))

(module* demo racket
  (require (submod "..")
           (submod ".." tmp)
           xml
           racket/runtime-path)
  (define-runtime-path metadata-archive.fasl.rktd
    "../metadata-archive.fasl.rktd")
  (write-xexpr
   (metadata-demo-xexpr
    (metadata-archive->internal
     (call-with-input-file* metadata-archive.fasl.rktd
       read-metadata-archive))))
  (newline))
   

(require "../kernel/types.rkt"
         (submod "../kernel/types.rkt"
                 private-for-frontend))

(define-metadata-archive-subtype deserialized-metadata-archive
  #:transparent
  #:reader-name read-metadata-archive
  #:accessor-name metadata-archive->internal
  #:internal-representation
  (struct metadata
    ([demo-xexpr : Any])
    #:transparent
    #:type-name Internal-Metadata)
  #:build build-metadata-values)

(define (build-metadata-values prefab)
  (define books-vec
    (secondary-lit-metadata-archive-books prefab))
  (define title-string
    "Secondary Literature Metadata: Initial Results")
  `(html
    ([lang "en"])
    (head
     (title ,title-string)
     (meta ([charset "utf-8"]))
     (meta ([name "viewport"]
            [content "width=device-width,initial-scale=1"])))
    (body
     (h1 ,title-string)
     ,@(map (λ (item)
              (define inner (metadata-item-metadata item))
              `(section ()
                        ,@(if (pair? inner)
                              (book-part->forest (car inner) (vector-ref books-vec (cdr inner)))
                              (article->forest inner))
                        (div
                         (h3 "Assosciated Words")
                         (ul ,@(map (λ (pr)
                                      `(li ,(symbol->string (car pr))))
                                    (metadata-item-matches item))))))
            (secondary-lit-metadata-archive-items prefab)))))



(define (book-part->forest this whole)

  '("book"))

(define (apply-when proc v)
  (and v (proc v)))

(define (article->forest this)
  `{,@(cond
        [(article-article-title this)
         => (λ (x)
              `(h2 ,@(append* (add-between
                               (cons (article-title-main-title x)
                                     (article-title-subtitles x))
                               '(": ")))))]
        [else
         `{(h2 "Untitled")}])
    (p ,@(append*
          (add-between (filter values
                               (list (apply-when (compose1 list journal-title-main-title)
                                                 (article-journal-title this))
                                     (apply-when (λ (str) `("volume " ,str))
                                                 (article-volume this))
                                     (apply-when (λ (str) `("issue " ,str))
                                                 (article-issue this))))
                       '(", "))))
    ,(maybe-abstract->xexpr (article-abstract this))
    })


(define maybe-abstract->xexpr
  (match-lambda
    [#f
     `(p "No abstract available.")]
    [(abstract label title body)
     `(div
       (h3 ,(append* (add-between (cons '("Abstract")
                                        (filter values (list label title)))
                                  '(": "))))
       ,@body)]))
     


