#lang typed/racket/base/no-check ;; FIXME !!!
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
         racket/list
         racket/match
         (submod "../kernel/types.rkt"
                 private-for-frontend))

(define-type Flex-Xexpr* (Listof Flex-Xexpr))
(define-type Flex-Xexpr
  (U String
     (Pairof Symbol (U Flex-Xexpr*
                       (Pairof (Listof (List Symbol String))
                               Flex-Xexpr*)))))

(define-metadata-archive-subtype deserialized-metadata-archive
  #:transparent
  #:reader-name read-metadata-archive
  #:accessor-name metadata-archive->internal
  #:internal-representation
  (struct metadata
    ([demo-xexpr : Flex-Xexpr])
    #:transparent
    #:type-name Internal-Metadata)
  #:build build-metadata-values)

(: build-metadata-values (-> secondary-lit-metadata-archive Flex-Xexpr))
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
     ,@(map (λ ([item : metadata-item])
              (define inner (metadata-item-metadata item))
              `(section ()
                        ,@(if (pair? inner)
                              (book-part->forest (car inner) (vector-ref books-vec (cdr inner)))
                              (article->forest inner))
                        (div
                         (h3 "Assosciated Words")
                         (ul ,@(map (λ ([pr : (Pairof Symbol Positive-Index)])
                                      `(li ,(symbol->string (car pr))))
                                    (metadata-item-matches item))))))
            (secondary-lit-metadata-archive-items prefab)))))


(: book-part->forest (-> book-part whole-book-meta Flex-Xexpr*))
(define (book-part->forest this whole)

  '("book"))

(define #:∀ (A B) (apply-when [proc : (-> A B)] [v : (U A #f)])
  : (U B #f)
  (and v (proc v)))

(define #:∀ (A) (filter-true [lst : (Listof (U A #f))])
  : (Listof A)
  (for/list ([a (in-list lst)]
             #:when a)
    a))

(: article->forest (-> article Flex-Xexpr*))
(define (article->forest this)
  `{,@(cond
        [(article-article-title this)
         => (λ (x)
              `{(h2 ,@(append* (add-between
                                (cons (article-title-main-title x)
                                      (article-title-subtitles x))
                                '(": "))))})]
        [else
         `{(h2 "Untitled")}])
    (p ,@(append*
          (add-between (filter-true
                        (list (let ([v (article-journal-title this)])
                                (and v (list (journal-title-main-title v))))
                              (let ([str (article-volume this)])
                                (and str `("volume " ,str)))
                              (let ([str (article-issue this)])
                                (and str `("issue " ,str)))))
                       '(", "))))
    ,(maybe-abstract->xexpr (article-abstract this))
    })


(: maybe-abstract->xexpr (-> (U #f abstract) Flex-Xexpr))
(define maybe-abstract->xexpr
  (match-lambda
    [#f
     `(p "No abstract available.")]
    [(abstract label title body)
     `(div
       (h3 ,@(append* (add-between (cons '("Abstract")
                                         (filter-true (list label title)))
                                   '(": "))))
       ,@body)]))
     


