;; See https://github.com/racket/typed-racket/issues/902
;; Succeeds:
;;   #lang typed/racket/no-check
;; Fails with "struct-ref: bad access",
;; with or without `#:no-optimize`:
#lang typed/racket #:no-optimize

(provide Metadata-Archive
         metadata-archive?
         write-metadata-archive
         (all-from-out "topic-model.rkt")
         (struct-out secondary-lit-metadata-archive)
         (struct-out metadata-item)
         (struct-out article)
         (struct-out journal-title)
         make-journal-title
         (struct-out article-title)
         (struct-out whole-book-meta)
         (struct-out book-publisher)
         (struct-out book-part)
         (struct-out book-part-title)
         Contributor
         (struct-out styled-name-part)
         (struct-out contributor)
         (struct-out group-contributor)
         (struct-out unstructured-contributor)
         (struct-out person-contributor)
         (struct-out given-only-contributor)
         (struct-out surname-contributor)
         (struct-out publication-date)
         YMD
         (struct-out abstract)
         Strict-Xexpr
         Html-Forest)

(module+ private-for-preprocessor
  (provide make-metadata-archive-from-preprocessor))

(require "topic-model.rkt"
         (for-syntax racket/base
                     syntax/parse))

(require/typed
 racket/fasl
 [(s-exp->fasl archive-prefab->fasl)
  (-> secondary-lit-metadata-archive Output-Port Void)])

(define-type Metadata-Archive metadata-archive)
(struct metadata-archive
  ([external-representation : secondary-lit-metadata-archive]))

(: write-metadata-archive (-> Metadata-Archive Output-Port Void)) 
(define (write-metadata-archive it out)
  (archive-prefab->fasl (metadata-archive-external-representation it) out))

(define-for-syntax varref
  (#%variable-reference))

(define-syntax make-metadata-archive-from-preprocessor
  (syntax-parser
    [(_ prefab:expr)
     ;; The privacy-enforcement is experimental
     #:fail-unless (syntax-original? (syntax-local-introduce this-syntax))
     "use syntax is not syntax-original?"
     (define expected
       (module-path-index-resolve
        (module-path-index-join
         "../preprocessor/types.rkt"
         (variable-reference->module-path-index varref))))
     (define given
       (let ([v (syntax-source this-syntax)])
         (if (or (path? v) (symbol? v))
             (make-resolved-module-path v)
             v)))
     (unless (equal? expected given)
       (raise-syntax-error #f
                           (format "illegal use site\n  expected: ~e\n  given: ~e"
                                   expected
                                   given)
                           this-syntax))
     (syntax-protect
      #'(ann (metadata-archive (ann prefab secondary-lit-metadata-archive))
             metadata-archive))]))





(struct secondary-lit-metadata-archive
  ([model : Topic-Model-Spec]
   [books : (Immutable-Vectorof whole-book-meta)]
   [items : (Listof metadata-item)])
  #:prefab)

(struct metadata-item
  ([metadata : (U article (Pairof book-part Index))]
   [matches : (Nonempty-Listof (Pairof Symbol Positive-Index))])
  #:prefab)

(struct article
  ([journal-title : (U #f journal-title)] ;; FIXME: try ISSN when not found?
   [publisher : String]
   [id : (Pairof Symbol String)]
   [self-uri : (U #f String)]
   [volume : (U #f String)]
   [issue : (U #f String)]
   [article-title : (U #f article-title)]
   [contributors : (Listof Contributor)]
   [publication-dates : (Listof publication-date)]
   [abstract : (U #f abstract)])
  #:prefab)

(struct journal-title
  ([main-title : String]
   [subtitle : (U #f String)]
   [abbreviation : (U #f String)])
  #:prefab
  #:extra-constructor-name make-journal-title)

(struct article-title
  ([main-title : Html-Forest]
   [subtitles : (Listof Html-Forest)])
  #:prefab)

(struct whole-book-meta
  ([main-title : Html-Forest]
   [subtitles : (Listof Html-Forest)]
   [contributors : (Listof Contributor)]
   [publishers : (Listof book-publisher)]
   [publication-dates : (Listof publication-date)]
   [self-uri : String])
  #:prefab)

(struct book-publisher
  ([name : Html-Forest]
   [location : (U #f Html-Forest)])
  #:prefab)

(struct book-part
  ([maybe-title : (U #f book-part-title)]
   [contributors : (Listof Contributor)]
   [page-spec : (U #f String (Pairof String String))]
   [jstor-id : (U #f String)] ;; Sadly not always present ...
   [abstract : (U #f abstract)])
  #:prefab)

(struct book-part-title
  ([label : (U #f Html-Forest)]
   [main-title : (U #f Html-Forest)]
   [subtitles : (Listof Html-Forest)])
  #:prefab)

(define-type Contributor
  (U group-contributor
     unstructured-contributor
     given-only-contributor
     surname-contributor))

(struct styled-name-part
  ([string : String]
   [forest : Html-Forest])
  #:prefab)

(struct contributor
  ;; not controlled vocab, but examples include
  ;; '(author editor translator)
  ([contribution-type : (U #f Symbol)])
  #:prefab)

(struct group-contributor contributor
  ([name : styled-name-part])
  #:prefab)

(struct unstructured-contributor contributor
  ([string : String])
  #:prefab)

(struct person-contributor contributor
  ([prefix : (U #f styled-name-part)]
   [suffix : (U #f styled-name-part)])
  #:prefab)

(struct given-only-contributor person-contributor
  ([given-names : styled-name-part])
  #:prefab)

(struct surname-contributor person-contributor
  ([style : (U 'western 'eastern 'islensk)]
   [surname : styled-name-part]
   [given-names : (U #f styled-name-part)])
  #:prefab)

(struct publication-date
  ([;; e.g. 'print
    publication-format : (U #f Symbol)]
   [;; "what happened"
    ;; suggested values:
    ;; '(accepted corrected pub preprint received resubmitted
    ;;   retracted rev-recd rev-request)
    publication-event : (U #f Symbol)]
   [;; validated by Gregor in untyped code
    ymd : YMD]
   [season : (U #f String)]
   [string-date : (U #f String)])
  #:prefab)

(define-type YMD
  (U (List Exact-Positive-Integer
           Exact-Positive-Integer
           Exact-Positive-Integer)
     (List Exact-Positive-Integer
           Exact-Positive-Integer)
     (List Exact-Positive-Integer)
     Null))

(struct abstract
  ([label : (U #f Html-Forest)] ;; phrasing content
   [title : (U #f Html-Forest)] ;; phrasing content
   [body : Html-Forest]) ;; paragraphs
  #:prefab)

(define-type Strict-Xexpr
  (U String
     (Pairof Symbol
             (Pairof (Listof (List Symbol String))
                     (Listof Strict-Xexpr)))))

(define-type Html-Forest (Listof Strict-Xexpr))
