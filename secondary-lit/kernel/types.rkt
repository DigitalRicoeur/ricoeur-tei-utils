;; See https://github.com/racket/typed-racket/issues/902
;; Succeeds:
#lang typed/racket/base/no-check ;; FIXME !!!
;; Fails with "struct-ref: bad access",
;; with or without `#:no-optimize`:
;; #lang typed/racket/base #:no-optimize

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

(module+ private-for-frontend
  (provide define-metadata-archive-subtype))

(require "topic-model.rkt")

(require/typed
 racket/fasl
 [{s-exp->fasl secondary-lit-metadata-archive->fasl}
  (-> secondary-lit-metadata-archive Output-Port Void)]
 [{fasl->s-exp fasl->secondary-lit-metadata-archive}
  (-> (U Bytes Input-Port) secondary-lit-metadata-archive)])

(define-type Metadata-Archive metadata-archive)
(struct metadata-archive
  ([external-representation : secondary-lit-metadata-archive]))

(: write-metadata-archive (-> Metadata-Archive Output-Port Void)) 
(define (write-metadata-archive it out)
  (secondary-lit-metadata-archive->fasl
   (metadata-archive-external-representation it)
   out))

(module+ private-for-frontend
  (require syntax/parse/define
           (for-syntax racket/base
                       racket/syntax))
  (define-syntax-parser define-metadata-archive-subtype
    #:track-literals
    #:literals {struct}
    [(_ subtype-name:id
        (~alt (~optional (~seq #:type-name Type-Name:id))
              (~optional (~and #:transparent subtype-transparent-kw))
              (~once (~seq #:accessor-name metadata-archive->internal:id))
              (~once (~seq #:reader-name read-metadata-archive:id))
              (~once (~seq #:build build-proc-expr:expr))
              (~once (~seq #:internal-representation
                           (struct int-rep-name:id
                             ([int-rep-field-name:id
                               (~literal :)
                               Int-Rep-Field-Type:id]
                              ...)
                             (~alt (~optional (~and (~or #:transparent #:prefab)
                                                    int-rep-transparent-kw))
                                   (~optional (~and #:mutable int-rep-mutable-kw))
                                   (~optional (~seq #:type-name Int-Rep-Type-Name:id)))
                             ...))))
        ...)
     #:with Subtype:id #'(~? Type-Name subtype-name)
     #:with Int-Rep:id #'(~? Int-Rep-Type-Name int-rep-name)
     #:with (subtype?:id int-rep?:id)
     (map (λ (id) (format-id id "~a?" id #:subs? #t))
          (list #'subtype-name #'int-rep-name))
     #:with (int-rep-field-accessor:id ...)
     (map (λ (id) (format-id id "~a-~a" #'int-rep-name id #:subs? #t))
          (syntax->list #'(int-rep-field-name ...)))
     #:with (real-subtype-name:id
             real-subtype?:id real-int-rep-name:id real-int-rep?:id
             [real-int-rep-field-name:id real-int-rep-field-accessor] ...)
     (syntax-local-introduce
      #'(subtype-name
         subtype? int-rep-name int-rep?
         [int-rep-field-name int-rep-field-accessor] ...))
     #:with real-subtype-internal-representation:id
     (format-id #'real-subtype-name "~a-internal-representation" #'real-subtype-name)
     #:with build-int-rep (format-id #'real-int-rep-name "build-~a" #'real-int-rep-name)
     #:with build-int-rep-values #'(ann build-proc-expr
                                        (-> secondary-lit-metadata-archive
                                            (Values Int-Rep-Field-Type ...)))
     #'(begin
         (struct real-int-rep-name
           ([real-int-rep-field-name : Int-Rep-Field-Type] ...)
           (~? int-rep-transparent-kw)
           (~? int-rep-mutable-kw)
           #:type-name Int-Rep)
         (define int-rep? real-int-rep?)
         (define int-rep-field-accessor real-int-rep-field-accessor) ...
         (struct real-subtype-name metadata-archive
           ([internal-representation : Int-Rep])
           (~? subtype-transparent-kw)
           #:type-name Subtype)
         (define subtype? real-subtype?)
         (: build-int-rep (-> secondary-lit-metadata-archive Int-Rep))
         (define (build-int-rep prefab)
           (call-with-values (λ () (build-int-rep-values prefab))
                             real-int-rep-name))
         (: read-metadata-archive (-> (U Bytes Input-Port)
                                      (U exn:fail Metadata-Archive)))
         (define (read-metadata-archive in)
           (with-handlers ([exn:fail? (λ ([e : exn:fail]) e)])
             (define prefab
               (fasl->secondary-lit-metadata-archive in))
             (real-subtype-name prefab (build-int-rep prefab))))
         (define cache : (Weak-HashTable Metadata-Archive Int-Rep)
           (make-weak-hasheq))
         (: metadata-archive->internal (-> Metadata-Archive Int-Rep))
         (define (metadata-archive->internal outer)
           (cond
             [(real-subtype? outer)
              (real-subtype-internal-representation outer)]
             [(hash-ref cache outer #f)]
             [else
              (define ret
                (build-int-rep (metadata-archive-external-representation outer)))
              (hash-set! cache outer ret)
              ret])))]))







(module+ private-for-preprocessor
  (module mpi racket/base
    (require racket/runtime-path
             (for-syntax racket/base))
    (provide mpi:ricoeur/preprocessor/types)
    (define-runtime-module-path-index mpi:ricoeur/preprocessor/types
      "../preprocessor/types.rkt"))
  (require syntax/parse/define
           (for-syntax racket/base
                       (only-in (submod "." mpi)
                                mpi:ricoeur/preprocessor/types)))

  (define-for-syntax (source-module->resolved-module-path spec)
    (cond
      [(resolved-module-path? spec)
       spec]
      [(module-path-index? spec)
       (module-path-index-resolve spec)]
      [(or (and (symbol? spec) spec)
           (and (path? spec) (simplify-path spec)))
       => make-resolved-module-path]
      [else
       #f]))

  (define-syntax-parser make-metadata-archive-from-preprocessor
    [(_ prefab:expr)
     ;; The privacy-enforcement is experimental
     #:fail-unless (syntax-original? (syntax-local-introduce this-syntax))
     "use syntax is not syntax-original?"
     (let* ([expected
             (module-path-index-resolve
              mpi:ricoeur/preprocessor/types)]
            [stx-source-module
             (syntax-source-module this-syntax)]
            [stx-source-module/resolved
             (source-module->resolved-module-path stx-source-module)])
       (define (reject! given)
         (define fmt "illegal use site\n  expected: ~e\n  given: ~e")
         (raise-syntax-error #f (format fmt expected given) this-syntax))
       (unless (equal? expected stx-source-module/resolved)
         (unless (and stx-source-module/resolved
                      (symbol? (resolved-module-path-name stx-source-module/resolved)))
           (reject! (or stx-source-module/resolved stx-source-module)))
         (define stx-source (syntax-source this-syntax))
         (define stx-source/resolved (source-module->resolved-module-path stx-source))
         (unless (equal? expected stx-source/resolved)
           (reject! (or stx-source/resolved stx-source stx-source-module/resolved)))))
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
