#lang scribble/manual

@title{Implementation Details}

@(require (for-label (except-in racket
                                date
                                date?)
                     racket/unit
                     xml
                     data/maybe
                     ricoeur/tei
                     ricoeur/tei/xexpr/signatures
                     ricoeur/lib
                     db
                     json
                     (only-in ricoeur/tei/search
                              search-documents
                              searchable-document-set?
                              regexp-searchable-document-set
                              postgresql-searchable-document-set)
                     (submod ricoeur/tei/search/common private)
                     setup/matching-platform
                     gregor
                     ))

@section{Contract Implementation}
@defmodule[ricoeur/tei/xexpr/signatures]

The bindings documented in this section are provided by
@racketmodname[ricoeur/tei/xexpr/signatures], but not by
@racketmodname[ricoeur/tei] or @racketmodname[ricoeur/tei/base].
They are used in the implementation of contracts on TEI x-expressions.

@defsignature[tei-xexpr-contracts^ ()]{
 @signature-desc{
  This signature specifies both the contract bindings ultimately
  provided by @racketmodname[ricoeur/tei] and the binding
  @sigelem[tei-xexpr-contracts^ make-element-contract] for specifying
  the contracts on individual elements.
  It is implemented by @racket[tei-xexpr-contracts@].
 }
 @defthing[tei-element-name/c flat-contract?]{
  Used to implement @racket[tei-element-name/c].
 }
 @defproc[(tei-xexpr/c [name tei-element-name/c]) flat-contract?]{
  Used to implement @racket[tei-xexpr/c].
 }
 @defthing[any-tei-xexpr/c flat-contract?]{
  Used to implement @racket[any-tei-xexpr/c].
 }
 @defproc[(make-element-contract [name tei-element-name/c]
                                 [#:children children
                                  (listof (list/c (or/c 1 '1+ '0-1 '0+) tei-element-name/c))
                                  '()]
                                 [#:text? text? any/c #f]
                                 [#:required-order required-order
                                  (listof tei-element-name/c)
                                  '()]
                                 [#:attr-contracts attr-contracts
                                  (listof (list/c symbol? flat-contract?))
                                  '()]
                                 [#:required-attrs required-attrs (listof symbol?) '()]
                                 [#:extra-check extra-check
                                  (or/c #f (-> (and/c list? xexpr/c)
                                               (or/c blame? #f)
                                               any/c
                                               any/c))
                                  #f])
          flat-contract?]{
  Creates a contract recognizing a particular Digital Ricœur TEI XML element.
  Used by units implementing @racket[element-contracts^].

  The @racket[attr-contracts] argument specifies contracts for the values
  of attributes, without requiring that those attributes be present.

  The @racket[required-order] argument applies only to children that
  are actually present. It also allows other children to be present,
  and it does not insist that they be in any particular order.

  When a function is provided as the @racket[extra-check] argument,
  its first argument is the value to which the contract is being applied,
  which is guaranteed to satisfy all of the other requirements of
  the contract (including the implicit requirement of having valid children).
  When its second argument is @racket[#f], it should ignore the third
  argument and return @racket[#f] to signal that the check fails or
  any other value if the check succeeds.
  When its second argument is a blame object, the return value is ignored
  and violations must be signaled using @racket[raise-blame-error], 
  with the third argument supplied for the @racket[#:missing-party].
 }
 @defproc[(get-attributes [elem (and/c list? xexpr/c)])
          (listof (list/c symbol? string?))]{
  Extracts the attributes from any element x-expression
 }
 @defproc[(get-body [elem (and/c list? xexpr/c)])
          (listof xexpr/c)]{
  Extracts the body of any element x-expression
 }
}

@defthing[tei-xexpr-contracts@ unit?]{
 A unit implementing @racket[tei-xexpr-contracts^].
 It must be linked with an implementation of @racket[element-contracts^]
 prior to invokation.
}

@defsignature[element-contracts^ ()]{
 @signature-desc{
  This signature specifies contracts on individual Digital Ricœur TEI XML elements
  using @sigelem[tei-xexpr-contracts^ make-element-contract].
  The unit implementing this signature, @racket[element-contracts@], is
  documented in "literate programming" style
  under @secref["Formal_Specification"
                #:doc '(lib "ricoeur/tei/scribblings/guidelines.scrbl")]
  in @other-doc['(lib "ricoeur/tei/scribblings/guidelines.scrbl")].
  It must be linked with @racket[tei-xexpr-contracts@] prior to invokation.
}}

@;{
 ;                                                  
 ;                                                  
 ;                                                  
 ;                                                  
 ;                                           ;;     
 ;                                           ;;     
 ;     ;;      ;;;     ;;    ;; ;;;     ;;;  ;; ;   
 ;   ;;  ;   ;;   ;   ;  ;   ;;;      ;;   ; ;;; ;  
 ;    ;      ;    ;      ;;  ;;       ;      ;;  ;; 
 ;     ;;   ;;;;;;;;   ;;;;  ;;      ;;      ;;  ;; 
 ;       ;;  ;        ;  ;;  ;;       ;      ;;  ;; 
 ;   ;   ;   ;;   ;  ;;  ;;  ;;       ;;   ; ;;  ;; 
 ;    ;;;      ;;;    ;;; ;  ;;         ;;;  ;;  ;; 
 ;                                                  
 ;                                                  
 ;                                                  
 ;                                                  
}

@section{Search Implementation}
@defmodule[ricoeur/tei/search]

The bindings documented in this section are provided by
@racketmodname[ricoeur/tei/search], but not by
@racketmodname[ricoeur/tei]. They are used to implement the
@(method corpus% •term-search) method of @racket[corpus%], and
ultimately @racket[term-search].

@defproc[(searchable-document-set? [v any/c]) any/c]{
 A @deftech{searchable document set} encapsulates some work that must be done
 to prepare a set of TEI documents to be searched using a specific
 @tech{search backend}. Constuct @tech{searchable document sets} using
 @racket[regexp-searchable-document-set] or
 @racket[postgresql-searchable-document-set].
}

@defproc[(search-documents [term term/c]
                           [docs searchable-document-set?]
                           [#:exact? exact? any/c #f]
                           [#:ricoeur-only? ricoeur-only? any/c #t])
         (listof (is-a?/c document-search-results<%>))]{
 Used to implement @racket[term-search].
}

@defproc[(postgresql-searchable-document-set [#:db db postgresql-data-source/c]
                                             [docs (listof (is-a?/c TEI<%>)) '()])
         searchable-document-set?]{
 Constructs a @tech{searchable document set} using a PostgreSQL-based
 @tech{search backend} as specified by @racket[db].
}

@defproc[(regexp-searchable-document-set [docs (listof (is-a?/c TEI<%>)) '()])
         searchable-document-set?]{
 Constructs a @tech{searchable document set} using a simplistic regular-expression-based
 @tech{search backend} implemented in Racket.        
}

@defthing[noop-searchable-document-set searchable-document-set?]{
 A @tech{searchable document set} that never finds any results.
}

@subsubsection{Implementing Search Backends}
@defmodule[(submod ricoeur/tei/search/common private)]

The low-level bindings documented in this section are used to implement new kinds
of @tech{searchable document sets} to support new @tech{search backends}.

@defthing[EXCERPT_RATIO real?]{
 Specifies the maximum portion of a given TEI document which may be shown in
 the excerpt field of the @tech{search result} values for a given query.
 The specific @tech{searchable document set} implementation is responsible
 for ensuring that excess excerpts are replaced by @racket[nothing] values
 (perhaps by using @racket[nullify-search-result-excerpt]) before
 the @tech{search result} values are supplied to the @racket[document-search-results%]
 constructor.
}

@defproc[(make-search-result [#:counter counter natural-number/c]
                             [#:sub-counter sub-counter natural-number/c]
                             [#:meta meta pre-segment-meta/c]
                             [#:excerpt excerpt (maybe/c non-empty-string?)])
         search-result?]{
 Constructs a @tech{search result} value. The @racket[counter] and
 @racket[meta] arguments should be drawn from the fields of the corresponding
 @racket[pre-segment]. The @racket[sub-counter] argument should indicate the
 position of the @tech{search result} value relative to other
 @tech{search result} values for the same query that are drawn from the same
 @racket[pre-segment]: it is used to implement fuctions like
 @racket[search-result<?].

 @bold{Note} that using @racket[search-result-author-string] on a
 @tech{search result} value that has not yet been passed to
 the @racket[document-search-results%] constructor will raise
 an exception.
}

@defproc[(nullify-search-result-excerpt [s-r search-result?])
         search-result?]{
 Returns a @tech{search result} value like @racket[s-r], but with
 @racket[nothing] as the excerpt.
}

@defclass[document-search-results% object% (TEI-info<%> document-search-results<%>)]{
 A @tech{document search results} object is an instance of the
 @racket[document-search-results%] class, which is the only class to implement
 the @racket[document-search-results<%>] interface.
 @defconstructor[([info (is-a?/c TEI-info<%>)]
                  [results (listof search-result?)])]{
  Constructs a @tech{document search results} object. Note that the
  @tech{searchable document set} implementation is responsible for
  sanatizing @tech{search result} values as documented under
  @racket[EXCERPT_RATIO] before passing them as the @racket[results]
  argument to this constructor.
 }
}

@deftogether[(@defproc[(prepare-pre-segments [doc (is-a?/c TEI<%>)])
                       (listof pre-segment?)]
               @defstruct*[pre-segment ([title string?]
                                        [counter natural-number/c]
                                        [body string?]
                                        [meta pre-segment-meta/c]
                                        [resp #rx"^#.+"])
                           #:omit-constructor])]{
 Current search implementations rely on splitting TEI documents
 into smaller segments that share the same meta-data (such as page
 numbers). While each @tech{searchable document set} implementation
 will likeley use its own representation of segments internally
 (e.g. by storing them as rows in a database), the shared functionality
 for segmenting a document is implemented by the
 function @racket[prepare-pre-segments], which returns its results
 as @racket[pre-segment] values.

 Note that the @racket[pre-segment] constructor is not provided directly.

 The @racket[title] field contains the document's title, exactly
 as returned by @racket[(send doc #,(method TEI-info<%> get-title))].
 The @racket[counter] field indicates the position of the @racket[pre-segment]
 relative to the others returned by the same call to @racket[prepare-pre-segments],
 and should be pased on to @racket[make-search-result].
 The @racket[body] field contains the plain text of the @racket[pre-segment].
 The @racket[meta] field encapsulates some meta-data in the format expected
 by @racket[make-search-result].
 The @racket[resp] field contains either @racket["#ricoeur"], indicating that
 the @racket[pre-segment] represents a
 portion of the TEI document written by Paul Ricœur,
 or some other string, indicationg that the corresponding portion of the document
 was instead written by an editor, translator, etc.

 The segmentation of documents does not depend on the specific search term,
 so @tech{searchable document set} implementations should generally
 call @racket[prepare-pre-segments] only once for each @racket[doc].
}

@defthing[pre-segment-meta/c flat-contract?]{
 An opaque contract recognizing valid @racket[pre-segment] meta-data,
 the precise specification of which is a private implementation,
 detail except that all values satisfying @racket[pre-segment-meta/c]
 will also satisfy @racket[jsexpr?].
}

@defclass[abstract-searchable-document-set% object% ()]{
 A @tech{searchable document set} is an instance of a subclass of
 the abstract class @racket[abstract-searchable-document-set%].
 New @tech{search backends} are implemented by deriving new
 concrete subclasses of @racket[abstract-searchable-document-set%]
 and overriding the implementation of
 @(method abstract-searchable-document-set% do-search-documents).
 @defconstructor[()]{
  No initialization arguments are required or accepted by
  @racket[abstract-searchable-document-set%].
  Concrete subclasses will generally need to be initialized with
  at least some @racket[TEI<%>] objects, and perhaps other
  backend-specific arguments.
 }
 @defmethod[(do-search-documents [term term/c]
                                 [#:ricoeur-only? ricoeur-only? any/c #t]
                                 [#:exact? exact? any/c #f])
            (listof (is-a?/c document-search-results<%>))]{
  Used to implement @racket[search-documents].
                    
  When the @racket[ricoeur-only?] argument is non-false,
  the concrete implementation should exclude portions of the document
  not written by Paul Ricœur.

  @bold{This is an abstract method.} Concrete subclasses @bold{must}
  override it with an implementation that actually searches
  the documents encapsulated by @(this-obj).
 }
}





