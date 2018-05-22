#lang scribble/manual

@title{Old Implementation Details}

@(require (only-meta-in 0 "for-manual.rkt")
          (for-label (except-in racket
                                date
                                date?)
                     racket/unit
                     xml
                     data/maybe
                     ricoeur/tei/oop
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

@nested[#:style 'inset]{
 @bold{WARNING:} The bindings documented in this section
 are @bold{not} provided by @racketmodname[ricoeur/tei].
 They are subject to backwards-incompatible changes during
 the ongoing revision of this library.
}


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
 @racket[postgresql-searchable-document-set], or use
 @racket[noop-searchable-document-set].
}

@defproc[(search-documents [term term/c]
                           [docs searchable-document-set?]
                           [#:exact? exact? any/c #f]
                           [#:book/article book/article (or/c 'any 'book 'article) 'any]
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

@;{
 ;                                                                  
 ;                                                                  
 ;                                                                  
 ;                                                                  
 ;   ;;                      ;;                          ;;         
 ;   ;;                      ;;                          ;;         
 ;   ;;;;      ;;       ;;;  ;;   ;;   ;;;   ;; ;     ;;;;;    ;;   
 ;   ;;  ;    ;  ;    ;;   ; ;;  ;   ;;   ;  ;;; ;   ;   ;;  ;;  ;  
 ;   ;;  ;       ;;   ;      ;; ;    ;    ;  ;;  ;;  ;   ;;   ;     
 ;   ;;  ;;    ;;;;  ;;      ;;;;   ;;;;;;;; ;;  ;; ;;   ;;    ;;   
 ;   ;;  ;    ;  ;;   ;      ;;  ;   ;       ;;  ;;  ;   ;;      ;; 
 ;   ;;  ;   ;;  ;;   ;;   ; ;;   ;  ;;   ;  ;;  ;;  ;   ;;  ;   ;  
 ;   ; ;;     ;;; ;     ;;;  ;;   ;;   ;;;   ;;  ;;   ;;; ;   ;;;   
 ;                                                                  
 ;                                                                  
 ;                                                                  
 ;                                                                  
}

@subsection{Implementing Search Backends}
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

@deftogether[
 (@defthing[exact?-px-prefix-str string?]
   @defthing[exact?-px-suffix-str string?])]{
Strings for constructing a regular expression to exclude lexical variants.
For some @racket[term], a regular expression constructed using:
@racketblock[(pregexp (string-append exact?-px-prefix-str
                                 (regexp-quote term #f)
                                 exact?-px-suffix-str))]
will match only strings that contain @racket[term] exactly
(in the sense of the @racket[#:exact?] argument to @racket[term-search]).
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
                  [results (non-empty-listof search-result?)])]{
  Constructs a @tech{document search results} object. Note that the
  @tech{searchable document set} implementation is responsible for
  sanatizing @tech{search result} values as documented under
  @racket[EXCERPT_RATIO] before passing them as the @racket[results]
  argument to this constructor.
  
  Also note that @racket[results] must not be @racket['()] —
  it makes life easier for clients of this library to be able to rely on
  the guarantee that a @racket[document-search-results%] object
  means there are actually some results.
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
 will likely use its own representation of segments internally
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
 An opaque contract recognizing valid @racket[pre-segment] meta-data.
 The precise specification this meta-data is a private implementation
 detail, except that all values satisfying @racket[pre-segment-meta/c]
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
                                 [#:book/article book/article (or/c 'any 'book 'article) 'any]
                                 [#:exact? exact? any/c #f])
            (listof (is-a?/c document-search-results<%>))]{
  Used to implement @racket[search-documents].
                    
  When the @racket[ricoeur-only?] argument is non-false,
  the concrete implementation should exclude portions of the document
  not written by Paul Ricœur.

  When the @racket[exact?] argument is non-false,
  the concrete implementation should exclude lexical variants,
  perhaps by using @racket[exact?-px-prefix-str] and
  @racket[exact?-px-suffix-str].

  When the @racket[book/article] argument is @racket['book] or @racket['article],
  the concrete implementation should only include results from documents for
  which @racket[(eq? book/article (send doc #,(method TEI-info<%> get-book/article)))]
  would return @racket[#t].
  
  @bold{This is an abstract method.} Concrete subclasses @bold{must}
  override it with an implementation that actually searches
  the documents encapsulated by @(this-obj).
 }
}





