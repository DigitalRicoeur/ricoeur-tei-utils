#lang scribble/manual

@title[#:version ""]{Digital Ricœur TEI Library}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com")]
@defmodule[#:multi (ricoeur/tei ricoeur/tei/base)]

@(require (for-label (except-in racket
                                date
                                date?)
                     racket/unit
                     xml
                     data/maybe
                     ricoeur/tei
                     ricoeur/tei/xexpr/signatures
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

This manual documents utilities and Racket libraries for
working with TEI XML documents developed for Digital Ricœur.
In addition to being valid and well-formed XML, the
documents should conform to the structure specified in
@other-doc['(lib "ricoeur/tei/scribblings/guidelines.scrbl")].

This document is written for programmers intending either to
use these libraries in their own programs (particularly in the case
of @secref["High-level_Interface"] and, to a lesser extent,
@secref["Object_System"]) or to contribute to their implementation.
It assumes familiarity with Racket's class-based system for
object-oriented programming, @racketmodname[racket/class],
and to a lesser extent with Racket's @racketmodname[xml] library,
particularly the concept of x-expressions
(see the grammar documented under @racket[xexpr?]).

@(table-of-contents)

@section{High-level Interface}
@defmodule[ricoeur/tei #:link-target? #f]

The bindings documented in this section are provided by
@racketmodname[ricoeur/tei], but not @racketmodname[ricoeur/tei/base].

@defparam[current-corpus corpus (is-a?/c corpus%)
          #:value empty-corpus]{
 This library uses @deftech{corpus} objects to encapsulate
 collections of TEI documents. High-level functions, such
 as @racket[term-search] and @racket[list-TEI-info],
 invoke methods of the object in
 the @racket[current-corpus] parameter.

 In practice, this parameter should usually be initialized
 with a @racket[directory-corpus%] instance.
}

@defproc[(list-TEI-info) (listof (is-a?/c TEI-info<%>))]{
 Returns a list of @racket[TEI-info<%>] objects for all of the
 documents in the @racket[current-corpus].
}

@defproc[(get-checksum-table) (hash/c string?
                                      string?
                                      #:immutable #t)]{
 Returns an immutable hash table with an entry for every document in the
 @racket[current-corpus], where the keys are the titles of
 the documents (as reported by @(method TEI-info<%> get-title))
 and the values are their md5 checksums (as reported by @(method TEI<%> get-md5)).
}

@defclass[corpus% object% ()]{
 The root class for all @tech{corpus} objects.

 Note that creating a new instance of this class involves a
 fair amount of overhead, so creating redundant values should be avoided.
 (This also improves search performance through caching, for example.)
 
 @defconstructor[([docs (listof (is-a?/c TEI<%>))]
                  [search-backend (or/c #f 'noop postgresql-data-source/c) #f])]{
  Constructs a @tech{corpus} object encapsulating the TEI documents @racket[docs]
  which uses the @tech{search backend} specified by @racket[search-backend]
  to implement @racket[term-search].

  The @racket[docs] should all have distinct titles (in the sense of
  @(method TEI-info<%> get-title)).
  Duplicates will be ignored in an unspecified manner.

  Currently, three types of @deftech{search backend} are supported:
  @itemlist[@item{
               A value of @racket[#f] specifies using a simplistic
               regular-expression-based backend implemented in pure
               Racket.}
            @item{
               Using @racket['noop] as a backend will cause
               @(method corpus% •term-search) to always return 
               @racket['()]}
            @item{
               A @racket[postgresql-data-source/c] value will construct a
               backend using PostgreSQL's full-text search feature by
               connecting to the given database. @bold{Note that initializing
                a corpus will perform destructive modifications to the database.}
               The specified database should be dedicated completely to use by
               the constructed @tech{corpus} object: it should not be relied upon for
               other purposes, and multiple @tech{corpus} objects should not
               use the same database at the same time.}]
 }
 @defmethod[#:mode public-final (•term-search [term term/c]
                                              [#:exact? exact? any/c #f]
                                              [#:ricoeur-only? ricoeur-only any/c #t])
            (listof (is-a?/c document-search-results<%>))]{
  Used to implement @racket[term-search].
 }
 @defmethod[#:mode public-final (•list-TEI-info) (listof (is-a?/c TEI-info<%>))]{
  Used to implement @racket[list-TEI-info]
 }
 @defmethod[#:mode public-final (•get-checksum-table) (hash/c string?
                                                              string?
                                                              #:immutable #t)]{
  Used to implement @racket[get-checksum-table]
 }
 @defmethod[#:mode pubment (on-initialize [docs (listof (is-a?/c TEI<%>))]) any]{
  This method is called exactly once, at the end of @racket[corpus%]'s
  portion of @(this-obj)'s initialization.
  Attempting to call @(method corpus% on-initialize) directly will
  raise an exception.
  
  The argument given to @(method corpus% on-initialize) is the list of
  @racket[TEI<%>] objects encapsulated by @(this-obj).
  The implementation of @racket[corpus%] guarantees that each item
  in @racket[docs] will have a unique result for @(method TEI-info<%> get-title).

  The purpose of @(method corpus% on-initialize) is to provide a hook for subclasses
  of @racket[corpus%] to access the full list of @racket[TEI<%>] objects,
  without having @racket[corpus%] itself cause them remain reachable after
  initialization.

  @italic{Default implementation:} Does nothing.
 }
}

@defthing[postgresql-data-source/c contract?]{
 A contract recognizing values created using @racket[postgresql-data-source]
 with sufficient arguments
 (i.e. at least @racket[#:database] and @racket[#:user])
 to be able to be used with @racket[dsn-connect]
 without needing to supply any additional arguments.
}

@defthing[empty-corpus (is-a?/c corpus%)]{
 A corpus containing no documents
}

@defclass[directory-corpus% corpus% ()]{
 Extends @racket[corpus%] for the common case when you want to
 use documents from some directory in the filesystem.

 @defconstructor[([path (and/c path-string? directory-exists?)]
                  [search-backend (or/c #f postgresql-data-source/c) #f])]{
  Constructs a corpus from every file in @racket[path],
  including recursive subdirectories,
  that is recognized by @racket[xml-path?].
  If any such file is not a valid and well-formed TEI XML file
  satisfying Digital Ricœur's specification, it will be silently ignored.

  The @racket[search-backend] argument controls the @tech{search backend}
  as with @racket[corpus%].
 }
 @defmethod[#:mode public-final (get-path)
            path-string?]{
  Returns the path used to instantiate @(this-obj)
 }
}



@subsection{Search}

@deftogether[
 (@defproc[(term-search [term term/c]
                        [#:ricoeur-only? ricoeur-only? any/c #t]
                        [#:exact? exact? any/c #f])
           (listof (is-a?/c document-search-results<%>))]
   @defthing[term/c flat-contract?
             #:value (and/c non-empty-string? (not/c #px"^\\s*$"))])]{
 Searches the @tech{corpus} object determined by @racket[current-corpus]
 for @racket[term]

 When @racket[ricoeur-only?] is non-false, @racket[term-search]
 omits results from passages not by Paul Ricœur.
 When @racket[exact?] is non-false, only matches containing @racket[term]
 exactly (but in a case-insensitive manner that normalizes whitespace)
 are returned. Otherwise (and by default), the @tech{search backend}
 attempts to match lexical variants of the given @racket[term],
 though the details of this behavior are backend-specific and unspecified.
}

@definterface[document-search-results<%> (TEI-info<%>)]{
 The results of @racket[term-search] for each document in the @tech{corpus}
 are encapsulated in a @deftech{document search results} object.
 @margin-note{The @racket[document-search-results<%>]
  interface contains additional,
  private methods which prevent it from being implemented by clients of
  this library.}
 @defmethod[(get-results) (listof search-result?)]{
  Returns the @tech{search result} values contained by @(this-obj).
 }
 @defmethod[(count-results) natural-number/c]{
  Returns the number of @tech{search result} values contained by @(this-obj)
  in a way that is cached across repeated calls
 }
}
@deftogether[
 (@defproc[(document-search-results-title [dsr (is-a?/c document-search-results<%>)])
           string?]
   @defproc[(document-search-results-results [dsr (is-a?/c document-search-results<%>)])
            (listof search-result?)]
   @defform[#:kind "match expander"
            (document-search-results title-pat results-pat
                                     optional-pat ...)
            #:grammar ([optional-pat
                        (code:line #:date date-pat)
                        (code:line #:count count-pat)
                        (code:line #:citation cite-pat)])])]{
 Alternative means of extracting information from
 @tech{document search results} objects.
}

@deftogether[
 (@defproc[(search-result? [v any/c]) any/c]
   @defform[#:kind "match expander"
            (search-result kw-pat ...)
            #:grammar ([kw-pat
                        (code:line #:excerpt excerpt-pat)
                        (code:line #:location-stack loc-pat)
                        (code:line #:author author-pat)
                        (code:line #:page page-pat)])]
   @defproc[(search-result-excerpt [v search-result?])
            (maybe/c non-empty-string?)]
   @defproc[(search-result-page [v search-result?])
            (or/c (maybe/c string?)
                  (list/c (maybe/c string?) (maybe/c string?)))]
   @defproc[(search-result-location-stack [v search-result?])
            location-stack/c]
   @defproc[(search-result-author-string [v search-result?])
            string?]
   @defproc[(search-result<? [a search-result?] [b search-result?])
            any/c]
   @defproc[(search-result>? [a search-result?] [b search-result?])
            any/c])]{
 A @deftech{search result} value represents an individual result found
 by @racket[term-search]. It will have an excerpt unless copyright
 restrictions apply and a page number string unless the corresponding
 page was not numbered.

 The comparison functions @racket[search-result<?] and
 @racket[search-result>?] report the ordering of the results in the
 document from which they come. The results are only meaningful when
 both @tech{search result} values come from the same
 @tech{document search results} object.
}

@defthing[location-stack/c flat-contract?]{
 A contract similar to @racket[(listof location-stack-entry/c)],
 but which also enforces that any @racket['note] entries come
 before any @racket['div] entries and any @racket['div] entries
 come before any @racket['front] or @racket['back] entries:
 that is, that the location information added by the innermost
 element comes first.
}

@defthing[location-stack-entry/c flat-contract?
          #:value
          (or/c 'front 'back
                (list/c 'div
                        (or/c "chapter" "part" "section" "dedication"
                              "contents" "intro" "bibl" "ack" "index")
                        (maybe/c string?))
                (list/c 'note
                        (or/c "foot" "end")
                        string?
                        (or/c "transl" #f)))]{
 A contract recognizing values that can be supplied by
 @(xmethod TEI-body<%> to-pre-segments/add-metadata) to
 provide details about the location of a @racket[pre-segment] in a document.
}









@include-section["object-system.scrbl"]




@section{XML and X-Expression Operations}

@defproc[(xml-path? [pth path-string?]) any/c]{
 Tests whether @racket[pth] is syntactically a path to an XML file, without
 checking the validity of the file or even its existance.
}

@subsection{X-Expression Contracts}

@defthing[any-tei-xexpr/c flat-contract?]{
 Similar to @racket[(and/c list? xexpr/c)], but
 rejects some (not all) x-expressions that would break TEI
 validity rules, including the additional requirements
 imposed by Digital Ricœur.
}

@defproc[(tei-xexpr/c [name tei-element-name/c])
         flat-contract?]{
 Produces a contract similar to @racket[any-tei-xexpr/c], but
 which recognizes only tags named @racket[name].
}

@defthing[tei-element-name/c flat-contract?]{
 A contract recognizing the names of valid Digital Ricœur TEI XML element.
}

@subsection{@tt{xmllint}-based Operations}
@defmodule[ricoeur/tei/xmllint
           #:no-declare]
@(declare-exporting ricoeur/tei/xmllint
                    ricoeur/tei)


The functions documented in this section are provided
by both @racketmodname[ricoeur/tei] and
@racketmodname[ricoeur/tei/xmllint].

They depend on the 
external command-line utility @tt{xmllint} (which is part
of @tt{libxml2}) to work. If @tt{xmllint} can not be found,
a warning is logged to @racket[(current-logger)].

@defproc[(xmllint-available?) any/c]{
 Detects whether @tt{xmllint} is available at runtime.
}

@defproc[(valid-xml-file? [#:quiet? quiet? any/c #t]
                          [pth path-string?] ...+)
         boolean?]{
 Checks that every @racket[pth] is a valid XML file.

 When @racket[quiet?] is @racket[#f], writes any
 validation error messages (from @tt{xmllint}) to
 @racket[current-error-port].

 If @tt{xmllint} is not available, always returns @racket[#t].
}

@defproc[(directory-validate-xml [dir (and/c path-string? directory-exists?)]
                                 [#:quiet? quiet? any/c #f])
         boolean?]{
 Checks that every path ending in @litchar{.xml}
 in @racket[dir] and its recursive subdirectories
 is a valid XML file.

 When @racket[quiet?] is @racket[#f], writes any
 validation error messages (from @tt{xmllint}) to
 @racket[current-error-port].

 If @tt{xmllint} is not available, always returns @racket[#t].
}

@defproc[(call/prettyprint-xml-out [thunk (-> any/c)])
         any/c]{
 If @tt{xmllint} is not available, equivalent to @racket[(thunk)].

 When @tt{xmllint} is available, @racket[thunk] is called in
 a context where everything written to the @racket[current-output-port] is
 piped through @tt{xmllint}'s prettyprint function before being written to
 the original @racket[current-output-port].
 When prettyprinting succeeds, the result of @racket[call/prettyprint-xml-out]
 is the result of @racket[thunk].
 If prettyprinting fails (perhaps because the output of @racket[thunk] was
 not well-formed XML), @bold{@tt{xmllint} may still write to the
  original @racket[current-output-port]}, but @racket[call/prettyprint-xml-out]
 raises an exception rather than returning a value.
 If @racket[thunk] raises an exception and @tt{xmllint} is available,
 @tt{xmllint} is never invoked and nothing is written to the original
 @racket[current-output-port].
}






@section{Implementation Details}

@subsection{Contract Implementation}
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
  @;TODO: Add more detail

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






@subsection{Search Implementation}
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








@section{Installing & Updating This Library}

@margin-note{Installing this library will also install
 the tools documented under
 @secref["Tools"
         #:doc '(lib "ricoeur/tei/scribblings/guidelines.scrbl")].}
            
To use this library, you must install the Racket programming
language and runtime system for your platform from
@url["https://racket-lang.org"]. Racket version 6.11 or later
is currently required. If you use Mac OS, you are free to use the 
"cask" for the Homebrew packacge manager (but note that the
"minimal-racket" Homebrew formula is currently unmaintained and
should be avoided), and, if you use Ubuntu, you may wish to
consider the Racket
@hyperlink["https://launchpad.net/~plt/+archive/ubuntu/racket"]{PPA}.
Otherwise, you generally should @italic{not} use the version
of Racket from your OS's package manager,
as it will generally not be up-to-date.

You should also configure your @envvar{PATH} environment variable
so that the @tt{racket} and @tt{raco} programs can be run
from the command line. For example, on Mac OS, you should add a
line like the following to @filepath{~/.bash_profile}:
@verbatim[#:indent 2]{export PATH="/Applications/Racket v6.11/bin":$PATH}

While it is not strictly required, some features of this library
are implemented using the utility @tt{xmllint} from @tt{libxml2}.
This is included by default with Mac OS and is available via
the system package manager on GNU/Linux.
On Windows, the necessary binaries are provided as a platform-specific
dependency through the Racket package system.
@margin-note{Specifically, binaries are provided for platforms where
 @racket[(matching-platform? "win32\\x86_64")] returns @racket[#t].}

To install this library, you must first obtain a copy of the source
code by cloning its git repository from
@url["https://bitbucket.org/digitalricoeur/tei-utils"].
You then must install it as a Racket package. On platforms which
provide the utility @tt{make}, this can be done by running
@exec{make install} from the directory into which you have cloned
the repository.

Later, you can install updated versions of the repository simply
by running @exec{make}, which also handles pulling updates from
the server for you. More substantial changes may occasionally
require you to reinstall the package by running
@exec{make reinstall}.
