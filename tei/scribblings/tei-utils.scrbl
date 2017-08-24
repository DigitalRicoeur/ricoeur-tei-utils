#lang scribble/manual

@title[#:version ""]{Digital Ricœur TEI Library}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com")]
@defmodule[ricoeur/tei]

@(require (for-label (except-in racket
                                date
                                date?)
                     racket/unit
                     xml
                     data/maybe
                     ricoeur/tei
                     ricoeur/tei/signatures
                     db
                     json
                     (only-in ricoeur/tei/search
                              search-documents
                              searchable-document-set?
                              regexp-searchable-document-set
                              postgresql-searchable-document-set)
                     (submod ricoeur/tei/search/common private)
                     gregor
                     ))

This manual documents utilities and Racket libraries for
working with TEI XML documents developed for Digital Ricœur.
In addition to being valid and well-formed XML, the
documents should conform to the structure specified in
@other-doc['(lib "ricoeur/tei/scribblings/guidelines.scrbl")].

This document is written for programmers intending to either
use these libraries in their own programs (in the case particularly
of @secref["High-level_Interface"] and, to a lesser extent,
@secref["Object_System"]) or to contribute to their implementation.
It assumes familiarity with Racket's class-based system for
object-oriented programming, @racketmodname[racket/class],
and to a lesser extent with Racket's @racketmodname[xml] library,
particularly the concept of x-expressions
(see the grammar documented under @racket[xexpr?]).

@(table-of-contents)

@section{High-level Interface}

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

@defclass[corpus% object% ()]{
 The root class for all @tech{corpus} objects.

 Note that creating a new instance of this class involves a
 fair amount of overhead, so creating redundant values should be avoided.
 (This also improves search performance through caching, for example.)
 
 @defconstructor[([docs (listof (is-a?/c TEI<%>))]
                  [search-backend (or/c #f postgresql-data-source/c) #f])]{
  Constructs a @tech{corpus} object encapsulating the TEI documents @racket[docs]
  which uses the @tech{search backend} specified by @racket[search-backend]
  to implement @racket[term-search].

  Currently, two types of @deftech{search backend} are supported:
  @itemlist[@item{
               A value of @racket[#f] specifies using a simplistic
               regular-expression-based backend implemented in pure
               Racket.}
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
                                              [#:ricoeur-only ricoeur-only any/c #t])
            (listof (is-a?/c document-search-results<%>))]{
  Used to implement @racket[term-search].
 }
 @defmethod[#:mode public-final (•list-TEI-info) (listof (is-a?/c TEI-info<%>))]{
  Used to implement @racket[list-TEI-info]
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
  satisfying Digital Ricœur's specification, an error will occur.

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
 (@defproc[(term-search [term term/c] [#:ricoeur-only ricoeur-only any/c #t])
           (listof (is-a?/c document-search-results<%>))]
   @defthing[term/c flat-contract?
             #:value (and/c non-empty-string? (not/c #px"^\\s*$"))])]{
 Searches the @tech{corpus} object determined by @racket[current-corpus]
 for @racket[term]
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
                        (code:line #:page page-pat)])]
   @defproc[(search-result-excerpt [v search-result?])
            (maybe/c string?)]
   @defproc[(search-result-page [v search-result?])
            (or/c (maybe/c string?)
                  (list/c (maybe/c string?) (maybe/c string?)))]
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

@subsection{XML Validation}
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








@section{Implementation Details}

@subsection{Contract Implementation}
@defmodule[ricoeur/tei/signatures]

The bindings documented in this section are provided by
@racketmodname[ricoeur/tei/signatures], but not by
@racketmodname[ricoeur/tei].
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
                                 [#:required-attrs required-attrs (listof symbol?) '()])
          flat-contract?]{
  Creates a contract recognizing a particular Digital Ricœur TEI XML element.
  Used by units implementing @racket[element-contracts^].
  @;TODO: Add more detail
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
                             [#:meta meta jsexpr?]
                             [#:excerpt excerpt (maybe/c string?)])
         search-result?]{
 Constructs a @tech{search result} value. The @racket[counter] and
 @racket[meta] arguments should be drawn from the fields of the corresponding
 @racket[pre-segment]. The @racket[sub-counter] argument should indicate the
 position of the @tech{search result} value relative to other
 @tech{search result} values for the same query that are drawn from the same
 @racket[pre-segment]: it is used to implement fuctions like
 @racket[search-result<?].
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
                                        [meta jsexpr?]
                                        [resp string?])
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
 @defmethod[(do-search-documents [term term/c] [#:ricoeur-only? ricoeur-only? any/c #t])
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
@url["https://racket-lang.org"]. Racket version 6.10 or later
is currently required. If you use Mac OS, you are free to use the 
"cask" for the Homebrew packacge manager (but note that the
"minimal-racket" Homebrew formula is currently unmaintained and
should be avoided), and, if you use Ubuntu, you may wish to
consider the Racket
@hyperlink["https://launchpad.net/~plt/+archive/ubuntu/racket"]{PPA}.
Otherwise, you generally should @italic{not} use the version
of Racket from your OS's package manager,
as it will generally not be up-to-date.

You must also configure your @envvar{PATH} environment variable
so that the @tt{racket} and @tt{raco} programs can be run
from the command line. For example, on Mac OS, you should add a
line like the following to @filepath{~/.bash_profile}:
@verbatim[#:indent 2]{export PATH="/Applications/Racket v6.10/bin":$PATH}

While it is not strictly required, some features of this library
are implemented using the utility @tt{xmllint} from @tt{libxml2}.
This is included by default with Mac OS and is available via
the system package manager on GNU/Linux; it can also be installed 
on Windows.

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
