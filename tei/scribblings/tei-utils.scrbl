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
                     (only-in ricoeur/tei/search
                              search-documents
                              searchable-document?
                              prepare-searchable-document)
                     gregor
                     ))

This manual documents utilities and Racket libraries for
working with TEI XML files developed for Digital Ricœur.
In addition to being valid and well-formed XML, the
documents should conform to the structure specified in
@other-doc['(lib "ricoeur/tei/scribblings/guidelines.scrbl")].

@(table-of-contents)

@section{High-level Interface}

@defparam[current-corpus corpus (is-a?/c corpus%)
          #:value empty-corpus]{
 This library uses @deftech{corpus} objects to encapsulate
 collections of TEI documents. High-level functions, such
 as @racket[term-search], invoke methods of the object in
 the @racket[current-corpus] parameter.

 In practice, this parameter should usually be initialized
 with a @racket[directory-corpus%] instance.
}

@defclass[corpus% object% ()]{
 The root class for all @tech{corpus} objects.

 Note that creating a new instance of this class involves a
 fair amount of overhead, so creating redundant values should be avoided.
 (This also improves search performance through caching, for example.)
 
 @defconstructor[([docs (listof (is-a?/c TEI<%>))])]{
  Constructs an instance encapsulating the TEI documents @racket[docs]
 }
@defmethod[(•term-search [term term/c])
           (listof (is-a?/c document-search-results<%>))]{
Used to implement @racket[term-search].
 }
}

@defthing[empty-corpus (is-a?/c corpus%)]{
A corpus containing no documents
}

@defclass[directory-corpus% corpus% ()]{
 Extends @racket[corpus%] for the common case when you want to
 use documents from some directory in the filesystem.

@defconstructor[([path (and/c path-string? directory-exists?)])]{
  Constructs a corpus from every file in @racket[path],
  including recursive subdirectories,
  that is recognized by @racket[xml-path?].
  If any such file is not a valid and well-formed TEI XML file
  satisfying Digital Ricœur's specification, an error will occur.
 }
@defmethod[#:mode public-final (get-path)
           path-string?]{
  Returns the path used to instantiate @(this-obj)
 }
}



@subsection{Search}

@deftogether[
 (@defproc[(term-search [term term/c])
           (listof (is-a?/c document-search-results<%>))]
   @defthing[term/c flat-contract?
             #:value (and/c non-empty-string? (not/c #px"^\\s*$"))])]{
 Searches the @tech{corpus} object determined by @racket[current-corpus]
 for @racket[term]
}

@definterface[document-search-results<%> (TEI-info<%>)]{
 The results of @racket[term-search] for each document in the @tech{corpus}
 are encapsulated in a @deftech{document search results} object.
@margin-note{The @racket[document-search-results<%>] contains additional,
  private methods which prevent it from being implemented by clients of
  this library.}
 @defmethod[(get-results) (listof search-result?)]{
  Returns the @tech{search result} values contained by @(this-obj).
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
                        (code:line #:citation cite-pate)])])]{
Alternative means of extracting information from
@tech{document search results} objects.
}

@deftogether[
 (@defproc[(search-result? [v any/c]) any/c]
   @defform[#:kind "match expander"
            (search-result kw-pat ...)
            #:grammar ([kw-pat
                        (code:line #:excerpt excerpt-pat)
                        (code:line #:page-string page-string-pat)])]
   @defproc[(search-result-excerpt [v search-result?])
            (maybe/c string?)]
   @defproc[(search-result-page-string [v search-result?])
            (maybe/c string?)]
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
@racketmodname[ricoeur/tei].

@deftogether[(@defproc[(prepare-searchable-document [doc (is-a?/c TEI<%>)])
                       searchable-document?]
               @defproc[(searchable-document? [v any/c]) any/c])]{
A searchable document is an opaque value encapsulating some work
that needs to be done to prepare a TEI document to be searched.
It is a Racket object implementing the @racket[TEI-info<%>] interface.
}

@defproc[(search-documents [term term/c]
                           [docs (listof searchable-document?)])
         (listof (is-a?/c document-search-results<%>))]{
Used to implement @racket[term-search]
}



