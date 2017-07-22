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
                     gregor
                     ))

This manual documents utilities and Racket libraries for
working with TEI XML files developed for Digital Ricœur.

@defproc[(read-TEI [in input-port? (current-input-port)])
         (is-a?/c TEI<%>)]{
 Produces a Racket object representing the TEI XML
 document read from @racket[in]. 
}

@defproc[(xml-path? [pth path-string?]) any/c]{
 Tests whether @racket[pth] is the path to an XML file, without
 checking the validity of the file or even its existance.
}

@defproc[(tag->element [tag any-tei-xexpr/c])
         (is-a?/c element<%>)]{
 Converts a TEI XML tag, represented as an x-expression,
 to a Racket object.
}

@section{General Interfaces}

The interfaces represented in this section contain methods
implemented by broad categories of TEI elements.

@definterface[element<%> ()]{
 TEI elements are represented by Racket objects implementing
 the @racket[element<%>] interface.

 @margin-note{The @racket[element<%>] interface and derived
  interfaces contain additional private methods to preserve
  certain invarialts, and thus may only be implemented by
  the objects provided from @racketmodname[ricoeur/tei].}

 @defmethod[(to-xexpr) xexpr/c]{
  Returns the x-expression representation of @(this-obj).
 }

 @defmethod[(to-plain-text) string?]{
  Converts @(this-obj) to a plain-text string. The resulting
  string is @italic{not} the XML representation of
  @(this-obj): it is formated for uses that expect unstructured
  plain text. 
 }

 @defmethod[(get-name) symbol?]{
  Returns the name of the @(this-obj).
 }

 @defmethod[(get-attributes) (listof (list/c symbol? string?))]{
  Returns the attributes of @(this-obj) using the same
  representation used by x-expressions.
 }

 @defmethod[(get-body) (listof element-or-xexpr/c)]{
  Returns the contents of @(this-obj), with child elements
  represented by objects implementing @racket[element<%>].
 }

}

@defproc[(tei-element? [v any/c]) any/c]{
A predicate recognizing Racket objects that implement the
@racket[element<%>] interface.
}

@defthing[element-or-xexpr/c flat-contract?
          #:value (or/c (is-a?/c element<%>)
                        string?
                        symbol?
                        valid-char?
                        cdata?
                        comment?
                        p-i?)]{
 A contract recognizing objects implementing @racket[element<%>]
 and non-tag x-expressions.
}

@definterface[elements-only<%> (element<%>)]{
Elements implementing this interface may not contain textual
data directly.

@defmethod[#:mode extend
           (get-body) (listof (or/c (is-a?/c element<%>)
                                    comment?
                                    p-i?))]{
  Like @(xmethod element<%> get-body), but the returned list may
  not contain strings, symbols, characters, or @racket[cdata].
 }

 @defmethod[(get-body/elements-only) (listof (is-a?/c element<%>))]{
  Like @(method elements-only<%> get-body), but the resulting list
  contains only the child elements: that is, it will not contain
  comments or processing instructions.
 }
}

         
@definterface[TEI-body<%> (element<%>)]{
This interface is implemented by elements containing (all or part of)
the body of the document. In most cases, these methods should be invoked
on the top-level object implementing @racket[TEI<%>].

@defmethod[(get-page-breaks) (listof (is-a?/c pb<%>))]{
Returns a list of objects representing the page-break elements
recursively contained by @(this-obj)
 }

@defmethod[(smoosh) (listof (or/c string? (is-a?/c pb<%>)))]{
  @bold{This method should be considered private.} While it
  is documented here for completeness, it is inextricably tied
  to the specific implementation of the search feature and is
  subject to change, especially a wider result contract.

  In the basic case, @(method TEI-body<%> smoosh) returns a flattened
  list of the textual content of the recursive children of @(this-obj),
  plus page-break elements. However, the implementation is specialized
  by particular elements to omit content that is not of interest for the
  search feature.
 }
}


@definterface[guess-paragraphs<%> (TEI-body<%>)]{
 Extends @racket[TEI-body<%>] with a method for automatically
 converting child "anonymous block" elements (see @racket[ab<%>])
 to paragraphs. In most cases, this method should be invoked
 on the top-level object implementing @racket[TEI<%>].

 @defmethod[(guess-paragraphs [#:mode mode (or/c 'blank-lines 'line-breaks) 'blank-lines])
            (is-a?/c (object-interface #,(this-obj)))]{
  Returns an object like @(this-obj), but with any recursively conained
  "anonymous block" elements (see @racket[ab<%>]) replaced by
  paragraph and page-break elements (see @racket[p<%>] and @racket[pb<%>]).
  Children of the "anonymous block" elements are preserved.

  When mode is @racket['blank-lines] (the default),
  @(method guess-paragraphs<%> guess-paragraphs) assumes a blank line between
  blocks of text is used to indicate a paragraph; otherwise, it assumes
  every line-break begins a new paragraph. In both cases, entirely empty
  paragraphs are ommited, and freestanding page-breaks are allowed.

  Using @(method guess-paragraphs<%> guess-paragraphs) does not eliminate
  the need for manual review: for example, it will identify as "paragraphs"
  segments of text that should be represented as headings, list items, or
  footnotes. However, it saves a great deal of time for the common cases.
 }
}

@defproc[(guess-paragraphs? [v any/c]) any/c]{
 A predicate recognizing Racket objects that implement the
 @racket[guess-paragraphs<%>] interface
}

@definterface[TEI-info<%> ()]{
 An interface implemented by objects encapsulating information about a
 TEI XML document, notably objects implementing @racket[TEI<%>]
 @racket[teiHeader<%>].

 @margin-note{Unlike other interfaces in this section, @racket[TEI-info<%>]
  is not derived from @racket[element<%>], so it may be freely implemented
  by client modules. However, additional required methods may be added
  to @racket[TEI-info<%>] without warning.}
 
 @defmethod[(get-title) string?]{
  Returns the title of the document, including subtitles.
 }
@defmethod[(get-publication-date) (maybe/c date?)]{
  Returns the publication date of the work, if available
 }
@defmethod[(get-citation) string?]{
  Returns the human-readable citation for the work
 }
}



@section{Element-specific Interfaces}

The interfaces in this section identify objects that correspond to
specific TEI elements. Some add additional element-specific methods;
the others serve merely to identify elements convieniently.

@definterface[TEI<%> (element<%>
                      TEI-info<%>
                      TEI-body<%>
                      guess-paragraphs<%>
                      elements-only<%>)]{
 The object representing the top-level
 @litchar{<TEI>}@tt{...}@litchar{</TEI>} element implements this
 interface.

 @defmethod[(get-teiHeader) (is-a?/c teiHeader<%>)]{
  Returns an object representing the @tt{teiHeader} element
  from the document
 }

 @defmethod[(write-TEI [out output-port? (current-output-port)])
            any]{
  Writes the XML representation of @(this-obj) to @racket[out].

  Use @(method TEI<%> write-TEI) rather than 
  @racket[(write-xexpr (send #,(this-obj) #,(method element<%> to-xexpr))
                       out)] because @(method TEI.2<%> write-TEI)
  also writes an XML declaration and appropriate @tt{DOCTYPE} declaration.
 }
}

@definterface[teiHeader<%> (TEI-info<%>)]{
This interface identifies objects representing
@litchar{<teiHeader>}@tt{...}@litchar{</teiHeader>} elements.
}

@definterface[pb<%> (element<%>)]{
This interface identifies objects representing pagebreak elements.

@defmethod[(get-page-string) (maybe/c string?)]{
Returns a @racket[just] value containing the original string
from the element's @litchar{n} attribute or @racket[nothing]
if none was present.
 }

@defmethod[(get-kind) (or/c 'none 'number 'roman 'other)]{
  Returns a symbol repesenting the kind of page number
 }

@defmethod[(get-numeric) (maybe/c number?)]{
  Returns an optional value representing the page number string converted
  to a number, if possible 
 }
}

@definterface[p<%> (TEI-body<%>)]{
An interface identifying paragraph elements.
}

@definterface[ab<%> (TEI-body<%>)]{
An interface identifying "anonymous block" elements.

@defmethod[(do-guess-paragraphs [#:mode mode (or/c 'blank-lines 'line-breaks)
                                 'blank-lines])
           (listof (or/c (is-a?/c pb<%>)
                         (is-a?/c p<%>)))]{
  Used to implement @(xmethod guess-paragraphs<%> guess-paragraphs)
 }
}


@section{X-Expression Contracts}

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

@subsection{Contract Implementation}
@defmodule[ricoeur/tei/signatures]

The bindings documented in this section are provided by
@racketmodname[ricoeur/tei/signatures], but not by
@racketmodname[ricoeur/tei/signatures].
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


@section{XML Validation}
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

