#lang scribble/manual

@title[#:version ""]{TEI Utilities}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com")]
@defmodule[ricoeur/tei]

@(require (for-label racket
                     xml
                     data/maybe
                     ricoeur/tei
                     ricoeur/tei/xmllint
                     ))

This manual documents utilities and Racket libraries for
working with TEI XML files, developed for Digital Ricœur.


@defproc[(make-ricoeur-teiHeader
          [#:title title string?]
          [#:authors authors (non-empty-listof (tei-xexpr/c 'author))
           '((author "Paul Ricoeur"))]
          [#:editors editors (listof (tei-xexpr/c 'editor))]
          [bibl-body string?] ...+)
         (tei-xexpr/c 'teiHeader)]{
 Produces an x-expression for a @tt{teiHeader} element acording to
 the template for Digital Ricoeur. The @racket[bibl-body] strings
 (which may be convieniently given using the at-reader) are
 spliced into a @tt{bibl} element in the @tt{sourceDesc} element.
}

@defproc[(read-TEI [in input-port? (current-input-port)])
         (is-a?/c element<%>)]{
 Produces a Racket object representing the TEI XML
 document read from @racket[in]. If the document is
 valid, the resulting object will implement @racket[TEI.2<%>].
}

@defproc[(tag->element [tag any-tei-xexpr/c])
         (is-a?/c element<%>)]{
 Converts a TEI XML tag, represented as an x-expression,
 to a Racket object.
}

@definterface[element<%> ()]{
 TEI elements are represented by Racket objects implementing
 the @racket[element<%>] interface.

 @defmethod[(to-xexpr) xexpr/x]{
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


@definterface[TEI-info<%> (element<%>)]{
 This interface defines methods for accessing the catalog
 information about a TEI document

 @defmethod[(get-title) string?]{
  Returns the title of the document.
 }
}


@definterface[TEI.2<%> (element<%> TEI-info<%>)]{
 This interface defines additional methods for the top-level
 @litchar{<TEI.2>}@tt{...}@litchar{</TEI.2>} element.

 @defmethod[(guess-paragraphs) (is-a?/c TEI.2<%>)]{
  Returns an object like @(this-obj), but having attempted to
  infer paragraph breaks in the text.
 }

 @defmethod[(get-teiHeader) (is-a?/c teiHeader<%>)]{
  Returns an object representing the @tt{teiHeader} element
  from the document
 }

 @defmethod[(write-TEI [out output-port? (current-output-port)])
            any]{
  Writes the XML representation of @(this-obj) to @racket[out].

  Use @(method TEI.2<%> write-TEI) rather than 
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

@defmethod[(get-number) (maybe/c string?)]{
Returns an optional value containing the original string
from the element's @litchar{n} attribute or @racket[nothing]
if none was present.
 }

@defmethod[(interpret-number) (or/c #f
                                    (cons/c (or/c 'number 'roman) number?)
                                    (cons/c #f string?))]{
  Attempts to interpret the element's @litchar{n} attribute.
 }
}

@section{Contracts}

@defthing[any-tei-xexpr/c flat-contract?]{
 Similar to @racket[(and/c list? xexpr/c)], but
 rejects some (not all) x-expressions that would break TEI
 validity rules.
}

@defproc[(tei-xexpr/c [name symbol?])
         flat-contract?]{
 Produces a contract similar to @racket[any-tei-xexpr/c], but
 which recognizes only tags named @racket[name].
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

@section{XML Validation}
@defmodule[ricoeur/tei/xmllint
           #:no-declare]
@(declare-exporting ricoeur/tei/xmllint
                    ricoeur/tei
                    )


The functions documented in this section are provided
by both @racketmodname[ricoeur/tei] and
@racketmodname[ricoeur/tei/xmllint].

They depend on the 
external command-line utility @tt{xmllint} (which is part
of libxml2) to work. If @tt{xmllint} can not be found,
a warning is displayed.

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

