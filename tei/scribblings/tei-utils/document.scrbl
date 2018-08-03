#lang scribble/manual

@title{Document-level Functions}
@(declare-exporting ricoeur/tei/base
                    ricoeur/tei)

@(require "for-manual.rkt")

@(TODO/void Add prose: relationship btwn TEI documents and instances)

@defpredicate[tei-document?]{
 Recognizes @deftech{TEI document} values.

 A @tech{TEI document} is a @tech{tei element struct}
 that represents the root @tag{TEI} element of a document.
 TEI document values implement the @tech{instance info} interface
 for bibliographic information.
}

@defproc[(tei-document-checksum [doc tei-document?])
         symbol?]{
 Returns a checksum calculated based on
 a standardized XML representation of @racket[doc].
 The checksum is returned as a symbol to facilitate inexpensive
 comparisons.
}


@section{Reading & Writing TEI Documents}

@defproc[(file->tei-document [file (and/c path-string-immutable/c
                                          file-exists?)])
         tei-document?]{
 Produces a @tech{TEI document} value representing
 the TEI XML document @racket[file].
}

@defproc[(read-tei-document [in input-port? (current-input-port)])
         tei-document?]{
 Produces a @tech{TEI document} value representing the TEI XML
 document read from @racket[in].
}

@defproc[(write-tei-document [doc tei-document?]
                             [out output-port? (current-output-port)])
         any]{
 Writes the XML representation of @racket[doc] to out,
 prettyprinted using @racket[call/prettyprint-xml-out].

 Use @racket[write-tei-document] rather than
 @racket[(write-xexpr (tei-element->xexpr doc) out)]
 because @racket[write-tei-document] also writes
 an XML declaration and appropriate DOCTYPE declaration.
}

@defproc[(element-or-xexpr->plain-text [v (or/c tei-element? raw-xexpr-atom/c)]
                                       [#:include-header? include-header? any/c #t])
         string-immutable/c]{
 Converts @racket[v] to a plain-text string.
 The resulting string is @bold{not} the XML representation of @racket[v]:
 it is formated for uses that expect unstructured plain text.

 When @racket[include-header?] is non-false (the default),
 the resulting string may include more than just the content of @racket[v]:
 for example, if @racket[v] is a @tech{TEI document}, the string may begin
 with the title of the corresponding @tech{instance}.
 When @racket[include-header?] is @racket[#false], only the content
 will be included, which is sometimes preferable if the plain text form
 is intended for further processing by computer.

 While @racket[element-or-xexpr->plain-text] accepts any
 @tech{TEI element struct} or non-element @tech{raw x-expression},
 it is documented here because it is most useful
 with @tech{TEI document} values.

 For implementation details, see @racket[prop:element->plain-text].
}



@section{Paragraph Inference}

@(TODO/void Describe Paragraph Inference)

@deftogether[
 @(@defproc[(tei-document-paragraphs-status [doc tei-document?])
            guess-paragraphs-status/c]
    @defthing[guess-paragraphs-status/c flat-contract?
              #:value (or/c 'todo
                            'line-breaks
                            'blank-lines
                            'done
                            'skip)])]{
 Returns a symbol indicating whether paragraph-guessing
 has been performed for the @tech{TEI document} represented by
 @racket[doc].

 A value of @racket['todo] means that paragraph-guessing
 has not been performed and should be done as soon as possible.
 A value of @racket['skip] means that paragraph-guessing
 has been intentionally postponed, perhaps because the current
 strategies have not proven effective for @racket[doc].

 The values @racket['line-breaks], @racket['blank-lines], and
 @racket['done] all mean that paragraph-guessing has been completed
 successfully: @racket['line-breaks] and @racket['blank-lines]
 indicate the strategy by which paragraphs were infered,
 whereas @racket['done] is a legacy value indicating that
 paragraph-guessing was performed before this library began
 recording which strategy was used.
}

@defproc[(tei-document/paragraphs-status/c [status/c flat-contract?])
         flat-contract?]{
 Produces a contract recognizing @tech{TEI document} values
 (i.e. those recognized by @racket[tei-document?]) for which
 the result of @racket[tei-document-paragraphs-status] would
 satisfy the contract @racket[status/c].
}

@defproc[(tei-document-skip-guess-paragraphs
          [doc (tei-document/paragraphs-status/c 'todo)])
         (tei-document/paragraphs-status/c 'skip)]{
 Returns a new @tech{TEI document} value like @racket[doc],
 but with an annotation that paragraph-guessing has
 been intentionally skipped for this document.
}

@defproc[(tei-document-unskip-guess-paragraphs
          [doc (tei-document/paragraphs-status/c 'skip)])
         [doc (tei-document/paragraphs-status/c 'todo)]]{
 Returns a new @tech{TEI document} value like @racket[doc],
 but annotated to indicate that paragraph-guessing should
 be performed as soon as possible.
}

@defproc[(tei-document-guess-paragraphs
          [doc (tei-document/paragraphs-status/c
                (or/c 'todo 'skip))]
          [#:mode mode (or/c 'line-breaks 'blank-lines) 'blank-lines])
         (tei-document/paragraphs-status/c mode)]{
 Returns a new @tech{TEI document} value like @racket[doc],
 but with paragraphs infered based on the strategy @racket[mode].
}






