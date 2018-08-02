#lang scribble/manual

@title{Document-level Functions}
@(declare-exporting ricoeur/tei/base
                    ricoeur/tei)

@(require "for-manual.rkt")

@(TODO/void Add prose: relationship btwn TEI documents and instances)

@defpredicate[tei-document?]{
 Recognizes @deftech{TEI document} values.

 A @tech{TEI document} is a @tech{tei element struct}
 that represent the root @tag{TEI} element of a document.
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

@defproc[(file->tei-document [file (and/c path-string? file-exists?)])
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



@section{Paragraph Inference}

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
 has been performed for the document represented by
 @racket[doc].
}



