#lang scribble/manual

@title[#:version ""]{Object System}
@declare-exporting[ricoeur/tei/oop]

@(require (for-label (except-in racket
                                date
                                date?)
                     racket/unit
                     xml
                     data/maybe
                     ricoeur/tei/oop
                     (submod ricoeur/tei/search/common private)
                     gregor
                     ))

@nested[#:style 'inset]{
 @bold{WARNING:} The bindings documented in this section
 are provided by @racketmodname[ricoeur/tei/oop],
 but @bold{not} @racketmodname[ricoeur/tei].
 Backwards-incompatible changes are expected during
 the ongoing revision of this library.
}

Internally, this library represents TEI XML documents
as nested objects (from Racket's class-based object system),
which form a thin layer of abstraction over plain x-expressions.

@defproc[(file->TEI [file (and/c path-string? file-exists?)])
         (is-a?/c TEI<%>)]{
 Produces a Racket object representing the TEI XML document @racket[file].
}

@defproc[(read-TEI [in input-port? (current-input-port)]
                   [#:filename filename (or/c #f path-string?) #f])
         (is-a?/c TEI<%>)]{
 Produces a Racket object representing the TEI XML
 document read from @racket[in].

 If a @racket[filename] is provided, it is used (without the
 directory portion) for @(method TEI-info<%> get-filename).
}

@defproc[(tag->element [tag any-tei-xexpr/c]
                       [#:filename filename (or/c #f path-string?) #f])
         (is-a?/c element<%>)]{
 Converts a TEI XML tag, represented as an x-expression,
 to a Racket object.

 If @racket[filename] is provided, it is used as with @racket[read-TEI]
 when the resulting object implements @racket[TEI-info<%>].
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





@deftogether[
 (@defproc[(maybe-date<? [a (maybe/c date?)]
                         [b (maybe/c date?)])
           any/c]
   @defproc[(maybe-date>? [a (maybe/c date?)]
                          [b (maybe/c date?)])
            any/c])]{
 Comparison functions on optional date values.
 @; Should this be here at all ???
}




@include-section["oop-interfaces/general.scrbl"]
@include-section["oop-interfaces/specific.scrbl"]




