#lang scribble/manual

@title{Digital Ricœur TEI Library}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com")]
@defmodule[#:multi (ricoeur/tei
                    ricoeur/tei/base)]


@(require "for-manual.rkt")

This manual documents a Racket library for
working with Digital Ricœur's TEI XML documents.
In addition to being valid and well-formed XML, the
documents should conform to the structure specified in
@(guidelines-doc).
It also documents Digital Ricœur's @secref["_Term_Search__Tool"].

This document is written for programmers intending either to
use this library in their own programs
or to contribute to its implementation.
It assumes familiarity with the concept of @deftech{x-expressions}
from Racket's @racketmodname[xml] library
(see the grammar documented under @racket[xexpr?]).

The @racketmodname[ricoeur/tei] library provides all of the bindings
documented in this manual, except where otherwise noted.
The @racketmodname[ricoeur/tei/base] library provides most of
@racketmodname[ricoeur/tei], but omits some derived functionality
to achieve a slightly faster startup time, making it particularly
suitable for command-line scripts.

In addition to the bindings implemented as part of this library,
both @racketmodname[ricoeur/tei] and @racketmodname[ricoeur/tei/base]
re-export everything from @racketmodname[adjutor],
@racketmodname[gregor], and @racketmodname[data/maybe],
as well as
@racket[fmap] (the version of @racketidfont{map} from @racketmodname[data/functor])
and @racket[comment?], @racket[p-i?], @racket[valid-char?],
and @racket[cdata?] from @racketmodname[xml].

@margin-note{
 Not part of Digital @|Ricoeur|? Welcome!
 You probably want to read about the
 @secref["Status_of_This_Library"] before going much further.
}

@(table-of-contents)

@include-section["installing.scrbl"] @; Installing
@include-section["instance-info.scrbl"] @; Bibliographic Information
@include-section["corpus.scrbl"] @; High-level Corpus Functionality
@include-section["term-search.scrbl"] @; ``Term Search'' Tool
@include-section["document.scrbl"] @; Document-level Functions
@include-section["segment.scrbl"] @; Segments
@include-section["element.scrbl"] @; TEI Element Representation
@include-section["xexpr+xml.scrbl"] @; X-Expression and XML Operations
@include-section["general.scrbl"] @; General Utilities
@include-section["implementation.scrbl"] @; Implementation Details




