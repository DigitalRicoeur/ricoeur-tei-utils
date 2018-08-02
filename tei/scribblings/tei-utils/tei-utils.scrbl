#lang scribble/manual

@title[#:version ""]{Digital Ricœur TEI Library}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com")]
@defmodule[#:multi (ricoeur/tei ricoeur/tei/base)]


@(require "for-manual.rkt"
          )

This manual documents a Racket library for
working with TEI XML documents developed for Digital Ricœur.
In addition to being valid and well-formed XML, the
documents should conform to the structure specified in
@(guidelines-doc).

This document is written for programmers intending either to
use this library in their own programs
or to contribute to its implementation.
It assumes familiarity with the concept of @deftech{x-expressions}
from Racket's @racketmodname[xml] library
(see the grammar documented under @racket[xexpr?]).

The @racketmodname[ricoeur/tei] library provides all of the bindings
documented in this manual (except where otherwise noted).
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

@(table-of-contents)

@include-section["installing.scrbl"] @; Installing
@include-section["corpus.scrbl"] @; High-level Corpus Functionality
@include-section["instance-info.scrbl"] @; Bibliographic Information
@include-section["document.scrbl"] @; Document-level Functions
@include-section["segment.scrbl"] @; Segments
@include-section["element.scrbl"] @; TEI Element Representation
@include-section["xexpr+xml.scrbl"] @; X-Expression and XML Operations
@include-section["general.scrbl"] @; General Utilities
@include-section["implementation.scrbl"] @; Implementation Details

@section{Temporary Staging Area}
@defthing[prop:element->plain-text (struct-type-property/c
                                    (or/c (-> tei-element? string?)
                                          (-> tei-element? boolean? string?)))]{
 The definition of a @tech{tei element struct type} can use
 @racket[prop:element->plain-text] to override the default
 behavior of @racket[element-or-xexpr->plain-text].
 Note that attaching @racket[prop:element->plain-text] to
 unrelated struct types has no effect: it is only used
 for @tech{tei element structs}.

 The first argument to the function given as the property value
 is always the @tech{tei element struct} instance to be converted
 to plain text.
 If the function given as the property value accepts a second argument,
 it will be a boolean corresponding to the @racket[#:include-header?]
 argument to @racket[element-or-xexpr->plain-text].
                                                         
 @TODO/scrbl[[prop:element->plain-text |shouldn't| be documented here.]]{
  @bold{TODO:} Document this somewhere else.}
}

