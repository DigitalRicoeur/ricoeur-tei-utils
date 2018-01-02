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
                     ricoeur/lib
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

@include-section["high-level.scrbl"]
@include-section["object-system.scrbl"]
@include-section["xexpr+xmllint.scrbl"]
@include-section["implementation.scrbl"]
@include-section["lib.scrbl"]
@include-section["installing.scrbl"]





