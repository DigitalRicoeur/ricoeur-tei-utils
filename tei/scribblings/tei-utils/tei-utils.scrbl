#lang scribble/manual

@title[#:version ""]{Digital Ricœur TEI Library}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com")]
@defmodule[ricoeur/tei]

@(require "for-manual.rkt"
          (for-label (only-in ricoeur/tei/search
                              search-documents
                              searchable-document-set?
                              regexp-searchable-document-set
                              postgresql-searchable-document-set)
                     (submod ricoeur/tei/search/common private)
                     ricoeur/tei/corpus
                     ))

This manual documents utilities and Racket libraries for
working with TEI XML documents developed for Digital Ricœur.
In addition to being valid and well-formed XML, the
documents should conform to the structure specified in
@(guidelines-doc).

@margin-note{
 Some significant revisions to this library are
 currently in progress.
 All current functionality will still be supported,
 but significant backwards-incompatable API changes are
 planned, especially to the lower-level portions.
 The goal of the changes is to improve the organization
 of the library and to provide a better and more stable
 foundation for future extensions.
 In the mean time, clients of this library should beware.
}

This document is written for programmers intending either to
use these libraries in their own programs (particularly in the case
of @secref["High-level_Interface"] and, to a lesser extent,
@secref["Object_System"]) or to contribute to their implementation.
It assumes familiarity with the concept of @deftech{x-expressions}
from Racket's @racketmodname[xml] library
(see the grammar documented under @racket[xexpr?]).

@(table-of-contents)

@include-section["kernel-sans-lang.scrbl"]
@;include-section["high-level.scrbl"]
@;include-section["object-system.scrbl"]
@;include-section["xexpr+xmllint.scrbl"]
@;include-section["implementation.scrbl"]
@include-section["lib.scrbl"]
@include-section["installing.scrbl"]





