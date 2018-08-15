#lang scribble/manual

@title[#:style '(toc)]{Specification Language}
@defmodule[(spec-lang-mod) #:lang
           #:module-paths (ricoeur/tei/kernel/lang/specification-lang)]
@(require "for-lang-kernel.rkt")


A vital purpose of this library is to specify the precise structure
of Digital @|Ricoeur|'s TEI XML documents.
This specifcation operates on several different levels, including:
@(itemlist
  @item{The prose documentation rendered in the
 @guidelines-secref["Formal_Specification"] section of @(guidelines-doc);
}
  @item{The computer-verifiable contracts that enforce the
 specification (see @racket[tei-xexpr/c]); and
}
  @item{The implementation of the @tech{TEI element struct types}
 that provide an initial layer of abstraction over the details
 of the documents' structure.
 })

To ensure consistency, all of these operationalizations of the
specification are defined together in the language @(hash-lang-kernel).

@(local-table-of-contents)

@include-section["objectives.scrbl"]
@include-section["module-structure.scrbl"]
@include-section["define-element.scrbl"]
@include-section["linking.scrbl"]
@include-section["doc-lang.scrbl"]










