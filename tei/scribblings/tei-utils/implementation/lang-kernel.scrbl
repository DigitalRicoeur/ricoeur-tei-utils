#lang scribble/manual

@title{Specification Language}
@(define (spec-lang-mod)
   (racketmodlink ricoeur/tei/kernel/lang/specification-lang
                  (racketmodfont "ricoeur/tei/kernel")))
@defmodule[(spec-lang-mod) #:lang
           #:module-paths (ricoeur/tei/kernel/lang/specification-lang)]


@(require (except-in "../for-manual.rkt"
                     #%module-begin)
          (for-label ricoeur/tei/kernel/lang/specification-lang
                     ))


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
  @item{The implementation of the @tech{tei element struct types}
 that provide an initial layer of abstraction over the details
 of the documents' structure.
 })

@(define (hash-lang-kernel)
   @elem{@(hash-lang) @(spec-lang-mod)})

To ensure consistency, all of these operationalizations of the
specification are defined together in the language @(hash-lang-kernel).
A module in @(hash-lang-kernel) consists of interleaved
@deftech{runtime} and @deftech{documentation-time} code,
some of which is generated from syntactic forms that have meanings
at both phases.
(This is somewhat analagous to @racketmodname[scribble/lp2],
which was used in an earlier version of this library.)
Additionally, to promote modularity and avoid cyclic dependency
problems, some portions of a module in @(hash-lang-kernel)
are encapsulated to be invoked later, generally after being
linked with encapsulated information from
other @(hash-lang-kernel) modules.
(This is somewhat analagous to the @racketmodname[racket/unit]
system, which was used in an earlier version of this library.)
The remainder of this section will explain these aspects of
the @(hash-lang-kernel) language in detail.

@(racketgrammar*
  [spec-module (code:line #,(hash-lang) #,(spec-lang-mod)
                          init-whitespace ...
                          maybe-spec-name-declaration
                          spec-module-form ...)]
  [maybe-spec-name-declaration (code:line)
   [#:spec spec-id]
   [#:spec spec-id
    #:with-local local-spec-id
    #:extends extended-spec-id ...+]
   [#:spec spec-id
    #:extends extended-spec-id ...+
    #:with-local local-spec-id]]
  #|END racketgrammar*|#)

@;{
 ricoeur/tei/kernel/sans-lang
 racket/contract
 racket/match
 racket/string
 racket/list
 "stxparam.rkt" ; this is the shared one
 "adt.rkt"
 ;;;;
 #%module-begin
}


@defform[(begin-for-runtime body ...)]
define-element
define-elements-together
@defform[(begin-for-test body ...)]


