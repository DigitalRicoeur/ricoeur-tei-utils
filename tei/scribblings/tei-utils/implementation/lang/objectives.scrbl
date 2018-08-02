#lang scribble/manual

@title{Language Objectives}
@(require "for-lang-kernel.rkt")


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
The next several sections of this manual will explain these aspects of
the @(hash-lang-kernel) language in detail.

A module in @(hash-lang-kernel) provides its clients
at least two, and possibly three, distinct kinds of services:
@(itemlist
  @item{The @tech{documentation-time} phase of the module
 is grouped into an implicit @racket[doc] submodule
 (declared with @racket[module*]), which may be incorporated
 into a larger Scribble document via @racket[include-section].
}
  @item{Each module in @(hash-lang-kernel) exports
 an @deftech{elements specification transformer} binding
 that encapsulates the specifications of the TEI XML elements
 defined in that module.
 This can be invoked using @racket[define-values/elements-specifications],
 possibly after being linked with other modules'
 @tech{elements specification transformers}, to generate definitions
 for @racket[tei-xexpr/c], @racket[xexpr->element],
 and other derived bindings.
 More details of @tech{elements specification transformers}
 will be discussed below, especially under @racket[define-element].
}
  @item{Optionally, a module in @(hash-lang-kernel) can export
 bindings using @racket[provide] like any other module.
 In particular, a module may provide functions that operate
 on the @tech{tei element struct types} it defines.
 Some of these may become part of the public API of this
 library, while others may be used only in the implementation
 of cooperating @(hash-lang-kernel) modules:
 all of Racket's facilities are available for managing and
 protecting such exports.
 })
