#lang scribble/manual

@title[#:style '(toc)]{Specification Language}
@(define (spec-lang-mod)
   (racketmodlink ricoeur/tei/kernel/lang/specification-lang
                  (racketmodfont "ricoeur/tei/kernel")))
@defmodule[(spec-lang-mod) #:lang
           #:module-paths (ricoeur/tei/kernel/lang/specification-lang)]


@(require (except-in "../for-manual.rkt"
                     #%module-begin)
          (for-label ricoeur/tei/kernel/lang/specification-lang
                     (submod ricoeur/tei/kernel private)
                     scribble/base
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

@(local-table-of-contents)

@section{Language Objectives}

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

@section{Module Structure}

The reader for @(hash-lang-kernel) is a variant of
the at-reader that uses @litchar{ƒ} as its command character
(like the default for @racketmodname[_-exp]).
The reader begins in text mode; the syntactic ``outer'' layer
of a @(hash-lang-kernel) module is the @tech{documentation-time}
phase, even though, in expanded form, the enclosing module
contains the @tech{runtime} phase and the @tech{documentation-time}
code is placed in an implicit @racket[doc] submodule
(declared with @racket[module*]).

The top-level grammar of a @(hash-lang-kernel) module is as follows:
@racketgrammar[spec-module
               (code:line #,(hash-lang) #,(spec-lang-mod)
                          init-whitespace ...
                          spec-name-declaration
                          spec-module-body ...)]

The @racket[_init-whitespace] forms are literal strings consisting
exclusively of whitespace: there are ignored, though they are
permitted for convienience with the text-mode reader.

The @racket[_spec-name-declaration] (which may be empty)
controls the binding of
the module's @tech{elements specification transformer};
it is discussed in more detail bellow.

The @racket[_spec-module-body] forms are the body of the
module; they have meaning both at @tech{runtime} and at
@tech{documentation-time}.

At @tech{documentation-time}, most forms are expanded/evaluated
normally. Any @racket[begin-for-runtime] forms are replaced
with @racket[(void)] at documentation-time,
and uses of @racket[define-element] or @racket[define-elements-together]
generate typeseting at documentation-time; see their documentation
below for more details.
The other bindings which are provided by @(hash-lang-kernel) for
documentation-time use are documented under
@racketmodname[ricoeur/tei/kernel/lang/doc-lang].

At @tech{runtime}, the @racket[_spec-module-body] forms
are partially expanded to reveal uses of @racket[begin-for-runtime],
@racket[define-element], or @racket[define-elements-together].
Any @racket[_spec-module-body] form that does not expand 
to one of these forms is ignored at runtime.
@TODO/scrbl[[Expansion to define-element
             #: From outside the "library," how would it actually
             work to expand to define-element at both phases?
             Would it be better not to give the doc module the
             raw forms?]]{
 @margin-note*{The details of expansion to these forms remain
  under development and are subject to change.}}
The bodies of all @racket[begin-for-runtime] forms are
spliced together at the beginning of the module (see below)
and evaluated before any of the @racket[define-element]
or @racket[define-elements-together] forms.

In addition to the bindings documented here, for @tech{runtime}
use @(hash-lang-kernel) provides everything from
@racketmodname[ricoeur/tei/kernel], @racketmodname[racket/contract].
@racketmodname[racket/match], @racketmodname[racket/list],
@racketmodname[racket/string], and @racketmodname[racket/base]
(except that it provides a replacement for
@(defidform/inline #%module-begin)).


@defform[(begin-for-runtime body ...)]{
 In the @tech{runtime} phase, similar to
 @racket[(begin body ...)], indcluding splicing
 into module and internal-definition contexts.

 In the @tech{documentation-time} phase,
 expands to @racket[(void)].
}

@defform[(begin-for-test body ...)]{
 Expands to:
 @racketblock[(begin-for-runtime
                (module+ test
                  body ...))]
}

@subsection{The @racket[_spec-name-declaration]}
The grammar of the @racket[_spec-name-declaration] is as follows:
@(let ()
   (define (override-color-id id)
     (elem (racketidfont (symbol->string id))
           #:style (style #f (list (color-property "black")))))
   (define default-comment
     @elem{defaults to @racket[[#:spec #,(override-color-id 'spec)]]})
   (racketgrammar*
    #:literals (spec)
    (spec-name-declaration atomic-spec-name-declaration
                           compound-spec-name-declaration)
    (atomic-spec-name-declaration (code:line #,(hspace 1) (code:comment #,default-comment))
                                  [#:spec spec-id])
    (compound-spec-name-declaration [#:spec spec-id
                                     #:with-local local-spec-id
                                     #:extends extended-spec-id ...+]
                                    [#:spec spec-id
                                     #:extends extended-spec-id ...+
                                     #:with-local local-spec-id])))
The @racket[_spec-id], which defaults to @racket[spec] if
the declaration is left implicit, determines the name of
@tech{elements specification transformer}:
it is always implicitly @racket[provide]d.

An @racket[_atomic-spec-name-declaration] is the basic form
of @racket[_spec-name-declaration]: it defines
@racket[_spec-id] as an @tech{elements specification transformer}
that encapsulates the contract and @tech{tei element struct type}
information from all of the uses of @racket[define-element]
or @racket[define-elements-together] in the module.

A @racket[_compound-spec-name-declaration] is a convienient way
to combine @tech{elements specification transformers} from
multiple modules. In that case, the current module's
information is bound to @racket[_local-spec-id]
(primarily for error-reporting purposes),
then @racket[_spec-id] is defined as an @tech{elements specification transformer}
that combines @racket[_local-spec-id] with all of the
@racket[_extended-spec-id]s, which must be bount to
@tech{elements specification transformers}
(probably imported from other modules).

@margin-note{
 See also @racket[define-combined-elements-specification]
 for a more general means of combining @tech{elements specification transformers}.
 The expansion of a @racket[_compound-spec-name-declaration]
 is implemented using @racket[define-combined-elements-specification].
}


@section{Defining TEI Elements}
@;{
 "stxparam.rkt" ; this is the shared one
 "adt.rkt"
}

@(defidform/inline define-element)

@(defidform/inline define-elements-together)




@section{The Documentation Phase}
@defmodule[ricoeur/tei/kernel/lang/doc-lang]
@;"stxparam.rkt" ; this is the shared one
 




