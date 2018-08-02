#lang scribble/manual

@title{Module Structure}
@(require "for-lang-kernel.rkt")


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
@racketmodname[racket/contract],
@racketmodname[racket/match], @racketmodname[racket/list],
@racketmodname[racket/string], and @racketmodname[racket/base]
(except that it provides a replacement for
@(defidform/inline #%module-begin)).
@TODO/scrbl[[List things from ricoeur/tei/kernel.]]{
 @bold{TODO:} List things from @racketmodfont{ricoeur/tei/kernel}.
}


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

@section{The @racket[_spec-name-declaration]}
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
