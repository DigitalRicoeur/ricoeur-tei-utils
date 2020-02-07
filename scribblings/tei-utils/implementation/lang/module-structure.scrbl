#lang scribble/manual

@title{Module Structure}
@(declare-exporting ricoeur/tei/spec-lang/doc-lang)
@(require "for-lang-kernel.rkt"
          (only-in adjutor/unstable TODO/void))


The reader for @(hash-lang-kernel) is a variant of the
@(seclink "reader"
          #:doc '(lib "scribblings/scribble/scribble.scrbl")
          "Scribble reader")
that uses @litchar{ƒ} as its command character
(like the default for @racketmodname[_-exp]).
The reader begins in text mode: the syntactic ``outer'' layer
of a @(hash-lang-kernel) module is the @tech{documentation-time}
phase, even though, in expanded form, the enclosing module
contains the @tech{runtime} phase, while the @tech{documentation-time}
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
the module's @tech{elements specification transformer}.

The @racket[_spec-module-body] forms are the body of the module.
They have meaning both at @tech{runtime} and at
@tech{documentation-time}.




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




@section{The @racket[_spec-module-body] Forms}

The @racket[_spec-module-body] forms are first partially
expanded in the @tech{documentation-time} environment
to reveal uses of @racket[begin-for-runtime] and related
forms, prominently including @racket[define-element]
and @racket[define-elements-together].
@margin-note*{The details of expansion to these forms remain
 under development and are subject to change.}

The contained @tech{runtime} forms are spliced together
to form the body of the module.
Additionally, a @racket[test] submodule is started using
@racket[module+], as though the following had appeared
among the @racket[_spec-module-body] forms:
@racketblock[(begin-for-test
               (require (submod ".." doc)))]

The @tech{documentation-time} code from the
@racket[_spec-module-body] forms is placed in
an implicit @racket[doc] submodule
(declared with @racket[module*]).

@TODO/void[Move this paragraph #: to a new section?]@;{
 In addition to the bindings documented here, for @tech{runtime}
 use @(hash-lang-kernel) provides everything from
 @racketmodname[racket/contract],
 @racketmodname[racket/match], @racketmodname[racket/list],
 @racketmodname[racket/string], and @racketmodname[racket/base]
 (except that it provides a replacement for
 @racketidfont{#%module-begin}).
 @TODO/void[List things from ricoeur/tei/kernel.]@;
 @bold{TODO:} List things from @racketmodfont{ricoeur/tei/kernel}.
}

@defform[(begin-for-runtime body ...)]{
 The basic form for escaping from the @tech{documentation-time}
 phase to the @tech{runtime} phase.

 The @racket[body] sub-forms are at the runtime phase.
 They are spliced into the module level of the enclosing
 @tech{runtime} phase module, somewhat like
 @racket[(begin body ...)].

 As an expression in the @tech{documentation-time} phase,
 a @racket[begin-for-runtime] form evaluates to
 @void-const, and the @racket[body] sub-forms are ignored.

 A @racket[begin-for-runtime] form may only appear at the
 module level or in an @tech{element definition prose}
 context (see @racket[define-element]
 and @racket[define-elements-together]).
 Restrictions on the nesting of @tech{element definition prose}
 contexts keeps the order of the synthesized
 @tech{runtime} phase clear from the program's source.
}

@defform[(begin-for-test body ...)]{
 Expands to:
 @racketblock[(begin-for-runtime
                (module+ test
                  body ...))]
}

@defform[(begin-for-runtime/derived
           orig-datum
           maybe-doctime-expr
           body ...)
         #:grammar ([maybe-doctime-expr
                     (code:line)
                     (code:line #:expr doctime-expr)])
         #:contracts ([doctime-expr pre-flow?])]{                      
 Like @racket[(begin-for-runtime body ...)], but 
 reports errors in terms of @racket[orig-datum].
 
 If a @racket[doctime-expr] is given, it must be an
 expression at the @tech{documentation-time} phase,
 and the value of the @racket[begin-for-runtime/derived]
 expression at the @tech{documentation-time} phase
 is the value of the @racket[doctime-expr] expression.
 If no @racket[doctime-expr] is given, the default is
 @void-const, as with @racket[begin-for-runtime].
 
 @margin-note{
  The @racket[doctime-expr] mechanism is useful because,
  as with @racket[begin-for-runtime], @racket[begin-for-runtime/derived]
  may only be used at at the module level or in
  an @tech{element definition prose} context.
  Because @tech{element definition prose} contexts are expression
  contexts, macros that expand to @racket[begin-for-runtime/derived]
  cannot easily use more general methods to expand to something
  other than @void-const at the @tech{documentation-time} phase.
}}


@section{Runtime Submodules}
@(declare-exporting ricoeur/tei/spec-lang/specification-lang)
@deftogether[
 (@defform[(module+ id form ...)]
   @defform*[((module* id module-path form ...)
              (module* id #f form ...))])]{
 Like the versions from @racketmodname[racket/base],
 but wrapped so that submodules created with @racket[module+]
 or @racket[module*] with @racket[#f] instead of a @racket[module-path]
 use the @racket[#%module-begin] from @racketmodname[racket/base],
 rather than the version from @(hash-lang-kernel).
}


