#lang scribble/manual

@title{Linking & Invoking}
@defmodule[(submod ricoeur/tei/kernel private)]
@(require "for-lang-kernel.rkt")


The @submodlink[(submod ricoeur/tei/kernel private)] module
provides functions for working with
@tech{elements specification transformers},
plus some small utilities useful when writing contracts and constructors
in @racket[define-element] forms.

@defform[(define-values/elements-specifications [spec ...]
           binding-clause ...)
         #:grammar
         [(binding-clause (code:line #:tei-xexpr/c tei-xexpr/c-id)
                          (code:line #:static-tei-xexpr/c static-tei-xexpr/c-id)
                          (code:line #:any-tei-xexpr/c any-tei-xexpr/c-id)
                          (code:line #:xexpr->tei-element xexpr->tei-element-id maybe-contract)
                          (code:line #:tei-element-name/c tei-element-name/c-id))
          (maybe-contract code:blank #:define/contract)]]{
 Links and invokes all of the @racket[spec]s, which must be
 @tech{elements specification transformers}, binding the supplied
 identifiers to the resulting functions and syntactic forms.

 It is a syntax error if the @racket[spec]s encapsulate duplicate
 element definitions or if any of the encapsulated definitions
 refer to elements that are not been defined by one of the @racket[spec]s.

 Each @racket[binding-clause] must appear exactly once, but they may
 be in any order. If the @racket[#:define/contract] flag is present,
 the function bound to @racket[xexpr->tei-element-id] is protected
 using @racket[define/contract].

 For more details on the generated bindings, see
 @racket[tei-xexpr/c], @racket[static-tei-xexpr/c], @racket[any-tei-xexpr/c],
 @racket[xexpr->tei-element], and @racket[tei-element-name/c], which are
 implemented using @racket[define-values/elements-specifications].
}
          

@defform[(define-combined-elements-specification new-spec-id
           [spec ...+])]{
 Binds @racket[new-spec-id] as an @tech{elements specification transformer}
 by linking together all of the @racket[spec]s
 (which must be defined as @tech{elements specification transformers}).

 It is a syntax error if the @racket[spec]s encapsulate duplicate
 element definitions, but missing elements are allowed at the linking stage.
}

@defproc[(get-attributes [xs (and/c pair? xexpr/c)])
         (listof (list/c symbol? string?))]{
 Extracts the attributes list from @racket[xs], an
 @tech{x-expression} representing an XML element, or
 @racket['()] if there are no attributes.

 Note that this function does not currently enforce
 its contract.
}

@defproc[(get-body [xs (and/c pair? xexpr/c)])
         (listof xexpr/c)]{
 Extracts the children of @racket[xs], an
 @tech{x-expression} representing an XML element.

 Note that this function does not currently enforce
 its contract.
}