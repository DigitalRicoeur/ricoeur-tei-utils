#lang scribble/manual

@title{Kernel Utilities}
@defmodule[ricoeur/tei/kernel] @TODO/void[improve this]

@(require "for-manual.rkt"
          ricoeur/lib
          )

In addition to the bindings documented below,
@racketmodname[ricoeur/tei/kernel] also re-exports
everything from @racketmodname[adjutor],
@racketmodname[gregor], and @racketmodname[data/maybe],
@racket[fmap] (the version of @tt{map} from @racketmodname[data/functor]),
and @racket[comment?], @racket[p-i?], @racket[valid-char?],
and @racket[cdata?] from @racketmodname[xml].

@defproc[(title<? [a string?] [b string?]) any/c]{

 Like @racket[string-ci<?], but performs additional
 normalization on @racket[a] and @racket[b] appropriate
 for titles, such as ignoring the first word if
 it is @litchar{A}, @litchar{An}, or @litchar{The}.

 Use this function with @racket[sort] to alphabetize by title.
}

@section{X-Expression and XML Operations}

@deftogether[
 (@defthing[raw-xexpr/c flat-contract?
            #:value (or/c raw-xexpr-atom/c
                          raw-xexpr-element/c)]
   @defthing[raw-xexpr-atom/c flat-contract?
             #:value (or/c normalized-xexpr-atom/c
                           entity-symbol/c
                           valid-char?
                           cdata?)]
   @defthing[raw-xexpr-element/c flat-contract?])]{

 Many functions in this library that operate on @tech{x-expressions}
 require that they conform to the additional requirement
 that all symbols representing symbolic entities be known to this
 library, which is determined by the contract @racket[entity-symbol/c].
 X-expressions meeting these requirements qualify as
 @deftech{raw x-expressions} (or @deftech{raw xexprs}) and are recognized
 by the contract @racket[raw-xexpr/c].
 
 The contract @racket[raw-xexpr-element/c] recognizes specifically those
 @tech{raw x-expressions} which represent elements (all of which would also
 satisfy @racket[(and/c pair? list?)]).

 Note that @tech{normalized x-expressions} are a subset of
 @tech{raw x-expressions}.
}

@deftogether[
 (@defthing[normalized-xexpr/c flat-contract?
            #:value (or/c normalized-xexpr-atom/c
                          normalized-xexpr-element/c)]
   @defthing[normalized-xexpr-atom/c flat-contract?
             #:value (or/c string?
                           comment?
                           p-i?)]
   @defthing[normalized-xexpr-element/c flat-contract?])]{

 Some higher-level operations in this library produce and/or
 consume @deftech{normalized x-expressions}
 (or @deftech{normalized xexprs}), which are a restricted
 subset of @tech{x-expressions}.

 These are non-destructively normalized in a few ways,
 which simplifies case analysis when working with
 @tech{normalized xexprs}:
 @(itemlist
   @item{CDATA content and entities (both numeric and symbolic)
  are normalized to strings;
 }
   @item{All element @tech{normalized x-expressions}, which are recognized
  by the contract @racket[normalized-xexpr-element/c],
  must have an attribute list, even if it is empty; and
 }
   @item{The body of an element @tech{normalized x-expression} may
  only contain @tech{normalized x-expressions}.
  })
}


@defthing[entity-symbol/c flat-contract]{
 Similar to @racket[symbol?], but only recognizes those symbols
 which correspond to symbolic entities known to this library
 which can be converted to strings with functions like
 @racket[non-element-xexpr->plain-text].
 Most commonly-used symbolic entities are supported.

 A value satisfying @racket[entity-symbol/c] is a
 @tech{raw x-expression}, but not a @tech{normalized x-expression}
}

@defproc[(normalize-xexpr [raw raw-xexpr/c]) normalized-xexpr/c]{
 Returns a @tech{normalized xexpr} equivalent to the given
 @tech{raw xexpr}.
}

@defproc[(non-element-xexpr->plain-text [xs raw-xexpr-atom/c])
         string?]{
 Converts a @tech{raw x-expression} (including @tech{normalized xexprs})
 that @bold{does not} represent an XML element to a string.

 Because of the restriction that @racket[xs] must not
 represent an XML element, @racket[(non-element-xexpr->plain-text xs)]
 is very similar to @racket[(normalize-xexpr xs)], except that
 @racket[comment] and @racket[p-i] (processing instruction) structures
 are replaced by @racket[""].
}

@defproc[(non-element-body->plain-text [body (listof raw-xexpr-atom/c)])
         string?]{
 Like @racket[non-element-xexpr->plain-text], but concatenates the
 string forms on a list of non-element @tech{raw xexprs},
 such as might form the body of a @tech{raw xexpr} representing an
 XML element that may not contain any child elements.
}

@deftogether[(@defpredicate[normalized-xexpr?]
               @defpredicate[normalized-xexpr-element?]
               @defpredicate[raw-xexpr?]
               @defpredicate[raw-xexpr-element?])]{
 Equivalent to @racket[normalized-xexpr/c],
 @racket[normalized-xexpr-element/c], @racket[raw-xexpr/c], and
 @racket[raw-xexpr-element/c], respectively, except that
 they avoid some overhead assosciated with the contract system
 (but give bad error messages when used as contracts).
}


@deftogether[
 (@defproc[(check-normalized-xexpr-element [blame blame?]
                                           [val α]
                                           [neg-party any/c])
           (and/c α normalized-xexpr-element/c)]
   @defproc[(check-raw-xexpr-element [blame blame?]
                                     [val α]
                                     [neg-party any/c])
            (and/c α raw-xexpr-element/c)])]{
 These low-level functions can be used to implement
 contracts derived from @racket[normalized-xexpr-element/c]
 or @racket[raw-xexpr-element/c], respectively.

 Each function returns @racket[val] directly if it
 satisfies the corresponding contract;
 otherwise, it calls @racket[raise-blame-error] to report
 the details of the violation, supplying @racket[neg-party]
 as the missing party of the blame object.
}








@section{@tt{xmllint}-based Operations}

The functions documented in this section depend on the 
external command-line utility @exec{xmllint} (which is part
of @tt{libxml2}) to work as their names indicate.
If @exec{xmllint} can not be found,
a warning is logged to @racket[(current-logger)] at startup
and these functions fall back to the alternate behavior
specified in each function's documentation, which is
typically a no-op.

@defproc[(xmllint-available?) any/c]{
 Reports whether @exec{xmllint} is available at runtime.
}

@defproc[(valid-xml-file? [#:quiet? quiet? any/c #t]
                          [pth path-string?] ...+)
         boolean?]{
 Checks that every @racket[pth] is a valid XML file.

 When @racket[quiet?] is @racket[#f], writes any
 validation error messages (from @tt{xmllint}) to
 @racket[current-error-port].

 If @exec{xmllint} is not available, always returns @racket[#t].
}

@defproc[(directory-validate-xml [dir (and/c path-string? directory-exists?)]
                                 [#:quiet? quiet? any/c #f])
         boolean?]{
 Checks that every path satisfying @racket[xml-path?]
 in @racket[dir] and its recursive subdirectories
 is a valid XML file.

 When @racket[quiet?] is @racket[#f], writes any
 validation error messages (from @exec{xmllint}) to
 @racket[current-error-port].

 If @exec{xmllint} is not available, always returns @racket[#t].
}

@defproc[(call/prettyprint-xml-out [thunk (-> any/c)])
         any/c]{
 If @exec{xmllint} is not available, equivalent to @racket[(thunk)].

 When @exec{xmllint} is available, @racket[thunk] is called in
 a context where everything written to the @racket[current-output-port] is
 piped through @exec{xmllint}'s prettyprint function before being written to
 the original @racket[current-output-port].
 When prettyprinting succeeds, the result of @racket[call/prettyprint-xml-out]
 is the result of @racket[thunk].
 
 If prettyprinting fails (perhaps because the output of @racket[thunk] was
 not well-formed XML), @exec{xmllint} @bold{may still write} to the
 original @racket[current-output-port], but @racket[call/prettyprint-xml-out]
 raises an exception rather than returning a value.
 See @racket[with-output-to-file/unless-exn] for an alternative 
 when this behavior is undesirable.
 
 If @racket[thunk] raises an exception and @exec{xmllint} is available,
 @exec{xmllint} is never invoked and nothing is written to the original
 @racket[current-output-port].
}

