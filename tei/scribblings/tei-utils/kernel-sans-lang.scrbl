#lang scribble/manual

@title[#:style '(toc)]{Kernel Utilities}
@defmodule[ricoeur/tei/kernel #:no-declare]
@;; ricoeur/tei/kernel/pre-kernel-lib
@;; and ricoeur/tei/kernel/xmllint
@;; to support ricoeur/tei/oop
@(declare-exporting ricoeur/tei/kernel
                    ricoeur/tei/base
                    ricoeur/tei
                    ricoeur/tei/oop
                    #:use-sources (ricoeur/tei/kernel/pre-kernel-lib
                                   ricoeur/tei/kernel/xmllint)
                    )

@(require "for-manual.rkt"
          (for-label ricoeur/tei/kernel/pre-kernel-lib
                     ricoeur/tei/kernel/xmllint
                     ))

In addition to the bindings documented below,
@racketmodname[ricoeur/tei/kernel] also re-exports
everything from @racketmodname[adjutor],
@racketmodname[gregor], and @racketmodname[data/maybe],
as well as
@racket[fmap] (the version of @tt{map} from @racketmodname[data/functor])
and @racket[comment?], @racket[p-i?], @racket[valid-char?],
and @racket[cdata?] from @racketmodname[xml].

@(local-table-of-contents)

@section{X-Expression and XML Operations}

@defproc[(xml-path? [pth path-string?]) any/c]{
 Tests whether @racket[pth] is syntactically a path to an XML file, 
 without checking the validity of the file or even its existance.
}

@subsection{Raw X-Expressions}

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
 @tech{raw x-expressions} which represent XML elements (all of which 
 would also satisfy @racket[(and/c pair? list?)]).

 Note that @tech{normalized x-expressions} are a subset of
 @tech{raw x-expressions}.
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
 string forms of a list of non-element @tech{raw xexprs},
 such as might form the body of a @tech{raw xexpr} representing an
 XML element that may not contain any child elements.
}

@defthing[entity-symbol/c flat-contract]{
 Similar to @racket[symbol?], but only recognizes those symbols
 which correspond to symbolic entities known to this library,
 which can be converted to strings with functions like
 @racket[non-element-xexpr->plain-text].
 Most commonly-used symbolic entities are supported.

 A value satisfying @racket[entity-symbol/c] is allways a
 @tech{raw x-expression}, but never a @tech{normalized x-expression}
}

@deftogether[(@defpredicate[raw-xexpr?]
               @defpredicate[raw-xexpr-element?])]{
 Equivalent to @racket[raw-xexpr/c] and
 @racket[raw-xexpr-element/c], respectively, except that
 these functions avoid some overhead
 assosciated with the contract system,
 but give bad error messages when used as contracts.
}

@defproc[(check-raw-xexpr-element [blame blame?]
                                  [val α]
                                  [neg-party any/c])
         (and/c α raw-xexpr-element/c)]{
 A low-level function that can be used to implement
 contracts derived from @racket[raw-xexpr-element/c].

 Returns @racket[val] directly if it
 satisfies @racket[raw-xexpr-element/c];
 otherwise, calls @racket[raise-blame-error] to report
 the details of the violation, supplying @racket[neg-party]
 as the missing party of the blame object.
}



@subsection{Normalized X-Expressions}

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

 Note that all @tech{normalized x-expressions} are also
 @tech{raw x-expressions}.
}

@deftogether[(@defpredicate[normalized-xexpr?]
               @defpredicate[normalized-xexpr-element?])]{
 Equivalent to @racket[normalized-xexpr/c] and
 @racket[normalized-xexpr-element/c], respectively,
 but with the same relative advantages and disadvantages
 as @racket[raw-xexpr?] and @racket[raw-xexpr-element?].
}


@defproc[(check-normalized-xexpr-element [blame blame?]
                                         [val α]
                                         [neg-party any/c])
         (and/c α normalized-xexpr-element/c)]{
 Like @racket[check-raw-xexpr-element], but
 for implementing contracts derived from
 @racket[normalized-xexpr-element/c].
}








@subsection{@tt{xmllint}-based Operations}

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








@section{Development To-Do Expressions}
@defform*[[(TODO message)
           (TODO #:expr runtime-expr message)
           (TODO message #:expr runtime-expr)]
          #:grammar [(message (code:line msg-datum ...+ maybe-detail))
                     (maybe-detail (code:line)
                                   (code:line #: msg-datum ...+))]]{
 The @racket[TODO] form is intended to be used as a placeholder during
 development. When a @racket[runtime-expr] is given, the entier
 form is equivalent to the @racket[runtime-expr] being used directly.
 If there is no @racket[runtime-expr], evaluating the @racket[TODO] form
 at runtime raises an error (based on the @racket[message]).

 @(TODO/void find a nice way to degrade if the todo-list docs "aren't"
             installed)
 If the @other-doc['(lib "todo-list/scribblings/todo-list.scrbl")]
 plugin is installed (via the @tt{todo-list} package),
 DrRacket will also highlight the placeholders specially.

 A @racket[msg-datum] is implicitly quoted and must me an literal
 string, symbol, or number. These are converted to strings
 with a @racket[" "] added between them to form the message.
 If a @racket[maybe-detail] part is given, it is omited
 from the summary view, for example.
}

@defform[(TODO/void message)]{
 Cooperates with DrRacket like @racket[TODO], but evaluates to
 @void-const at runtime.
}

@defform[(TODO/scrbl [message] runtime-expr ...)]{
 Cooperates with DrRacket like @racket[TODO],
 but designed when for use with the Scribble reader.
 Evaluates to @racket[(begin runtime-expr ...)]
 at runtime.
}





@section{Other Utilities}

@defproc[(title<? [a string?] [b string?]) any/c]{

 Like @racket[string-ci<?], but performs additional
 normalization on @racket[a] and @racket[b] appropriate
 for titles, such as ignoring the first word if
 it is @litchar{A}, @litchar{An}, or @litchar{The}.

 Use this function with @racket[sort] to alphabetize by title.
}


@defproc[(with-output-to-file/unless-exn [path path-string?]
           [thunk (-> any/c)]
           [#:mode mode-flag (or/c 'binary 'text) 'binary]
           [#:exists exists-flag
            (or/c 'error 'append 'update
                  'replace 'truncate 'truncate/replace)
            'error]
           [#:buffer buffer (or/c 'memory 'file) 'memory])
         any/c]{
 Like @racket[with-output-to-file], but does not open @racket[path]
 for writing unless @racket[thunk] returns without raising an exception.
 In contrast, using @racket[with-output-to-file] with an @racket[exists-flag]
 like @racket['replace] may delete existing data, even if @racket[thunk]
 raises an exception without writing anything.

 The @racket[buffer] argument, if given, controls where the data is actually
 written duting the call to @racket[thunk].
 If it is @racket['memory] (the default), an internal Racket
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{pipe} is used;
 otherwise, if it is @racket['file],
 @racket[with-output-to-file/unless-exn] uses a temporary file.

 (An additional difference from @racket[with-output-to-file] is that
 @racket[thunk] is currently limited to a single return value.)
}




