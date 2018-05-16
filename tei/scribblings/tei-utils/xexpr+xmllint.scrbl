#lang scribble/manual

@title{XML and X-Expression Operations}

@(require (for-label (except-in racket
                                date
                                date?)
                     xml
                     data/maybe
                     ricoeur/tei
                     ricoeur/lib
                     db
                     json
                     (only-in ricoeur/tei/search
                              search-documents
                              searchable-document-set?
                              regexp-searchable-document-set
                              postgresql-searchable-document-set)
                     (submod ricoeur/tei/search/common private)
                     setup/matching-platform
                     gregor
                     ))

@section{X-Expression Contracts}

@defthing[any-tei-xexpr/c flat-contract?]{
 Similar to @racket[(and/c list? xexpr/c)], but
 rejects some (not all) x-expressions that would break TEI
 validity rules, including the additional requirements
 imposed by Digital Ricœur.
}

@defproc[(tei-xexpr/c [name tei-element-name/c])
         flat-contract?]{
 Produces a contract similar to @racket[any-tei-xexpr/c], but
 which recognizes only tags named @racket[name].
}

@defthing[tei-element-name/c flat-contract?]{
 A contract recognizing the names of valid Digital Ricœur TEI XML element.
}

@section{@tt{xmllint}-based Operations}

The functions documented in this section depend on the 
external command-line utility @tt{xmllint} (which is part
of @tt{libxml2}) to work. If @tt{xmllint} can not be found,
a warning is logged to @racket[(current-logger)].

@defproc[(xmllint-available?) any/c]{
 Detects whether @tt{xmllint} is available at runtime.
}

@defproc[(valid-xml-file? [#:quiet? quiet? any/c #t]
                          [pth path-string?] ...+)
         boolean?]{
 Checks that every @racket[pth] is a valid XML file.

 When @racket[quiet?] is @racket[#f], writes any
 validation error messages (from @tt{xmllint}) to
 @racket[current-error-port].

 If @tt{xmllint} is not available, always returns @racket[#t].
}

@defproc[(directory-validate-xml [dir (and/c path-string? directory-exists?)]
                                 [#:quiet? quiet? any/c #f])
         boolean?]{
 Checks that every path ending in @litchar{.xml}
 in @racket[dir] and its recursive subdirectories
 is a valid XML file.

 When @racket[quiet?] is @racket[#f], writes any
 validation error messages (from @tt{xmllint}) to
 @racket[current-error-port].

 If @tt{xmllint} is not available, always returns @racket[#t].
}

@defproc[(call/prettyprint-xml-out [thunk (-> any/c)])
         any/c]{
 If @tt{xmllint} is not available, equivalent to @racket[(thunk)].

 When @tt{xmllint} is available, @racket[thunk] is called in
 a context where everything written to the @racket[current-output-port] is
 piped through @tt{xmllint}'s prettyprint function before being written to
 the original @racket[current-output-port].
 When prettyprinting succeeds, the result of @racket[call/prettyprint-xml-out]
 is the result of @racket[thunk].
 
 If prettyprinting fails (perhaps because the output of @racket[thunk] was
 not well-formed XML), @bold{@tt{xmllint} may still write to the
  original @racket[current-output-port]}, but @racket[call/prettyprint-xml-out]
 raises an exception rather than returning a value.
 See @racket[with-output-to-file/unless-exn] for an alternative when this behavior
 is undesirable.
 If @racket[thunk] raises an exception and @tt{xmllint} is available,
 @tt{xmllint} is never invoked and nothing is written to the original
 @racket[current-output-port].
}

