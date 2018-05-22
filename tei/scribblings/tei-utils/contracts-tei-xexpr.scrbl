#lang scribble/manual

@title{TEI X-Expression Contracts}
@(declare-exporting ricoeur/tei
                    ricoeur/tei/base
                    ricoeur/tei/oop
                    )

@(require "for-manual.rkt"
          )

@defthing[any-tei-xexpr/c flat-contract?]{
 Similar to @racket[raw-xexpr-element/c], but
 rejects some (not all) @tech{raw x-expressions} that would break TEI
 validity rules, including the additional requirements
 imposed by Digital Ricœur.
}

@defproc[(tei-xexpr/c [name tei-element-name/c])
         flat-contract?]{
 Produces a contract similar to @racket[any-tei-xexpr/c], but
 which recognizes only elements named @racket[name].
}

@defform[(static-tei-xexpr/c elem-name-id)]{
 Like @racket[(tei-xexpr/c (#,(racket quote) elem-name-id))],
 but resolved to the specific contract at compile-time,
 and a compile-time error is raised if @racket[(#,(racket quote) elem-name-id)]
 would not satisfy @racket[tei-element-name/c].
}

@defthing[tei-element-name/c flat-contract?]{
 A contract recognizing the names of valid
 Digital Ricœur TEI XML elements.
}


