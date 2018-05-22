#lang scribble/manual

@title{TEI X-Expression Contracts}
@;; ricoeur/tei/base/def-from-spec to support ricoeur/tei/oop
@(declare-exporting ricoeur/tei/base
                    ricoeur/tei
                    ricoeur/tei/oop
                    #:use-sources (ricoeur/tei/base/def-from-spec)
                    )

@(require "for-manual.rkt"
          (for-label ricoeur/tei/base/def-from-spec
                     ))

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


