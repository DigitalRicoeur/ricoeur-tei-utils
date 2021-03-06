#lang scribble/manual

@title{General Utilities}
@(declare-exporting ricoeur/tei/base
                    ricoeur/tei
                    ;; ???
                    ;; to support ricoeur/tei/kernel/lang/specification-lang
                    #:use-sources (ricoeur/tei/kernel))

@(require "for-manual.rkt")

@deftogether[
 (@defthing[string-immutable/c flat-contract?
            #:value (and/c string?
                           immutable?)]
   @defthing[path-string-immutable/c flat-contract?
             #:value (or/c path? (and/c string-immutable/c
                                        path-string?))])]{
 Contracts recognizing immutable strings and
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
  path or string} values, respectively.
}

@defthing[trimmed-string-px pregexp?
          #:value #px"^[^\\s]$|^[^\\s].*[^\\s]$"]{
 Matches non-empty strings that neither start nor end with whitespace.
}

@defproc[(title<? [a string-immutable/c] [b string-immutable/c]) any/c]{

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
