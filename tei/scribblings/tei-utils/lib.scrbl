#lang scribble/manual

@title{General Libraries}
@defmodule[ricoeur/lib]

@(require ricoeur/lib
          (for-label racket
                     ricoeur/lib))

The bindings documented in this section are provided by
@racketmodname[ricoeur/lib] but not necessarily by @racketmodname[ricoeur/tei],
unless otherwise specified.

They are not specifically related to processing TEI XML files,
but they are used in the implementation of @racketmodname[ricoeur/tei],
and they may also be useful in other contexts.

@defproc[(xml-path? [pth path-string?]) any/c]{
 Tests whether @racket[pth] is syntactically a path to an XML file, without
 checking the validity of the file or even its existance.
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

@defform[(TODO/scrbl [message] runtime-expr ...+)]{
 Cooperates with DrRacket like @racket[TODO],
 but designed when for use with the Scribble reader.
 Evaluates to @racket[(begin runtime-expr ...)]
 at runtime.
}


