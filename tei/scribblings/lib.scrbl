#lang scribble/manual

@title{General Libraries}
@defmodule[ricoeur/lib]

@(require (for-label racket
                     ricoeur/lib))

The bindings documented in this section are provided by
@racketmodname[ricoeur/lib], not @racketmodname[ricoeur/tei] or
@racketmodname[ricoeur/tei/base]. 

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
