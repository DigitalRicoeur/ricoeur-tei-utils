#lang scribble/manual

@title{``TEI Lint'' Implementation}
@defmodule[ricoeur/tei/tools/tei-lint/tei-lint]
@(require "../for-manual.rkt"
          (for-label ricoeur/tei/tools/tei-lint/lint/toolkit
                     (except-in racket/gui
                                date
                                date?)
                     ))


This section will contain details on the implementation
of @guidelines-secref["TEI_Lint"].

@section{Interfaces}
@defmodule[ricoeur/tei/tools/tei-lint/lint/toolkit]

@deftogether[
 (@defthing[lint-status/c flat-contract?
            #:value (or/c 'error 'warning 'ok)]
   @defproc[(lint-status-more-urgent? [a lint-status/c]
                                      [b lint-status/c])
            any/c])]{

}

@definterface[lint-status<%> ()]{
 @defmethod[(get-lint-status)
            lint-status/c]{

}}

@definterface[directory-frame<%> (frame%)]{
 The interface of @deftech{directory frames}.
 @defmethod[(open-additional) any]{

 }
 @defmethod[(call-in-directory-context [thunk (-> any)])
            any]{

 }
 @defmethod[(refresh-directory!) any]{

 }
}

@definterface[get-dir-frame/false<%> ()]{
 @defmethod[(get-dir-frame/false)
            dir-frame/false/c]{

}}

@defthing[dir-frame/false/c flat-contract?
          #:value (or/c #f (is-a?/c directory-frame<%>))]{
 A contract for optional @tech{directory frames}.
}

@definterface[tei-document-frame<%> (frame% lint-status<%> get-dir-frame/false<%>)]{
 @defmethod[(get-path) path-string?]{

 }
 @defmethod[(get-old-modify-seconds) real?]{

 }
}

@definterface[proto-frame<%> (lint-status<%> get-dir-frame/false<%>)]{
 @defmethod[(show [show? any/c]) any]{

}}

@defclass[error-proto-frame% object% (proto-frame<%>)]{
 (class/c
 (init [path path-string?]
 [dir-frame dir-frame/false/c]
 [val (or/c exn:fail? string?)]))

}

@subsection{Document Frame Components}
make-tei-document-proto-frame-class
tei-document-proto-frame%/c
document-frame-component?

[document-frame-component
(-> (-> tei-document?
(values lint-status/c
initialize-proc/c))
document-frame-component?)]
[document-frame-component/ok
(-> (-> tei-document? initialize-proc/c)
document-frame-component?)]
[constant-document-frame-component
(-> initialize-proc/c
document-frame-component?)]
[document-frame-component-append
(-> document-frame-component? ...
document-frame-component?)]
[document-frame-component-run
(-> document-frame-component?
tei-document?
(values lint-status/c
initialize-proc/c))]

@subsection{Menu Bars}
[menu-bar:file%
(class/c
(init [dir-frame dir-frame/false/c]))]
[menu-bar:file+edit%
(class/c
(init [dir-frame dir-frame/false/c]))]

@section{More}

paragraphs.rkt document.rkt file-snip.rkt directory-frame.rkt

@section{Utility Library}
@defmodule[ricoeur/tei/tools/tei-lint/lib]

Theoretically, this module should provide small utilities
needed in multiple places that don't have much to do
with TEI Lint specifically.
In fact, this module is disorganized and badly in need of
maintenence.


@section{Application Icons}
@defmodule[ricoeur/tei/tools/tei-lint/convert-icon]

Running this module will rebuild the application icons.
It need only be done if there is a change.
The derived icon formats are checked into the Git repository
because @exec{raco setup} doesn't provide a hook to build them
programatically at the right time.
(See the @hyperlink["https://groups.google.com/d/topic/racket-users/l4nx2jLFg1k/discussion"]{
 mailing list} for more details.)
