#lang scribble/manual

@title[#:style '(toc)]{Implementation Details}

Unlike the rest of this manual, this section does
@bold{not} document the bindings provided by
@racketmodname[ricoeur/tei] or
@racketmodname[ricoeur/tei/base].

This section instead documents the @italic{implementation}
of certain parts of @racketmodname[ricoeur/tei] for
reference when maintaining the library.

@(local-table-of-contents)

@include-section["implementation/lang/lang-kernel.scrbl"]
@include-section["implementation/search.scrbl"]
@section{``TEI Lint'' Implementation}
This section will contain details on the implementation
of ``TEI Lint''.
