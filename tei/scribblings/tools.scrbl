#lang scribble/manual

@title[#:version ""]{Tools}

@(require (for-label ricoeur/tei))

We have implemented a number of tools
to assist in preparing and validating TEI XML documents.
All of these require the library @racketmodname[ricoeur/tei],
which should be installed as described under
@secref["Installing___Updating_This_Library"
        #:doc '(lib "ricoeur/tei/scribblings/tei-utils.scrbl")]
in @other-doc['(lib "ricoeur/tei/scribblings/tei-utils.scrbl")].

@section{TEI Lint}

Most comprehensively, the GUI program "TEI Lint" is a "linter" for
TEI XML documents:
it both checks the validity of the prepared documents (using @tt{xmllint}
from @tt{libxml2} when available) both in terms of the DTD and
with respect to our more stringent project-specific requirements
and also alerts the user to issues
with the documents that, while not making them invalid,
are indicative of potential subtle mistakes.

Using "TEI Lint" is the most important reason to have @tt{xmllint}
installed: without it, "TEI Lint" can check that the documents
are well-formed XML and meet the project-specific requirements
expressed in Racket code, but can't actually check that the 
documents are valid in terms of the DTD.

@subsection{Command-line Validation}

In the @tt{TEI} directory of the @tt{texts} repository,
the @tt{validate-all.rkt} script provides a way to perform
a subset of the checking done by "TEI Lint" from the command line,
though it only works under a Unix-like shell
(i.e. on Mac OS or GNU/Linux, or possibly under Cygwin).
It also only checks that the documents are valid, well-formed, and
meet the additional requirements stated in
@other-doc['(lib "ricoeur/tei/scribblings/guidelines.scrbl")]: it
does not check for subtle mistakes. For both of these reasons,
"TEI Lint" should generally be preferred.

The script can also be invoked by running
@tt{make validate} in the root directory of the @tt{texts} repository.

@section{encode-xml-entities}

The command-line tool @exec{encode-xml-entities} should be run on
plain text files before adding any XML markup.
It replaces the reserved characters @litchar{<} and @litchar{&}
with the corresponding XML entities as described under
@secref["Prerequisites"] above.
Run it with the flag @DFlag{help} or @Flag{h} for usage information.

@section{tei-guess-paragraphs}

The @exec{tei-guess-paragraphs} command-line tool
replaces a TEI XML file with an equivalent in which paragraph
breaks have been guessed using the method 
@(xmethod guess-paragraphs<%> guess-paragraphs).
Run it with the flag @DFlag{help} or @Flag{h} for usage information.

When @tt{xmllint} is available, the output will be prettyprinted.

Please always check the output of this tool: it operates on
a best-effort basis. If, for example, you notice that it has simply
replaced each "annonymous block" with one long paragraph, it
would be better to revert your change and wait for this tool
to be improved than to commit such semantically meaningless output.

@section{raco tei-to-plain-text}

The @tt{raco} subcommand @tt{tei-to-plain-text} writes a
TEI XML file to STDOUT as plain text.
It is primarily intended to be used by invoking @exec{make}
in the @tt{texts} repository, which will populate the
@tt{plain-text} directory with plain text versions of
every TEI XML file in the @tt{TEI} directory.
If for some reason you want to use it directly,
run @exec{raco tei-to-plain-text -h} for usage information.
