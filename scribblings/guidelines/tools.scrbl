#lang scribble/manual

@title[#:version ""]{Tools}

@(require (for-label ricoeur/tei))

We have implemented a number of tools
to assist in preparing and validating TEI XML documents.
All of these require the library @racketmodname[ricoeur/tei],
which should be installed as described under
@secref["Installing___Updating_This_Library"
        #:doc '(lib "ricoeur/scribblings/tei-utils/ricoeur-tei-utils.scrbl")]
in @other-doc['(lib "ricoeur/scribblings/tei-utils/ricoeur-tei-utils.scrbl")].

All of our command-line tools accept the flags
@DFlag{help} or @Flag{h} to print usage information.



@section{TEI Lint}

Our primary tool is the GUI program ``TEI Lint,''
which combines several related features.

As the name suggests, ``TEI Lint'' is a ``linter'' for TEI XML documents.
It checks the validity of the prepared documents (using @exec{xmllint}
from @tt{libxml2} when available) both in terms of the DTD and
with respect to our more stringent project-specific requirements.
It also alerts the user to issues
with the documents that, while not making them invalid,
are indicative of potential subtle mistakes or
high-priority encoding steps that have not yet been completed.
For some of these steps, such as inferring paragraph breaks,
``TEI Lint'' includes the ability to edit the documents automatically
with minimal human guidance.

Using ``TEI Lint'' is the most important reason to have @exec{xmllint}
installed: without it, ``TEI Lint'' can check that the documents
are well-formed XML and meet the project-specific requirements
expressed in Racket code, but can't actually check that the 
documents are valid in terms of the DTD.

In addition to serving as a linter, ``TEI Lint'' includes a simple,
fully graphical interface for converting a plain text file to
an initial TEI XML document.
This functionality is described in more detail above,
under @secref["Getting_Started"].



@section{@exec{raco ricoeur/tei}}

The @exec{raco ricoeur/tei} command extends @exec{raco} with some further
subcommands for processing TEI documents.
Most of these commands are primarily intended to be used via @exec{make}
in the @tt{texts} repository.
They are included as @exec{raco} subcommands to ensure they are
in the @tt{PATH} under most circumstances.
@margin-note{For more information about @exec{raco}, see
 @other-doc['(lib "scribblings/raco/raco.scrbl")].}
@itemlist[
 @item{@exec{raco ricoeur/tei validate-directory} validates all of
  the XML files in some directory, enforcing both the DTD
  (when @exec{xmllint} is available) and the additional requirements
  specified in this document.
  It does not give warnings about potential subtle mistakes,
  so ``TEI Lint'' should generally be preferred.

  Running @exec{make validate} in the root directory of the @tt{texts}
  repository validates the contents of the @tt{TEI} directory
  of that repository.
 }
 @item{@exec{raco ricoeur/tei directory-clean-filenames} renames all XML files in
  some directory (or the current directory, if none is provided)
  as needed to ensure that all start with a lower-case letter and that
  none have contain spaces.
  When given the @DFlag{git} or @Flag{g} flag, it uses @exec{git mv} to
  move files (if it is available).

  Running @exec{make rename} in the root directory of the @tt{texts}
  repository renames files as needed (using @exec{git mv})
  in the @tt{TEI} directory.

  A bug in the implementation of @racketmodname[ricoeur/tei]
  (or perhaps in @exec{xmllint} itself) sometimes causes files
  that do not conform to these naming requirements to fail validation.
 }
 @item{@exec{raco ricoeur/tei to-plain-text} writes a
  TEI XML file to STDOUT as plain text.
  It is primarily intended to be used by invoking @exec{make}
  (or @exec{make all}) in the @tt{texts} repository,
  which will populate the
  @tt{plain-text} directory with plain text versions of
  every TEI XML file in the @tt{TEI} directory.
  If for some reason you want to use it directly,
  run @exec{raco tei-to-plain-text -h} for usage information.
 }
 @item{@exec{raco ricoeur/tei encode-xml-entities} should be
  run on plain text files that are going to be converted to
  TEI XML documents manually: it @bold{must not} be run
  on files that will be converted using ``TEI Lint''.
  The command must be run before adding any XML markup.
  It replaces the reserved characters @litchar{<} and @litchar{&}
  with the corresponding XML entities.
  Run it with the flag @DFlag{help} or @Flag{h} for usage information.
 }
 @item{@exec{raco ricoeur/tei guess-paragraphs} is a more limited
  substitute for functionality included in ``TEI Lint'': under most
  circumstances, ``TEI Lint'' should be preferred.

  The command
  replaces a TEI XML file with an equivalent in which paragraph
  breaks have been guessed using @racket[tei-document-guess-paragraphs].
  Run it with the flag @DFlag{help} or @Flag{h} for usage information.

  When @exec{xmllint} is available, the output will be prettyprinted.

  Please always check the output of this tool: it operates on
  a best-effort basis. If, for example, you notice that it has simply
  replaced each ``annonymous block'' with one long paragraph, it
  would be better to revert your change and wait for this tool
  to be improved than to commit such semantically meaningless output.

  ``TEI Lint'' includes the same functionality as this tool, but
  with better output checking, so it should generally be preferred.
  }]

