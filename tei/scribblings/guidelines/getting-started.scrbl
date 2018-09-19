#lang scribble/manual

@title{Getting Started}
@(require "for-guidelines.rkt")


This section explains the process of converting 
a scanned PDF document to an initial TEI XML file
that satisfies our project's requirements.

The first step in this process is to extract the plain
text from the PDF document using Optical Character Recognition (OCR)
software.
The plain text should be saved in a file with the extension @litchar{.txt}.
Our current strategy is to take care at this initial stage
to produce the highest-quality plain text version possible:
for example, we will attempt to remove purely decorative page
headers and footers at this stage.
You will receive additional guidance if you are participating
in this step of the process.
The @bold{most important} requirement for this step is
that we must use the OCR software to mark each page-break in
the generated plain text file with an ASCII ``form feed'' character.

@margin-note{
 The ``form feed'' character is often notated as @racket["\f"].
 In Racket, it is @racket[#\page], which is the value of
 @racket[(integer->char 12)].
}

After producing a plain text file, the next step is to
transform it into an intial TEI XML document.
Our GUI tool ``TEI Lint'' (see @secref["TEI_Lint"], below)
performs most of this process automatically:
you need only fill in a form with some basic information about the document.
A few of the details are worth addressing specificly:
@itemlist[
 @item{You will need to provide a title for the document, which will be used
  to fill in the TEI @tag{title} element.
  The title needs to be sufficient to unambigously refer to
  the specific instance in question.
  In some cases, this means that it will be necessary to
  add disambiguating information:
  for example, use ``Introduction [in Philosophical Foundations of Human Rights]''
  rather than simply ``Introduction.''
 }
  @item{You will also asked be asked to provide a human-readable citation
  specifying the source from which the digitized document was
  created---for example, as drawn from the ``Books in English'' spreadsheet
  in our Google Drive folder.
  This will become part of the content of the @tag{bibl} element.
 }
  @item{Each document will automatically include an @tag{author} element
  representing Paul Ricœur.
  You must also enter information on any additional authors, editors,
  translators, @etc using the ``TEI Lint'' interface.
  This information will be used to generate additional @tag{author}
  and @tag{editor} elements.
 }
  @item{A particularly important part of the initial encoding process
  is adding page numbers.
  The ``TEI Lint'' interface requires you to account for every page
  in the document.
  Pages my be assigned Roman or Arabic numerals or be marked as unnumbered
  in the source, and consecutive pages may be numbered in one step.
  For example, a book might begin with 5 unnumbered pages,
  then 12 pages with Roman numerals beginning with ``i,''
  and finally 403 pages with Arabic numerals beginning with ``1.''
  An article, on the other hand, might consist of 18 pages
  with Arabic numerals beginning with ``386.''
  The numbering information will be used to generate @tag{pb} elements.
  }]

Once you have provided all of the required information,
``TEI Lint'' will allow you to save the document as a TEI XML file
(with the extension @litchar{.xml}).
The finished files should generally be added to the ``TEI''
directory of the ``texts'' repository,
which is the basis of the corpus available through the portal website.

@section{Minimal Template}

The initial TEI XML documents generated by ``TEI Lint'' are
structured according to a ``minimal template'' documented here.
You will need to understand the general structure of this
template in order to proceed to the additional encoding tasks
documented under @secref["Refining_the_Encoding"].
You should also follow this template if you are prepairing a
TEI XML document manually, without using ``TEI Lint.''

@margin-note{
 If you are prepairing a TEI XML document manually,
 before adding any XML markup, you @bold{must} replace
 the reserved characters @litchar{<} and @litchar{&} in the
 plain text file with the XML entities
 @litchar{&lt;} and @litchar{&amp;}, respectively.
 The command-line tool @exec{raco ricoeur/tei encode-xml-entities}
 can do this automatically: see @secref["raco_ricoeur_tei"] for details.
}

Initially, we enclose all of the text
in a single @tag{ab} (``anonymous block'') element,
which is a container for marked-up text
(including page-breaks marked with @tag{pb} elements)
that does not specify any semantic meaning.
This is a compromise, allowing us to achieve
a valid initial TEI encoding without spending the time to manually mark
sections and paragraphs.

In the following example, syntax typeset like @litchar{this}
should appear verbatim. Keywords typeset like @racketvarfont{this}
indicate sections that should be filled in with a specific
type of content, which is explained below the example.
Whitespace (including indentation) is not significant.

@(define (kwref it)
   (elemref it (racketvarfont it)))

@(define (indent str)
   (make-string (string-length str) #\space))

@(filebox
  "example.xml"
  (verbatim
   @litchar{<?xml version="1.0" encoding="utf-8"?>}"\n"
   @litchar{<TEI version="5.0" xmlns="http://www.tei-c.org/ns/1.0">}"\n"
   "  "@litchar{<teiHeader>}"\n"
   "    "@litchar{<fileDesc>}"\n"
   "      "@litchar{<titleStmt>}"\n"
   "        "@kwref{title-statement}"\n"
   "      "@litchar{</titleStmt>}"\n"
   "      "@litchar{<publicationStmt>}"\n"
   "        "@litchar{<authority>Digital Ricoeur</authority>}"\n"
   "        "@litchar{<availability status="restricted">}"\n"
   "          "@litchar{<p>Not for distribution.</p>}"\n"
   "        "@litchar{</availability>}"\n"
   "      "@litchar{</publicationStmt>}"\n"
   "      "@litchar{<sourceDesc>}"\n"
   "        "@litchar{<bibl>}"\n"
   "          "@kwref{source-citation}"\n"
   "        "@litchar{</bibl>}"\n"
   "      "@litchar{</sourceDesc>}"\n"
   "    "@litchar{</fileDesc>}"\n"
   "    "@litchar{<profileDesc>}"\n"
   "      "@litchar{<textClass>}"\n"
   "        "@litchar{<catRef scheme="https://schema.digitalricoeur.org/}"\n"
   "        "@(indent "<catRef scheme=\"ht")@litchar{taxonomy/type"}"\n"
   "        "@(indent "<catRef ")@litchar{target=}@kwref{book/article-target}@litchar{ />}"\n"
   "        "@litchar{<keywords scheme="https://schema.digitalricoeur.org/}"\n"
   "        "@(indent "<keywords scheme=\"ht")@litchar{tools/tei-guess-paragraphs">}"\n"
   "          "@litchar{<term>todo</term>}"\n"
   "        "@litchar{</keywords>}"\n"
   "      "@litchar{</textClass>}"\n"
   "    "@litchar{</profileDesc>}"\n"
   "  "@litchar{</teiHeader>}"\n"
   "  "@litchar{<text xml:lang=}@kwref{text-lang}@litchar{>}"\n"
   "    "@litchar{<body>}"\n"
   "      "@litchar{<ab>}"\n"
   "        "@kwref{main-text}"\n"
   "      "@litchar{</ab>}"\n"
   "    "@litchar{</body>}"\n"
   "  "@litchar{</text>}"\n"
   @litchar{</TEI>}))

@(define (defkeyword it . body)
   (define key
     (toc-target2-element #f
                          (racketvarfont (larger (larger it)))
                          `(elem ,it)
                          (racketvarfont it)))
   (list
    (tabular #:style 'boxed
             (list (list key)))
    (apply nested #:style 'inset body)))

@defkeyword["title-statement"]{
 The @kwref{title-statement} (i.e. the body of the @tag{titleStmt}
 element) must contain a single @tag{title} element,
 one or more @tag{author} elements, and any number of @tag{editor}
 elements.

 There must always be an @tag{author} element representing Paul Ricœur,
 which should be exactly as follows:
 @(nested #:style 'inset
          @litchar{<author xml:id="ricoeur">Paul Ricoeur</author>})
}

@defkeyword["source-citation"]{
 The @kwref{source-citation} (i.e. the content of the @tag{bibl} element)
 should be free-form text specifying the source from which
 the digitized document was created---for example, as
 drawn from the ``Books in English'' spreadsheet in our Google Drive folder.

 The parts of the citation that refer to the publication date must be
 wrapped in @tag{date} elements.
 The @kwref{source-citation} must contain either one or two
 @tag{date} elements, depending on whether the source from
 which the document was prepared is the first published version in
 any language: see the formal specification of the @tag{bibl}
 and @tag{date} elements for more details and examples.
}

@defkeyword["book/article-target"]{
 The @kwref{book/article-target} (i.e. the value of the @attr{target}
 attribute of the @tag{catRef} element) must be either:
 @itemlist[
 @item{@racket["https://schema.digitalricoeur.org/taxonomy/type#article"],
   if the document is an article; or
  }
 @item{@racket["https://schema.digitalricoeur.org/taxonomy/type#book"],
   if the document is a book.
   }]}

@defkeyword["text-lang"]{
 The @kwref{text-lang} (i.e. the value of the @attr{xml:lang}
 attribute of the @tag{text} element) must be either:
 @itemlist[
 @item{@racket["en"] if the document is primarily in English;}
 @item{@racket["fr"] if the document is primarily in French; or}
 @item{@racket["de"] if the document is primarily in German.}
]}

@defkeyword["main-text"]{
 The @kwref{main-text} part of the
 template is where the actual digitized text should be included,
 including @tag{pb} elements to mark page breaks.

 A @tag{pb} element must be inserted to mark the beginning of
 every page, including the first page.
 Unless the page is not assigned a number in the scanned original,
 the element should include the @attr{n} attribute to denote the page number,
 perhaps as a Roman numeral.

 When prepairing the text, we should be careful to practice non-destructive
 editing. For example, while we aren't focusing on adding @tag{note}
 tags at this stage, we should leave the numbers in place for footnotes and
 endnotes so that we can add them later. However, we should remove redundant
 ``decorative'' text (like the title of a book printed at the top of every page)
 that isn't really part of the work itself, and it is always good to correct
 OCR errors if we see them.
}