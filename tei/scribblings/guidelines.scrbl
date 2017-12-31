#lang scribble/manual

@title[#:version ""]{TEI Encoding Guidelines for Digital Ricœur}

@(require racket/runtime-path
          racket/list
          (submod ricoeur/tei/xexpr/specification doc)
          (for-syntax racket/base
                      syntax/parse
                      adjutor
                      )
          (for-label ricoeur/tei
                     ))

This manual specifies how to encode texts for Digital Ricœur.

A foundational assumption is that we want to
complete an initial encoding quickly by keeping the complexity of TEI
tagging process to a minimum, while still producing well-formed XML
documents that conform to the TEI standards.
Our initial priorities are marking pagebreaks
and recording basic catalog information.

To that end, we have defined a custom subset of TEI, which is codified
in the @tt{DR-TEI.dtd} Document Type Definition (DTD).
(A DTD is a formal, computer-checkable specification of the structure of
an XML document.)
To ensure consistency and facilitate the development of tools,
we also impose additional requirements beyond those specified by the DTD.
These requirements are documented in this manual.

@margin-note{
 For additional documentation pertaining only to the requirements specified
 by the DTD, see @url{https://manuals.digitalricoeur.org/DR-TEI_doc.html}.
}

We have implemented a number of tools
to assist in preparing and validating TEI XML documents,
which are documented under @secref["Tools"].

@(table-of-contents)

@include-section["background.scrbl"]

@section{Getting Started}

This section explains the process of converting a raw text file
to a TEI XML document that satisfies our project's requirements.

@subsection{Prerequisites}

We should always start from the highest-quality OCRed text files.

In addition, before we start adding XML syntax, we must encode the 
reserved characters @litchar{<} and @litchar{&} in the text
with @litchar{&lt;} and @litchar{&amp;}, respectively. This can be
done automatically using the command-line utility
@exec{encode-xml-entities} as documented under @secref["Tools"] below.

@subsection{Minimal Template}

Once we have prepared a properly escaped text, we must add certain
boilerplate to make it into a valid and well-formed TEI XML file.
(In practice, we are also adding @tag{pb} tags at this stage.)
This section describes a template for this step.

The finished documents should 
be saved with the extension @litchar{.xml}, rather than @litchar{.txt},
which is for plain text.
They should also be moved to the @tt{TEI} directory
in the @tt{texts} repository.

In the template, the body of the text
is enclosed in an @tag{ab} ("anonymous block") element,
which is a container for marked-up text
(including pagebreaks marked with @tag{pb} elements)
that does not
specify any semantic meaning. This is a compromise, allowing us to achieve
a valid initial TEI encoding without spending the time to manually mark
sections and paragraphs.

In the common case that paragraphs are indicated
by blank lines, we can automatically convert an annonymous block
to a series of paragraph elements
with a Racket script using @(method guess-paragraphs<%> guess-paragraphs).

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
   @litchar{<!DOCTYPE TEI SYSTEM "DR-TEI.dtd">}"\n"
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
   "        "@litchar{<catRef scheme="https://schema.digitalricoeur.org/"}"\n"
   "        "@(indent "<catRef scheme=\"ht")@litchar{taxonomy/type"}"\n"
   "        "@(indent "<catRef ")@litchar{target=}@kwref{book/article-target}@litchar{ />}"\n"
   "        "@litchar{<keywords scheme="https://schema.digitalricoeur.org/"}"\n"
   "        "@(indent "<keywords scheme=\"ht")@litchar{tools/tei-guess-paragraphs">}"\n"
   "          "@litchar{<term>todo</term>}"\n"
   "        "@litchar{</keywords>}
   "      "@litchar{</textClass>}"\n"
   "    "@litchar{</profileDesc>}"\n"
   "  "@litchar{</teiHeader>}"\n"
   "  "@litchar{<text>}"\n"
   "    "@litchar{<body>}"\n"
   "      "@litchar{<ab>}"\n"
   "        "@kwref{main-text}"\n"
   "      "@litchar{</ab>}"\n"
   "    "@litchar{</body>}"\n"
   "  "@litchar{</text>}"\n"
   @litchar{</TEI>}))

@(define (defkeyword it . body)
   (list
    (tabular #:style 'boxed
             (list (list (elemtag it (racketvarfont (larger (larger it)))))))
    (apply nested #:style 'inset body)))

@defkeyword["title-statement"]{
 The @kwref{title-statement}
 should contain the following tags, which may appear in any order:
 @itemlist[@item[@litchar{<title>}
                 @elem{the title of the work}
                 @litchar{</title>}]
           @item[@litchar{<author>}
                 @elem{the author's name}
                 @litchar{</author>}]
           @item[@litchar{<editor role="editor">}
                 @elem{the name of an editor}
                 @litchar{</editor>}]]
 
 The @tag{author} and @tag{editor} tags may be repeated as necessary,
 and the @tag{editor} tag should be ommited if it doesn't apply.

 The @tag{author} element representing Paul Ricœur should be
 exactly as follows:
 @(nested #:style 'inset
          @litchar{<author xml:id="ricoeur">Paul Ricoeur</author>})

 The @attr{role} attribute of the @tag{editor} tag, in addition to
 @racket["editor"], might use values like @racket["translator"],
 @racket["compiler"], or @racket["preface"] (for the author of a preface).
}

@defkeyword["source-citation"]{
 The @kwref{source-citation}
 should be free-form text specifying the source from which
 the digitized document was created — for example, as
 drawn from the "Books in English" spreadsheet in our Google Drive folder.

 The parts of the citation that refers to the publication date must be
 wrapped in a @tag{date} element, which must have:
 @itemlist[
 @item{a @attr{type} attribute with a value of @racket["publication"];}
 @item{a @attr{subtype} attribute with a value of @racket["this"],
   @racket["original"], or @racket["thisIsOriginal"]; and}
 @item{a @attr{when} attribute giving the
   date in machine-readable format: @racket["YYYY-MM-DD"], @racket["YYYY-MM"],
   or @racket["YYYY"], where the month
   and day, if present, must allways be two digits
   (e.g. @tt{01} for January).}]

 The @kwref{source-citation}
 must contain either one @tag{date} element with a @attr{subtype} of
 @racket["thisIsOriginal"] or two @tag{date} elements,
 one with a @attr{subtype} of @racket["this"]
 and one with a @attr{subtype} of @racket["original"].
 The @racket["this"] @tag{date} encodes the publication date of the
 specific version from which the digitized document was created,
 and the @racket["original"] @tag{date} encodes the date when the
 work was first published. (For compilations of articles, both
 dates refer to the collection as a whole.) The option of using
 @racket["thisIsOriginal"] is provided for convienence in the case
 that the specific version on which the digitized document is based
 is the first published version.
}

@defkeyword["book/article-target"]{
 The @kwref{book/article-target} (i.e. the value of the @attr{target}
 attribute of one @tag{catRef} element) must be either:
 @itemlist[
 @item{@racket["https://schema.digitalricoeur.org/taxonomy/type#article"],
   if the document is an article; or
  }
 @item{@racket["https://schema.digitalricoeur.org/taxonomy/type#book"],
   if the document is a book.
   }]
}

@defkeyword["main-text"]{
 The @kwref{main-text} part of the
 template is where the actual digitized text should be included,
 along with pagebreak tags.

 When prepairing the text, we should be careful to practice non-destructive
 editing. For example, while we aren't focusing on adding @tag{note}
 tags at this stage, we should leave the numbers in place for footnotes and
 endnotes so that we can add them later. However, we should remove redundant
 "decorative" text (like the title of a book printed at the top of every page)
 that isn't really part of the work itself, and it is always good to correct
 OCR errors if we see them.

 For more details about pagebreak tags, which are especially
 important for our initial goals, see @secref["Encoding_Page_Numbers"]. 
}

@;{
 ;                                                                  
 ;                                                                  
 ;                                                                  
 ;                                                                  
 ;                       ;;;    ;               ;                   
 ;                     ;;       ;;              ;;                  
 ;   ;; ;;;    ;;;   ;;;;;;; ;;;;;   ;; ;    ;;;;;   ;; ;      ;;;;;
 ;   ;;;     ;;   ;    ;;       ;;   ;;; ;      ;;   ;;; ;    ;  ;  
 ;   ;;      ;    ;    ;;       ;;   ;;  ;;     ;;   ;;  ;;  ;;  ;; 
 ;   ;;     ;;;;;;;;   ;;       ;;   ;;  ;;     ;;   ;;  ;;   ;  ;  
 ;   ;;      ;         ;;       ;;   ;;  ;;     ;;   ;;  ;;    ;;   
 ;   ;;      ;;   ;    ;;       ;;   ;;  ;;     ;;   ;;  ;;  ;;     
 ;   ;;        ;;;     ;;       ;;   ;;  ;;     ;;   ;;  ;;   ;;;;; 
 ;                                                           ;    ;;
 ;                                                          ;;    ; 
 ;                                                            ;;;;  
 ;                                                                  
}

@section{Refining the Encoding}

This section provides more details about specific tasks in the encoding
process after the initial preparation of a valid and well-formed TEI XML
document (which is described in @secref["Getting_Started"]).

They are loosely organized by the order
in which an encoder would likely want to perform them.

@subsection{Encoding Page Numbers}

The beginning of every new page (including the first page) must be marked by
a @tag{pb} element. Unless the page is not assigned a number in the scanned
original, the element should include the @attr{n} attribute to denote the
page number (perhaps as a Roman numeral).

@subsection{Front- and Back-matter}

The minimal template described in @secref["Getting_Started"] initially
wraps the entire text of the document in a single @tag{ab} element in
the @tag{body} element of the @tag{text} element.
However, TEI also provides elements for marking front-matter
(such as "abstracts, title page, prefaces, dedications, etc.") and
back-matter (such as indices or appendices).

When front-matter is present, it should be placed in a @tag{front} element
immedidiately before the @tag{body} element inside the @tag{text} element.
Likewise, when back-matter is present, it should be placed in a @tag{back}
element immediately after the @tag{body} element inside the @tag{text} element.

Like the @tag{body} element, @tag{front} and @tag{back}
@bold{MUST NOT} contain text directly: they should contain elements like
@tag{ab} or @tag{div}.

If you are working from the recomended minimal template,
you will need to split the @tag{ab}
element into multiple @tag{ab} elements accordingly. For example,
from a document with the @tag{text} element structured like this:
@(nested
  #:style 'inset
  (verbatim
   @litchar{<text>}"\n"
   "  "@litchar{<body>}"\n"
   "    "@litchar{<ab>}"\n"
   "      "@elem{All of the text here}"\n"
   "    "@litchar{</ab>}"\n"
   "  "@litchar{</body>}"\n"
   @litchar{</text>}))

You might produce a new @tag{text} element like this:

@(nested
  #:style 'inset
  (verbatim
   @litchar{<text>}"\n"
   "  "@litchar{<front>}"\n"
   "    "@litchar{<ab>}"\n"
   "      "@elem{Front-matter goes here}"\n"
   "    "@litchar{</ab>}"\n"
   "  "@litchar{</front>}"\n"
   "  "@litchar{<body>}"\n"
   "    "@litchar{<ab>}"\n"
   "      "@elem{The main body goes here}"\n"
   "    "@litchar{</ab>}"\n"
   "  "@litchar{</body>}"\n"
   "  "@litchar{<back>}"\n"
   "    "@litchar{<ab>}"\n"
   "      "@elem{Back-matter goes here}"\n"
   "    "@litchar{</ab>}"\n"
   "  "@litchar{</back>}"\n"
   @litchar{</text>}))

@subsection{Chapters & Other Sections}
 
Once we have tagged the front- and back-matter,
the next step is marking large-scale structural divisions
(like chapters or sections).

These divisions are
marked with @tag{div} tags, which have a @attr{type} attribute that has
one of a fixed list of values to denote the type of division,
such as @racket["chapter"] or @racket["section"].
(See @secref["Structural_Elements"] under @secref["Formal_Specification"]
for the complete list of allowable @attr{type} values.) If the division
is numbered in the text, the number should be given in an @attr{n} attribute.

Note that @litchar{div} elements @bold{MUST NOT} directly contain text:
the contents of the
@litchar{div} must be wrapped in @litchar{p}, @litchar{ab}, @litchar{head} or
similar tags.

A few special @attr{type}s of @tag{div} are a particular priority for encoding,
as they can improve our search feature:
@(itemlist
  @item{@racket["contents"] (for the table of contents)}
  @item{@racket["intro"] (for an introduction)}
  @item{@racket["index"]}
  @item{@racket["bibl"] (for a bibliograpgy)}
  @item{@racket["dedication"]}
  @item{@racket["ack"] (for acknowledgements)})

As an example, if by following the steps above you have produced a @tag{body}
element like this:

@(nested
  #:style 'inset
  (verbatim
   @litchar{<body>}"\n"
   "  "@litchar{<ab>}"\n"
   "    "@elem{Content from two chapters}"\n"
   "  "@litchar{</ab>}"\n"
   @litchar{</body>}))

You might encode the chapters like this:

@(nested
  #:style 'inset
  (verbatim
   @litchar{<body>}"\n"
   "  "@litchar{<div type="chapter" n="1">}"\n"
   "    "@litchar{<ab>}"\n"
   "      "@elem{Content from chapter one}"\n"
   "    "@litchar{</ab>}"\n"
   "  "@litchar{</div>}"\n"
   "  "@litchar{<div type="chapter" n="2">}"\n"
   "    "@litchar{<ab>}"\n"
   "      "@elem{Content from chapter two}"\n"
   "    "@litchar{</ab>}"\n"
   "  "@litchar{</div>}"\n"
   @litchar{</body>}))

@subsubsection{Sections Not by Ricœur}

Some works contain sections that are not written by Paul Ricœur.
Marking these sections as such is a particular priority, as they
should be excluded from search results.

Some preparation is needed to encode authorship in a machine-readable
manner. For each author or editor who needs to be identified, you
must add an @attr{xml:id} attribute to the corresponding
@tag{author} or @tag{editor} element in the @tag{titleStmt}.
The value for this attribute must be unique across the entire document:
often, the last name is a good choice. Note that the value 
@racket["ricoeur"] is reserved across all documents for the 
@tag{author} element representing Paul Ricœur.

Once these identifiers have been assigned, you can mark a @tag{div}
element as being by a particular author/editor by adding a  
@attr{resp} attribute. The value for the @attr{resp} attribute
must point to the corresponding identifier by prefixing it with the
character @tt{#}. For example, if you created an author with
@tt{xml:id="smith"}, you would write @tt{resp="#smith"}.

Any @tag{div} elements that do not have a @attr{resp} attribute are
assumed to be by Paul Ricœur. In addition, there is no need to add
a @attr{resp} attribute for @tag{div} elements with a @attr{type} of
@racket["contents"] or @racket["index"].

A few documents, such as "Tragic Wisdom and Beyond", take the form
of an extended dialogue between Paul Ricœur and some other party.
The speakers in these documents are encoded through a special process.
Rather than adding a @attr{resp} attribute to the @tag{div} elements,
each passage where a distinct individual is speaking should be enclosed
in a @tag{sp} ("speech") element. This element must have a @attr{who} attribute
that points to the speaker as described above. (The @attr{who} attribute
is required even when the speaker is Paul Ricœur.) Note that, like the
@tag{div} element, the @tag{sp} element must not directly contain text.
Because the information is contained in the @attr{who} attribute,
the notation of the speaker in the text itself can be removed.

@subsection{Footnotes & Endnotes}

Footnotes (those references, notes, and citations appearing at the bottom of the page)
and endnotes (those which appear at the end of a book or article)
are encoded using the @tag{note} element.
It must have a @attr{place} attribute with a value of either @racket["foot"] or
@racket["end"]. It must also have a @attr{n} attribute giving the
number or symbol used to reference the note in the original.

Notes must be encoded where they are referenced.
In other words, at the location of the note reference in the text,
embed the @tag{note} element itself in place.

Translation notes should have a @attr{type} attribute with a value of @racket["transl"].

Any note which was added by someone other than Paul Ricœur (such as an editor or
translator) must have a @attr{resp} attribute with an appropriate value as
described under @secref["Sections_Not_by_Ric_ur"] above.
 
Examples:
@(nested
  #:style 'inset
  @para[@litchar{<note place="foot" n="1">}
        @tt{We explain below why we use the uncommon term older logical writings.}
        @litchar{</note>}]
  @para[@litchar{<note place="end" n="*">}
        @tt{See Bachelard's Poetics of Space, Beacon Press, Boston (1969),
         p. xxi; "retentissement" in French.}
        @litchar{</note>}])


@italic{Adapted from @url["https://www.cdlib.org/groups/stwg/MS_BPG.html#fnote"]
 (but note that we encode endnotes differently)}

@subsection{Paragraphs, Headings, and Lists}

Except when they can be computed automatically, encoding paragraphs is
probably a fairly low priority for our project at this stage. Ultimately,
however, each paragraph should be wrapped in a @tag{p} element.
Headings (such as chapter titles) should ideally be encoded with
@tag{head} elements, not @tag{p} elements. 

Lists are encoded specially. The list as a whole is wrapped in a @tag{list}
element. If it is a numbered list, it should have a @attr{rend} attribute
with a value of @racket["numbered"]. Individual items on the list are
wrapped with @tag{item} elements.

Example:
@(nested
  #:style 'inset
  (verbatim
   @tt{<div type="chapter" n="1">}(linebreak)
   "  "@tt{<pb n="1"/>}(linebreak)
   "  "@tt{<head>The Question of Selfhood</head>}(linebreak)
   "  "@tt{<p>This is a <pb n="2"/> paragraph.</p>}(linebreak)
   "  "@elem{…}(linebreak)
   @tt{</div>}))







@include-section[(submod ricoeur/tei/xexpr/specification doc)]
@include-section["tools.scrbl"]

