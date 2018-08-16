#lang scribble/manual

@title{Getting Started}
@(require "for-guidelines.rkt")


This section explains the process of converting a raw text file
to a TEI XML document that satisfies our project's requirements.

@section{Prerequisites}

We should always start from the highest-quality OCRed text files.

In addition, before we start adding XML syntax, we must encode the 
reserved characters @litchar{<} and @litchar{&} in the text
with @litchar{&lt;} and @litchar{&amp;}, respectively. This can be
done automatically using the command-line utility
@exec{encode-xml-entities} as documented under @secref["Tools"] below.

@section{Minimal Template}

Once we have prepared a properly escaped text, we must add certain
boilerplate to make it into a valid and well-formed TEI XML file.
(In practice, we are also adding @tag{pb} tags at this stage.)
This section describes a template for this step.

The finished documents should 
be saved with the extension @litchar{.xml}, rather than @litchar{.txt},
which is for plain text.

In the template, the body of the text
is enclosed in an @tag{ab} (``anonymous block'') element,
which is a container for marked-up text
(including pagebreaks marked with @tag{pb} elements)
that does not specify any semantic meaning.
This is a compromise, allowing us to achieve
a valid initial TEI encoding without spending the time to manually mark
sections and paragraphs.

In the common case that paragraphs are indicated
by line breaks or blank lines, we can automatically convert an annonymous block
to a series of paragraph elements using @secref["TEI_Lint"].

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
 See the complete listing unger @tag{editor} below.
}

@defkeyword["source-citation"]{
 The @kwref{source-citation} (i.e. the content of the @tag{bibl} element)
 should be free-form text specifying the source from which
 the digitized document was created — for example, as
 drawn from the ``Books in English'' spreadsheet in our Google Drive folder.

 The parts of the citation that refer to the publication date must be
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
 must contain either one or two @tag{date} elements:
 @(itemlist
   @item{If the digitized document is based on the first
  @lib-tech{instance} to be published in any language,
  there must be one @tag{date} element with a @attr{subtype}
  of @racket["thisIsOriginal"] marking that date.
 }
   @item{Otherwise, there must be two @tag{date} elements:
  one with a @attr{subtype} of @racket["this"] marking the
  publication date of the specific @lib-tech{instance} from
  which the digitized document was prepared,
  and one with a @attr{subtype} of @racket["original"] giving
  the first publication date in any language.
  })

 For compilations of articles, the @racket["thisIsOriginal"]
 or @racket["original"] @tag{date} refers to the first 
 publication of the collection as a whole.
 
 In either case, make sure that the textual content of the
 @kwref{source-citation} as a whole is understandable if
 displayed without the tags, as it will be shown in that format
 to end-users.

 Examples:
 @(itemlist
   @item{@tt{@litchar{<bibl>}Fallible Man. Introduction by C.A. Kelbley.
   Chicago: Henry Regnery,
   @litchar{<date type="publication" subtype="this" when="1965">}1965@litchar{</date>},
   xxix-224 p. (Paper: Gateway Editions).
   Revised edition in 1986.
   First published in French in
   @litchar{<date type="publication" subtype="original" when="1960">}1960@litchar{</date>}.@litchar{</bibl>}}}
   @item{@tt{@litchar{<bibl>}Can Fictional Narratives be true? 
   Analecta Hus­serliana Vol. XIV
   (@litchar{<date type="publication" subtype="thisIsOriginal" when="1983">}1983@litchar{</date>})
   3-19.@litchar{</bibl>}}})              
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
   }]
}

@defkeyword["text-lang"]{
 The @kwref{text-lang} (i.e. the value of the @attr{xml:lang}
 attribute of the @tag{text} element) must be either:
 @itemlist[
 @item{@racket["en"] if the document is primarily in English; or}
 @item{@racket["fr"] if the document is primarily in French.}]
}

@defkeyword["main-text"]{
 The @kwref{main-text} part of the
 template is where the actual digitized text should be included,
 along with pagebreak tags.

 When prepairing the text, we should be careful to practice non-destructive
 editing. For example, while we aren't focusing on adding @tag{note}
 tags at this stage, we should leave the numbers in place for footnotes and
 endnotes so that we can add them later. However, we should remove redundant
 ``decorative'' text (like the title of a book printed at the top of every page)
 that isn't really part of the work itself, and it is always good to correct
 OCR errors if we see them.

 For more details about pagebreak tags, which are especially
 important for our initial goals, see @secref["Encoding_Page_Numbers"]. 
}