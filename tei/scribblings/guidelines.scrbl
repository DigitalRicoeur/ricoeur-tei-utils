#lang scribble/manual

@title[#:version ""]{TEI Encoding Guidelines for Digital Ricœur}

@(require racket/runtime-path
          racket/list
          (for-syntax racket/base
                      syntax/parse
                      adjutor
                      )
          (for-label ricoeur/tei
                     ))

@(define-runtime-path example-image
   "ricoeur-tei-example-cropped.png")

This document describes some basic guidelines for TEI tagging texts
for Digital Ricœur. A foundational assumption is that we want to
complete an initial encoding quickly by keeping the complexity of TEI
tagging to a minimum, while still producing well-formed XML
documents that will be valid per the TEI Document
Type Definition.

Our initial priorities are marking pagebreaks
and recording basic catalog information.

@section{Prerequisites}

@;Use best OCRed source available

@;Have excaped @litchar{<} and @litchar{&} with @litchar{&lt;} and @litchar{&amp;}

@section{Getting Started: Minimal Template for Valid TEI Documents}

The first step in preparing our corpus is converting the raw OCRed
text files into valid and well-formed TEI XML files. (These should
be saved with the extension @litchar{.xml}, rather than @litchar{.txt},
which is for plain text.)

The following describes a recommended template for quickly converting
our digitized texts into TEI XML format.
In the template, the body of the text
is enclosed in an "annonymous block",
which is a container for text that does not
specify any semantic meaning. This is a compromise, allowing us to achieve
a valid initial TEI encoding without spending the time to manually mark
sections and paragraphs.

In the common case that paragraphs are indicated
by blank lines, we can automatically convert an annonymous block
to a series of paragraph elements
with a Racket script using @(method TEI.2<%> guess-paragraphs).

In the following examples, syntax typeset like @litchar{this}
should appear verbatim. Keywords typeset like @racketvarfont{this}
indicate sections that should be filled in with a specific
type of content, which is explained below the example.
Whitespace is not significant.

@(filebox
  "example.xml"
  (verbatim
   @litchar{<?xml version="1.0" encoding="utf-8"?>}"\n"
   @litchar{<!DOCTYPE TEI.2 PUBLIC "-//TEI P4//DTD Main Document Type//EN" }"\n"
   "  "@litchar{"tei2.dtd" [}"\n"
   "    "@litchar{<!ENTITY % TEI.XML   'INCLUDE' >}"\n"
   "    "@litchar{<!ENTITY % TEI.prose 'INCLUDE' >}"\n"
   "    "@litchar{<!ENTITY % TEI.linking 'INCLUDE' >}"\n"
   @litchar{]>}"\n"
   @litchar{<TEI.2>}"\n"
   "  "@litchar{<teiHeader>}"\n"
   "    "@litchar{<fileDesc>}"\n"
   "      "@litchar{<titleStmt>}"\n"
   "        "(elemref "catalog-info" @racketvarfont{catalog-info})"\n"
   "      "@litchar{</titleStmt>}"\n"
   "      "@litchar{<publicationStmt>}"\n"
   "        "@litchar{<authority>Digital Ricoeur</authority>}"\n"
   "        "@litchar{<availability status="restricted">}"\n"
   "          "@litchar{<p>Not for distribution.</p>}"\n"
   "        "@litchar{</availability>}"\n"
   "      "@litchar{</publicationStmt>}"\n"
   "      "@litchar{<sourceDesc>}"\n"
   "        "@litchar{<bibl>}"\n"
   "          "(elemref "source-citation" @racketvarfont{source-citation})"\n"
   "        "@litchar{</bibl>}"\n"
   "      "@litchar{</sourceDesc>}"\n"
   "    "@litchar{</fileDesc>}"\n"
   "  "@litchar{</teiHeader>}"\n"
   "  "@litchar{<text>}"\n"
   "    "@litchar{<body>}"\n"
   "      "@litchar{<ab>}"\n"
   "        "(elemref "main-text" @racketvarfont{main-text})"\n"
   "      "@litchar{</ab>}"\n"
   "    "@litchar{</body>}"\n"
   "  "@litchar{</text>}"\n"
   @litchar{</TEI.2>}))

@(define (defkeyword it . body)
   (list
    (tabular #:style 'boxed
             (list (list (elemtag it (racketvarfont (larger (larger it)))))))
    (apply nested #:style 'inset body)))

@defkeyword["catalog-info"]{
 The @(elemref "catalog-info" @racketvarfont{catalog-info})
 should contain the following tags, which may appear in any order:
 @itemlist[@item[@litchar{<title>}@tt{the title of the work}@litchar{</title>}]
           @item[@litchar{<author>}
                 @tt{the author's name (usually @litchar{Paul Ricoeur})}
                 @litchar{</author>}]
           @item[@litchar{<editor role="editor">}
                 @tt{the name of an editor}
                 @litchar{</editor>}]]
 The @tt{author} and @tt{editor} tags may be repeated as necessary,
 and the @tt{editor} tag should be ommited if it doesn't apply.

 The @tt{role} attribute of the @tt{editor} tag, in addition to
 @tt{"editor"}, might use values like @tt{"translator"},
 @tt{"compiler"}, or @tt{"preface"} (for the author of a preface).
}

@defkeyword["source-citation"]{
 The @(elemref "source-citation" @racketvarfont{source-citation})
 should be free-form text specifying the source from which
 the digitized document was created
}

@defkeyword["main-text"]{
 The @(elemref "main-text" @racketvarfont{main-text}) part of the
 template is where the actual digitized text should be included,
 along with pagebreak tags.

 @; notes about other tags and non-destructive editing

 For more details about pagebreak tags, which are especially
 important for our initial goals, see @secref["Encoding Page Numbers"] @; FIX ME!!!
}

The following screenshot illustrates a valid TEI XML file based on this template:

@image[example-image #:scale 0.5]{
 The example document with XML syntax highlighting.
}

@(define-syntax (deftag stx)
   (syntax-parse stx
     [(_ name:id (~describe #:opaque "reference url string" ref-url:str)
         (~or (~optional (~seq (~and attributes? #:attributes)
                               ([attr-name:id attr-desc:expr] ...+)))
              (~optional (~seq (~and examples? #:eg) [example:expr ...+])))
         ...
         body:expr ...)
      #`(begin (tabular
                #:style 'boxed
                #:sep @hspace[1]
                #:column-properties '(top top)
                (list (list @elem{Tag:} (litchar (symbol->string 'name)))
                      #,@(list-when (attribute attributes?)
                           (list #`(list @elem{Attributes:}
                                         (tabular
                                          #:sep @hspace[1]
                                          (list (list (litchar (symbol->string 'attr-name))
                                                      @elem{:}
                                                      attr-desc) ...)))))
                      (list @elem{Reference:} (url ref-url))
                      #,@(list-when (attribute examples?)
                           (list #`(list @elem{Examples:}
                                         (tabular
                                          (add-between (list (list example) ...)
                                                       (list (linebreak)))))))
                      ))
               body ...)]))

@section{Encoding Page Numbers}

@deftag[pb "http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-pb.html"
        #:attributes ([n @elem{the page number}])
        #:eg (@litchar{<pb n="i"/>}
               @litchar{<pb n="42"></pb>}
               @litchar{<pb/>})]{
 The beginning of every new page (including the first page) must be marked by
 a @tt{pb} element. Unless the page is not assigned a number in the scanned
 original, the element should include the @litchar{n} attribute to denote the
 page number (perhaps as a Roman numeral).

 A @tt{pb} element should never appear between @tt{div} elements: instead,
 encode such pagebreaks at the very begining of the subsequent @tt{div} element.

 @italic{Adapted from @(url "https://www.cdlib.org/groups/stwg/MS_BPG.html#pb")}
}



                                                 
@;                                                  
@;       ;;;          ;;                            
@;     ;;             ;;                            
@;   ;;;;;;; ;;  ;; ;;;;;;;  ;;  ;;  ;; ;;;    ;;;  
@;     ;;    ;;  ;;   ;;     ;;  ;;  ;;;     ;;   ; 
@;     ;;    ;;  ;;   ;;     ;;  ;;  ;;      ;    ; 
@;     ;;    ;;  ;;   ;;     ;;  ;;  ;;     ;;;;;;;;
@;     ;;    ;;  ;;   ;;     ;;  ;;  ;;      ;      
@;     ;;     ; ;;;    ;      ; ;;;  ;;      ;;   ; 
@;     ;;      ; ;;     ;;;    ; ;;  ;;        ;;;  
@;                                                  
                                           

@section{Future Extensions: Additional Useful Tags}

Our initial focus is on creating valid TEI XML documents and encoding
page breaks. However, there are several additional TEI elements that
will be useful to our project in the future: some of these are documented in
this section. More details will be added as we draw closer to completing
the initial round of encoding.

@subsection{Front- and Back-matter}
@deftag[front "http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-front.html"]
@deftag[body "http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-body.html"]
@deftag[back "http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-back.html"]

While our template initially uses only the @litchar{body} element,
the enclosing @litchar{text} element can also contain a @litchar{front} element
— which should contain any front-matter (such as "abstracts, title page, prefaces,
dedications, etc.") and should come before the @litchar{body} —
and a @litchar{back} element — which contains back-matter, such as indeces or
appendices, and should come after the @litchar{body}.

Like the @litchar{body} element, @litchar{front} and @litchar{back}
@bold{MUST NOT} contrain text directly: they should contain elements like
@litchar{ab} or @litchar{div}.

@subsection{Chapters & Other Sections}
@deftag[div "http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-div.html"
        #:attributes ([type @elem{@tt{"chapter"}, @tt{"part"}, @tt{"section"},
                                @tt{"dedication"}, @tt{"contents"},
                                @tt{"ack"} (for acknowledgements), @tt{"index"},
                                and other specialised values}]
                      [n @elem{the chapter number, if applicable}])
        #:eg ((verbatim
               @litchar{<div type="chapter" n="1">}(linebreak)
               "  "@elem{Chapter text goes here.}(linebreak)
               @litchar{</div>}))]{
@; Can div contain <ab>? Can div contain immediate text?
}

@subsection{Footnotes & Endnotes}
@deftag[note "http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-note.html"
        #:attributes ([place @elem{@tt{"foot} or @tt{end}}]
                      [n @elem{the number or symbol for the footnote or endnote}]
                      [type @elem{use @tt{"transl} for translation notes}])
        #:eg (@elem[@litchar{<note place="foot" n="1">}
                    @tt{We explain below why we use the uncommon term older logical writings.}
                    @litchar{</note>}]
               @elem[@litchar{<note place="end">}
                     @tt{See Bachelard's Poetics of Space, Beacon Press, Boston (1969),
                              p. xxi; "retentissement" in French.}
                     @litchar{</note>}])]{
 Footnotes (those references, notes, and citations appearing at the bottom of the page)
 and endnotes (those which appear at the end of a book or article)
 must be encoded where they are referenced.
 In other words, at the location of the note reference in the text,
 embed the @litchar{<note>} itself in place.

 @italic{Adapted from @url["https://www.cdlib.org/groups/stwg/MS_BPG.html#fnote"]
 (but note that we encode endnotes differently)}
}

@subsection{Headings}
@deftag[head "http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-head.html"
        #:eg ((verbatim
               @litchar{<div type="chapter" n="1">}(linebreak)
               "  "@litchar{<pb n="1"/>}(linebreak)
               "  "@litchar{<head>The Question of Selfhood</head>}(linebreak)
               "  "@tt{...}(linebreak)
               @litchar{</div>}))]{
 Headings (such as chapter titles) should ideally be encoded with
 @litchar{head} elements, not @litchar{p} elements.                                                   
}

@subsection{Paragraphs}
@deftag[p "http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-p.html"
        ]{
 Except when they can be computed automatically, encoding paragraphs is
 a fairly low priority for our project at this stage.
}