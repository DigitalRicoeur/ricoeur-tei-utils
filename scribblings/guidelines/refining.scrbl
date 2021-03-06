#lang scribble/manual

@title{Refining the Encoding}
@(require "for-guidelines.rkt")

This section provides more details about specific tasks in the encoding
process after the initial preparation of a valid and well-formed TEI XML
document (which is described in @secref["Getting_Started"]).

They are loosely organized by the order
in which an encoder would likely want to perform them.

@section{Front- and Back-matter}

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

@section{Chapters & Other Sections}
 
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

@subsection{Sections Not by Ricœur}

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

@section{Footnotes & Endnotes}

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

@section{Paragraphs, Headings, and Lists}

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







