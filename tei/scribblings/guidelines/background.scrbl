#lang scribble/manual

@title[#:version ""]{Background: XML and TEI}

@(require (submod ricoeur/tei/kernel doc))

These guidelines assume familiarity with basic concepts of
XML and the TEI system. This section serves as a brief
review of some key points and refers readers in need of
more extensive background information to external resources.

@section{XML}

XML ("eXtensible Markup Language") is a system for (among other things)
adding structured, machine-readable @tech{metadata} to text-based
documents. It is maintained as an international standard by the
World Wide Web Consortium (W3C).

@margin-note{@deftech{Metadata} is data about data. For example,
 in our context, the sentence @tt{"The text used here is
  B. Jowett's translation of The Dialogues of Plato, 
  Vol. I, Random House, New York."} is a piece of data.
 The information that the sentence was an end-note to page 135 of
 Ricœur's "The Function Of Fiction In Shaping Reality" and that it was
 added by the translator is @tech{metadata}.}

Describing XML as a "markup language" is a statement about its
@deftech{concrete syntax}: the way in which it is written down.
In general terms, an XML document begins with a human-readable
plain-text document and then "marks up" the structure of the document
and additional @tech{metadata} using @deftech{tags}, special bits
of syntax enclosed in angle brackets (@litchar{<} and @litchar{>}).
@margin-note{This is a simplification: comments, declarations, and
 processing instructions in XML also use angle brackets, but are not
 considered @tech{tags} in this sense.}

Consider the following example:
@(nested
  #:style 'inset
  @litchar{<bibl>}@tt{Paul Ricoeur, "The Function Of Fiction In
 Shaping Reality", in Man and World 12:2
 (}@litchar{<date subtype="thisIsOriginal" type="publication" when="1979">}@tt{1979}@litchar{</date>}@tt{),
 123-141}@litchar{</bibl>})

In this example, the portions typeset like @tt{this} are
data, and the portions typeset like @litchar{this} are XML
syntax. Considering only the XML syntax, we see an opening
@tag{bibl} @tech{tag}, an opening @tag{date} @tech{tag},
a closing @tag{date} @tech{tag},
and a closing @tag{bibl} @tech{tag}. Notice in particular
that every opening @tech{tag} has a corresponding closing @tech{tag}
and that the most recently opened @tech{tag} must always be
closed before an outer @tech{tag} can be closed.
@margin-note{XML provides a shorthand for writing tags that
 are immediately closed: for example, writing
 @litchar{<pb n="0" />} is equivalent to
 @litchar{<pb n="0"></pb>}.}

While the @tech{concrete syntax} of an XML document looks like
a sequence of characters, much of the power of XML derives
from the fact that an XML document actually specifies a 
@hyperlink["https://en.wikipedia.org/wiki/Tree_(data_structure)"]{tree}
data structure of nested @tech{elements}.
An @deftech{element} is an abstract, logical entity which may
contain textual data and/or other @tech{elements}.

In the example above, the whole example is a @tag{bibl} element,
which contains both textual data (a human-readable citation)
and a @tag{date} element, which marks part of the citation
as specifying a publication date.

@margin-note{Readers will notice the close relationship between
@tech{elements}, the abstract, logical entities, and @tech{tags},
the notations in XML's @tech{concrete syntax} that mark them.
In practice, "element" and "tag" are often used synonymously.}

In addition to its contents, an @tech{element} may have
@deftech{attributes}, which provide additional machine-readable
@tech{metadata} about the @tech{element}. Each @tech{attribute} has
a @italic{name} and, when present, is assigned a @italic{value}.
In our example, the @tag{date} @tech{element} has an @tech{attribute}
named @attr{when} with a value of @racket["1979"]. This attribute
encodes the date specified by the @tech{element} in a standard,
machine-readable format.

We also rely on a very minimal understanding of the XML concept
of @deftech{entities}. In XML's @tech{concrete syntax}, the 
characters @litchar{&} and @litchar{<} have special meaning, and 
therefore are not allowed in textual data. They must be 
replaced with the corresponding XML @tech{entities} 
@litchar{&amp;} and @litchar{&lt;}, respectively, as discused
under @secref["Prerequisites"] below. No attempt is made here
to explain the other, more advanced uses of @tech{entities} in XML.

XML is specifically an "extensible" markup language because,
beyond the common @tech{concrete syntax} of @tech{tags} and its
interpretation as @tech{elements}, @tech{attributes}, and
@tech{entities}, it makes little attempt to specify the
structure or meaning of an XML document.
Those aspects are left to specific applications of XML, which
can vary from recipies to entries in library catalogues.
They will typically be codified in a
@deftech{Document Type Definition} (DTD),
which is a formal, machine-checkable specification for the structure
of an XML document. Many projects in the humanities (including ours)
use @tech{Document Type Definitions} based on the TEI model, which is
described below. An XML document may refer to the DTD on which it is
based using a @deftech{DOCTYPE declaration}: in our case,
@tt{<!DOCTYPE TEI SYSTEM "DR-TEI.dtd">}.

@subsection{Further Reading}

Many systematic introductions to XML for beginners are available
freely online, such as the
@hyperlink["https://www.w3schools.com/Xml/"]{XML Tutorial} from
the website "W3 Schools". In fact, many of these tutorials
cover far more detail about XML than is
necessary to contribute to this project.

The W3C publishes a page called
@hyperlink["https://www.w3.org/standards/xml/core"]{XML Essentials}.

@section{TEI}

As discussed above, the XML standard itself does not specify
what @tech{elements} exist, the semantic meanings of
particular @tech{elements},
or how the hierarchy of @tech{elements} and textual data should
be structured in a document. The Text Encoding Initiative
consortium (TEI) publishes a standard (also referred to as TEI)
based on XML suitable for many projects in the humanities.
This standard is described at @url["http://www.tei-c.org"].

The TEI standard is what tells us, for example, that the @tag{p}
@tech{element} means "this is a paragraph", as well as specifying
the structure for the catalog information in the @tag{teiHeader}
@tech{element}.

However, because the TEI standard aims to define @tech{elements}
to meet the needs of many diverse projects (from original poetry
to facsimiles of manuscripts), projects must define smaller, more
targeted @tech{Document Type Definitions} that address their precise
use-cases.
The TEI consortium provides a variety of tools to
define such customizations with relative ease.

Digital Ricœur's specific customization of the TEI standard is
known as @tt{DR-TEI.dtd}. It comes with documentation automatically
generaterd by the TEI consortium's tools, which is available at
@url{https://manuals.digitalricoeur.org/DR-TEI_doc.html}.
We also impose additional requirements on our TEI documents that are
not easily specified using a custom DTD: these requirements
are specified in this manual and are checked by the tools
described under @secref{Tools}.

