#lang scribble/lp2

@title[#:version ""]{Formal Specification}
@(require (for-label racket/base))
@(provide attr tag)

Digital Ricœur imposes requirements for the structure of TEI
documents that are more stringent than merely being well-formed XML
that is valid with respect to the @tt{DR-TEI.dtd} Document Type
Declaration.
The rest of this manual has introduced the structure of our documents
and these project-specific requirements in an informal tutorial style.
This section specifies these requirements in a precise, succinct
form suitable for reference once you understand the basics.

This specification is written in a "literate programming" style,
where the prose specifications of elements
are intermixed with the Racket code that enforces them,
helping to ensure consistency between the two.
For Racket purposes, the code in this document is combined
into a module as follows:

@chunk[<*>
       (require racket/unit
                racket/contract
                ricoeur/tei/signatures)

       (provide element-contracts@)

       (define-unit element-contracts@
         (import tei-xexpr-contracts^)
         (export element-contracts^)
         <root-element>
         <header>
         <title-statement>
         <publication-statement>
         <source-description>
         <text-element>
         <structural-elements>
         <content-containing-elements>)]

@section{Document Structure}

The document should begin with an XML declaration and DOCTYPE
declaration, which must be exactly as follows:
@(nested #:style 'inset
         (verbatim
          @tt{<?xml version="1.0" encoding="utf-8"?>}"\n"
          @tt{<!DOCTYPE TEI SYSTEM "DR-TEI.dtd">}))

@(define (deftag tag-str)
   (elemtag tag-str (litchar tag-str)))
@(define (tag tag-str)
   (elemref tag-str (litchar tag-str)))
@(define attr 
   tt)

The root element is a @deftag{TEI} element,
which contains exactly (in order)
@tag{teiHeader} and @tag{text} elements.
It must have the attributes
@tt{version="5.0"} and
@tt{xmlns="http://www.tei-c.org/ns/1.0"}.

@chunk[<root-element>
       (define TEI/c
         (make-element-contract
          'TEI
          #:children `([1 teiHeader]
                       [1 text])
          #:required-order `(teiHeader text)
          #:attr-contracts `([version "5.0"]
                             [xmlns "http://www.tei-c.org/ns/1.0"])
          #:required-attrs `(version xmlns)))]

@subsection{The @tag{teiHeader} Element}

The @deftag{teiHeader} element contains only
the @tag{fileDesc} element.

The @deftag{fileDesc} element contains exactly (in order)
the @tag{titleStmt}, @tag{publicationStmt}, and
@tag{sourceDesc} elements.


@chunk[<header>
       (define teiHeader/c
         (make-element-contract
          'teiHeader
          #:children `([1 fileDesc])))

       (define fileDesc/c
         (make-element-contract
          'fileDesc
          #:children `([1 titleStmt]
                       [1 publicationStmt]
                       [1 sourceDesc])
          #:required-order `(titleStmt publicationStmt sourceDesc)))]

@subsubsection{The Title Statement}

The @deftag{titleStmt} contains
one or more @tag{title} elements,
one or more @tag{author} elements, and
zero or more @tag{editor} elements.
These may be intermixed freeley any order, though the order of repeated
elements indicates first author vs. second author,
main title vs. subtitle, etc.

The @deftag{title} element contains free-form text.

The @deftag{author} element contains free-form text and may have
an optional @attr{xml:id} attribute. As a special case,
the ID @racket["ricoeur"] is reserved for use with Paul Ricœur across all
documents.

@margin-note{If a type of editor arrises that does not
 fit neatly into these categories, we should decide on a standared
 value for the @tt{role} attribute and ammend this document.}
The @deftag{editor} element contains free-form text and
has optional @attr{role} and @attr{xml:id} attributes.
If the @attr{role} eattribute is present,
its value must be either @racket["editor"], @racket["translator"],
@racket["compiler"], or @racket["preface"] (to indicate the author of
a preface). Ommiting the @attr{role} attribute is equivalent to a value
of @racket["editor"].

@chunk[<title-statement>
       (define titleStmt/c
         (make-element-contract
          'titleStmt
          #:children `([1+ title]
                       [1+ author]
                       [0+ editor])))

       (define title/c
         (make-element-contract 'title #:text? #t))

       (define author/c
         (make-element-contract 'author #:text? #t))

       (define editor/c
         (make-element-contract
          'editor
          #:text? #t
          #:attr-contracts `([role ,(or/c "editor"
                                          "translator"
                                          "compiler"
                                          "preface")])))]

@subsubsection{The Publication Statement}
The @deftag{publicationStmt} should be exactly as follows:
@(nested #:style 'inset
         (verbatim
          @tt{<publicationStmt>}"\n"
          "  "@tt{<authority>Digital Ricoeur</authority>}"\n"
          "  "@tt{<availability status="restricted">}"\n"
          "    "@tt{<p>Not for distribution.</p>}"\n"
          "  "@tt{</availability>}"\n"
          @tt{</publicationStmt>}))

@chunk[<publication-statement>
       (define publicationStmt/c
         (make-element-contract
          'publicationStmt
          #:children '([1 authority]
                       [1 availability])))

       (define authority/c
         (make-element-contract 'authority #:text? #t))

       (define availability/c
         (make-element-contract
          'availability
          #:children '([1 p])
          #:attr-contracts `([status "restricted"])
          #:required-attrs '(status)))]


@subsubsection{The Source Description}
The @deftag{sourceDesc} element must contain exactly one
@tag{bibl} element.

The @deftag{bibl} element contains free-form text and
zero or one @tag{date} elements, which should be
included unless the date of publication is unknown.

The @deftag{date} element contains free-form text
representing the human-readable publication date.
It must have a @attr{when} attribute giving the
date in machine-readable format, which must be a subset of
the format required by TEI: @tt{YYYY},
@tt{YYYY-MM}, or @tt{YYYY-MM-DD}, where the month
and day, if present, must allways be two digits
(e.g. @tt{01} for January).

@chunk[<source-description>
       (define sourceDesc/c
         (make-element-contract
          'sourceDesc
          #:children '([1 bibl])))

       (define bibl/c
         (make-element-contract
          'bibl
          #:text? #t
          #:children '([0-1 date])))

       (define date/c
         (make-element-contract
          'date
          #:text? #t
          #:attr-contracts `([when #px"^(\\d\\d\\d\\d)(-\\d\\d)?(-\\d\\d)?$"])
          #:required-attrs '(when)))]

@subsection{The @tag{text} Element}

The @deftag{text} element may contain only (in order) 
a @tag{front} element, a @tag{body} element,
and a @tag{back} element,
but the @tag{front} and @tag{back} elements are optional.

The @deftag{body}, @deftag{front}, and @deftag{back} elements
may contain @tag{head}, 
@tag{p}, @tag{pb}, @tag{ab}, and @tag{div} elements.

@chunk[<text-element>
       (define text/c
         (make-element-contract
          'text
          #:children `([0-1 front]
                       [1 body]
                       [0-1 back])
          #:required-order `(front body back)))

       (define body/c
         (make-element-contract
          'body
          #:children `([0+ head]
                       [0+ p]
                       [0+ pb]
                       [0+ ab]
                       [0+ div])))

       (define front/c
         (make-element-contract
          'front
          #:children `([0+ head]
                       [0+ p]
                       [0+ pb]
                       [0+ ab]
                       [0+ div])))

       (define back/c
         (make-element-contract
          'back
          #:children `([0+ head]
                       [0+ p]
                       [0+ pb]
                       [0+ ab]
                       [0+ div])))]

@subsubsection{Structural Elements}

@margin-note{If a type of division arrises that does not
 fit neatly into these categories, we should decide on a standared
 value for the @tt{type} attribute and ammend this document.}
The @deftag{div} element may contain
@tag{head}, @tag{list}, @tag{p},
@tag{pb}, @tag{ab}, @tag{sp}, or nested @tag{div} elements.
It must have a @attr{type} attribute with a value of
@racket["chapter"], @racket["part"], @racket["section"],
@racket["dedication"], @racket["contents"], @racket["intro"],
@racket["ack"] (for acknowledgements), or @racket["index"].
If the division is numbered, it should have an
@attr{n} attribute giving the page number
as given in the source (i.e. possibly as a Roman numeral).

The @deftag{pb} element has no contents.
Unless the corresponding page is not numbered in the source,
it should have an @attr{n} attribute giving the page number
as given in the source (i.e. possibly as a Roman numeral).

The @deftag{list} element may contain
the @tag{head}, @tag{item}, and @tag{pb} elements.
It may have a @attr{rend} attribute with a value of either
@racket["numbered"] or @racket["bulleted"] (the default).

The @deftag{sp} ("speech") element must have a valid @attr{who}
attribute and may contain @tag{list}, @tag{p}, @tag{pb}, or @tag{ab}
elements.

@chunk[<structural-elements>
       (define div/c
         (make-element-contract
          'div
          #:children `([0+ head]
                       [0+ list]
                       [0+ p]
                       [0+ pb]
                       [0+ ab]
                       [0+ sp]
                       [0+ div])
          #:attr-contracts
          `([type ,(or/c "chapter"
                         "part"
                         "section"
                         "dedication"
                         "contents"
                         "ack"
                         "intro"
                         "index")])
          #:required-attrs '(type)))
       
       (define pb/c
         (make-element-contract 'pb))

       (define tei:list/c
         (make-element-contract
          'list
          #:children `([0+ head]
                       [0+ item]
                       [0+ pb])
          #:attr-contracts
          `([type ,(or/c "numbered" "bulleted")])))

       (define sp/c
         (make-element-contract
          'sp
          #:children `([0+ list]
                       [0+ p]
                       [0+ pb]
                       [0+ ab])
          #:required-attrs '(who)))]

@subsubsection{Content-Containing Elements}

The @deftag{ab} ("anonymous block")
element may contain a combination of
free-form text and @tag{list}, @tag{note},
and @tag{pb} elements.
It is used for blocks of text
that have not yet been divided into more semantically
meaningful elements.

The @deftag{p} ("paragraph")
element may contain a combination of
free-form text and @tag{list}, @tag{note},
and @tag{pb} elements.

The @deftag{head} ("heading")
element may contain a combination of
free-form text and @tag{note} elements.

The @deftag{note} element
may contain a combination of free-form text
and @tag{list}, @tag{note}, @tag{p}, and
@tag{ab} elements.
It must have a @tt{place} attribute with
a value of either @racket["foot"] or
@racket["end"].
It may have an @attr{n} attribute giving the
the number or symbol for the footnote or endnote.
If @attr{n} attributes are ommited for a document,
numbering will be assumed to be sequential.
Translation notes should have a @attr{type} attribute
with a value of @racket["transl"].

The @deftag{item} element may contain
a combination of free-form text and
@tag{list}, @tag{note}, @tag{p}, @tag{pb}, and
@tag{ab} elements.

@chunk[<content-containing-elements>
       (define ab/c
         (make-element-contract
          'ab
          #:text? #t
          #:children `([0+ list]
                       [0+ note]
                       [0+ pb])))

       (define p/c
         (make-element-contract
          'p
          #:text? #t
          #:children `([0+ list]
                       [0+ note]
                       [0+ pb])))

       (define head/c
         (make-element-contract
          'head
          #:text? #t
          #:children `([0+ note])))

       (define note/c
         (make-element-contract
          'note
          #:text? #t
          #:children `([0+ list]
                       [0+ note]
                       [0+ p]
                       [0+ ab])
          #:attr-contracts `([place ,(or/c "foot" "end")]
                             [type "transl"])
          #:required-attrs `(place)))

       (define item/c
         (make-element-contract
          'item
          #:text? #t
          #:children `([0+ list]
                       [0+ note]
                       [0+ p]
                       [0+ pb]
                       [0+ ab])))]

