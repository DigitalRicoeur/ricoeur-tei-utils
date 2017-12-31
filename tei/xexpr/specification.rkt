#lang scribble/lp2

@title[#:version ""]{Formal Specification}
@(require (for-label racket/base)
          scribble/core)
@(provide attr tag)

Digital Ricœur imposes requirements for the structure of TEI
documents that are more stringent than merely being well-formed XML
that is valid with respect to the @tt{DR-TEI.dtd} Document Type
Definition.
The rest of this manual has introduced the structure of our documents
and these project-specific requirements in an informal tutorial style.
This section specifies these requirements in a precise, succinct
form suitable for reference once you understand the basics.

This section is written in a "literate programming" style:
prose specifications of the rules for TEI elements
are intermixed with the Racket code that enforces those rules,
helping to ensure that the rules and the enforcement programs
remain consistent.
Readers focused on the preparation of TEI XML documents who are
not concerned with the implementation of Racket tools may simply
ignore the Racket code.

For Racket purposes, the code in this document is combined
into a module as follows:

@chunk[$*$
       (require racket/unit
                racket/contract
                racket/match
                ricoeur/tei/xexpr/signatures)

       (provide element-contracts@)

       (define-unit element-contracts@
         (import tei-xexpr-contracts^)
         (export element-contracts^)
         $root-element$
         $header$
         $title-statement$
         $publication-statement$
         $source-description$
         $textClass$
         $text-element$
         $structural-elements$
         $content-containing-elements$)]

@section{Document Structure}

The document should begin with an XML declaration and DOCTYPE
declaration, which must be exactly as follows:
@(nested #:style 'inset
         (verbatim
          @tt{<?xml version="1.0" encoding="utf-8"?>}"\n"
          @tt{<!DOCTYPE TEI SYSTEM "DR-TEI.dtd">}))

@(define (deftag tag-str)
   (toc-target2-element #f (litchar tag-str) (list 'tei tag-str) (tt tag-str)))
@(define (tag tag-str)
   (link-element #f (litchar tag-str) (list 'tei tag-str)))
@(define attr 
   tt)

The root element is a @deftag{TEI} element,
which contains exactly (in order)
@tag{teiHeader} and @tag{text} elements.
It must have the attributes
@tt{version="5.0"} and
@tt{xmlns="http://www.tei-c.org/ns/1.0"}.

@chunk[$root-element$
       (define TEI/c
         (make-element-contract
          'TEI
          #:children `([1 teiHeader]
                       [1 text])
          #:required-order `(teiHeader text)
          #:attr-contracts `([version "5.0"]
                             [xmlns "http://www.tei-c.org/ns/1.0"])
          #:required-attrs `(version xmlns)))]

@section{The teiHeader Element}

The @deftag{teiHeader} element contains one
@tag{fileDesc} element followed by one
@tag{profileDesc} element.

The @deftag{fileDesc} element contains exactly (in order)
the @tag{titleStmt}, @tag{publicationStmt}, and
@tag{sourceDesc} elements.

The @deftag{profileDesc} element contains only the
@tag{textClass} element.

@margin-note{
 Currently, to facilitate transition, documents missing the
 @tag{profileDesc} element will temporarily be accepted.}

@chunk[$header$
       (define teiHeader/c
         (make-element-contract
          'teiHeader
          #:required-order '(fileDesc profileDesc)
          #:children `([1 fileDesc]
                       (code:comment
                        @#,elem{Soon, exactly 1 @tag{profileDesc} will be required.})
                       [0-1 profileDesc])))

       (define fileDesc/c
         (make-element-contract
          'fileDesc
          #:children `([1 titleStmt]
                       [1 publicationStmt]
                       [1 sourceDesc])
          #:required-order `(titleStmt publicationStmt sourceDesc)))

       (define profileDesc/c
         (make-element-contract
          'profileDesc
          #:children `([1 textClass])))]

@subsection{The Title Statement}

The @deftag{titleStmt} contains
one or more @tag{title} elements,
one or more @tag{author} elements, and
zero or more @tag{editor} elements.
These may be intermixed freely any order, though the order of repeated
elements indicates first author vs. second author,
main title vs. subtitle, etc.

The @deftag{title} element contains free-form text.

The @deftag{author} element contains free-form text and may have
an optional @attr{xml:id} attribute. As a special case,
the ID @racket["ricoeur"] is reserved for use with Paul Ricœur across all
documents.

The @deftag{editor} element contains free-form text and
has optional @attr{role} and @attr{xml:id} attributes.
If the @attr{role} attribute is present,
its value must be either @racket["editor"], @racket["translator"],
@racket["compiler"], or @racket["preface"] (to indicate the author of
a preface). Ommiting the @attr{role} attribute is equivalent to a value
of @racket["editor"].
@margin-note{If a type of editor arises that does not
 fit neatly into these categories, we should decide on a standard
 value for the @tt{role} attribute and amend this document.}

@chunk[$title-statement$
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

@subsection{The Publication Statement}
The @deftag{publicationStmt}, which contains the @deftag{authority}
and @deftag{availability} elements, should be exactly as follows:
@(nested #:style 'inset
         (verbatim
          @tt{<publicationStmt>}"\n"
          "  "@tt{<authority>Digital Ricoeur</authority>}"\n"
          "  "@tt{<availability status="restricted">}"\n"
          "    "@tt{<p>Not for distribution.</p>}"\n"
          "  "@tt{</availability>}"\n"
          @tt{</publicationStmt>}))

@chunk[$publication-statement$
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


@subsection{The Source Description}
The @deftag{sourceDesc} element must contain exactly one
@tag{bibl} element.

The @deftag{bibl} element contains free-form text and
one or two @tag{date} elements: either one with
a @attr{subtype} attribute of @racket["thisIsOriginal"]
or both one with a @attr{subtype} attribute of @racket["this"]
and one with a @attr{subtype} attribute of @racket["original"].

The @deftag{date} element contains free-form text
representing the human-readable publication date.
It must have:
@itemlist[
 @item{a @attr{type} attribute with a value of @racket["publication"];}
 @item{a @attr{subtype} attribute with a value of @racket["this"],
  @racket["original"], or @racket["thisIsOriginal"]; and}
 @item{a @attr{when} attribute giving the
  date in machine-readable format, which must be a subset of
  the format required by TEI: @racket["YYYY-MM-DD"], @racket["YYYY-MM"],
  or @racket["YYYY"], where the month
  and day, if present, must allways be two digits
  (e.g. @tt{01} for January).}]

@chunk[$source-description$
       (define sourceDesc/c
         (make-element-contract
          'sourceDesc
          #:children '([1 bibl])))

       (define bibl/c
         (make-element-contract
          'bibl
          #:text? #t
          #:children '([1+ date])
          #:extra-check
          (λ (val maybe-blame neg-party)
            (match (for/list ([c (in-list (get-body val))]
                              #:when (and (list? c)
                                          (eq? 'date (car c))))
                     (cadr (assq 'subtype (get-attributes c))))
              [(or (list-no-order "this" "original")
                   (list "thisIsOriginal"))
               #t]
              [bad-subtypes
               (and maybe-blame
                    (raise-blame-error 
                     maybe-blame #:missing-party neg-party
                     val
                     '("invalid combination of date element subtypes inside bibl element"
                       expected: "'(\"this\" \"original\") '(\"original\" \"this\") or '(\"thisIsOriginal\")"
                       given: "~e"
                       "\n  bibl element...:\n   ~e")
                     bad-subtypes
                     val))]))))

       (define date/c
         (make-element-contract
          'date
          #:text? #t
          #:attr-contracts `([when #px"^(\\d\\d\\d\\d)(-\\d\\d)?(-\\d\\d)?$"]
                             [type "publication"]
                             [subtype ,(or/c "this" "original" "thisIsOriginal")])
          #:required-attrs '(when type subtype)))]


@subsection{The Text Classification}

The @deftag{textClass} element must contain
one @tag{catRef} element and one @tag{keywords} element.

The @deftag{catRef} element contains nothing.
It has two attributes, @attr{scheme} and @attr{target},
both of which are required.
This element encodes whether
the document is a book or an article.
The @attr{scheme} attribute must have the value
@racket["https://schema.digitalricoeur.org/taxonomy/type"].
The value for the @attr{target} attribute must be either:
@itemlist[
 @item{@racket["https://schema.digitalricoeur.org/taxonomy/type#article"],
  if the document is an article; or
 }
 @item{@racket["https://schema.digitalricoeur.org/taxonomy/type#book"],
  if the document is a book.
  }]

The @deftag{keywords} element is currently used to encode flags
for the @tt{guess-paragraphs} tool.
It must have a @attr{scheme} attribute with a value of
@racket["https://schema.digitalricoeur.org/tools/tei-guess-paragraphs"].
The @tag{keywords} element must contain exactly one @tag{term} element.

The @deftag{term} element contains immediate text, but its
contents must conform to the vocabulary prescribed by the @attr{scheme}
attribute of its parent @tag{keywords} element.
Currently, its contents must be exactly one of the following:
@litchar{todo}; @litchar{line-breaks}; @litchar{blank-lines}; @litchar{done};
or @litchar{skip}.
Only the @litchar{todo} value should be entered manually.

@chunk[$textClass$
       (define textClass/c
         (make-element-contract
          'textClass
          #:children '([1 catRef]
                       [1 keywords])))

       (define catRef/c
         (make-element-contract
          'catRef
          #:attr-contracts
          `([scheme "https://schema.digitalricoeur.org/taxonomy/type"]
            [target ,(or/c "https://schema.digitalricoeur.org/taxonomy/type#article"
                           "https://schema.digitalricoeur.org/taxonomy/type#book")])
          #:required-attrs '(scheme target)))

       @(define keywords/c
          (make-element-contract
           'keywords
           #:attr-contracts
           `([scheme "https://schema.digitalricoeur.org/tools/tei-guess-paragraphs"])
           #:required-attrs '(scheme)
           #:children '([1 term])))

       (define term/c
         (make-element-contract
          'term
          #:text? #t
          #:extra-check
          (λ (val maybe-blame neg-party)
            (code:comment @#,elem{We filter to allow XML comments.})
            (match (filter string? (get-body val))
              [(list (or "todo"
                         "line-breaks"
                         "blank-lines"
                         "done"
                         "skip"))
               #t]
              [bad
               (and maybe-blame
                    (raise-blame-error
                     maybe-blame #:missing-party neg-party
                     val
                     '("invalid contents of term element"
                       expected: "(list/c (or/c \"todo\" \"line-breaks\" \"blank-lines\" \"done\" \"skip\"))"
                       given: "~e"
                       "\n  term element...:\n   ~e")
                     bad
                     val))]))))]



@section{The text Element}

The @deftag{text} element may contain only (in order) 
a @tag{front} element, a @tag{body} element,
and a @tag{back} element,
but the @tag{front} and @tag{back} elements are optional.

The @deftag{body}, @deftag{front}, and @deftag{back} elements
may contain @tag{head}, 
@tag{p}, @tag{pb}, @tag{ab}, and @tag{div} elements.

@chunk[$text-element$
       (define text/c
         (make-element-contract
          'text
          #:children `([0-1 front]
                       [1 body]
                       [0-1 back])
          #:required-order `(front body back)))

       (define children:body+front+back
         `([0+ head]
           [0+ p]
           [0+ pb]
           [0+ ab]
           [0+ div]))
       
       (define body/c
         (make-element-contract
          'body
          #:children children:body+front+back))

       (define front/c
         (make-element-contract
          'front
          #:children children:body+front+back))

       (define back/c
         (make-element-contract
          'back
          #:children children:body+front+back))]

@subsection{Structural Elements}

The @deftag{div} element may contain
@tag{head}, @tag{list}, @tag{p},
@tag{pb}, @tag{ab}, @tag{sp}, or nested @tag{div} elements.
It must have a @attr{type} attribute with a value of
@racket["chapter"], @racket["part"], @racket["section"],
@racket["dedication"], @racket["contents"], @racket["intro"],
@racket["bibl"] (for a bibliography),
@racket["ack"] (for acknowledgements), or @racket["index"].
If the division is numbered, it should have an
@attr{n} attribute giving the page number
as given in the source (i.e. possibly as a Roman numeral).
Any @tag{div} element may, and a @tag{div} representing a
section not by Paul Ricœur must, have a @attr{resp} attribute
that is valid per the TEI standard, as discussed above under
@secref["Sections_Not_by_Ric_ur"].
@margin-note{If a type of division arises that does not
 fit neatly into these categories, we should decide on a standard
 value for the @tt{type} attribute and amend this document.}

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

@chunk[$structural-elements$
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
                         "bibl"
                         "ack"
                         "intro"
                         "index")]
            [resp #rx"#.+"])
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
          #:attr-contracts `([who #rx"#.+"])
          #:required-attrs '(who)))]

@subsection{Content-Containing Elements}

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
It must also have an @attr{n} attribute giving the
the number or symbol for the footnote or endnote.
Translation notes should have a @attr{type} attribute
with a value of @racket["transl"].
Any @tag{note} element may, and a @tag{note} representing a
note not by Paul Ricœur must, have a @attr{resp} attribute
that is valid per the TEI standard, as discussed above under
@secref["Sections_Not_by_Ric_ur"].

The @deftag{item} element may contain
a combination of free-form text and
@tag{list}, @tag{note}, @tag{p}, @tag{pb}, and
@tag{ab} elements.

@chunk[$content-containing-elements$
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
                             [resp #rx"#.+"]
                             [type "transl"])
          #:required-attrs `(place n)))

       (define item/c
         (make-element-contract
          'item
          #:text? #t
          #:children `([0+ list]
                       [0+ note]
                       [0+ p]
                       [0+ pb]
                       [0+ ab])))]

