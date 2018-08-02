#lang scribble/manual

@title{TEI Element Representation}
@(declare-exporting ricoeur/tei/base
                    ricoeur/tei)

@(require "for-manual.rkt")

This section documents the representations used by this library
for individual TEI elements.
TEI elements are translated from native XML form into
@tech{raw x-expressions} that satisfy @racket[any-tei-xexpr/c].
Internally, this library uses @tech{TEI element structs}
to provide a further layer of abstraction.

Many of the functions documented in this section expose
details of the current schema used by Digital @Ricoeur for
our TEI XML documents.
The details of this schema are subject to change:
indeed, a major purpose of this library is to provide clients
with an API that remains stable across changes to the schema.
Programmers are strongly advised to use higher-level abstractions
instead of the low-level operations documented in this section
whenever possible.






@section{TEI X-Expression Contracts}
@defthing[any-tei-xexpr/c flat-contract?]{
 Similar to @racket[raw-xexpr-element/c], but
 rejects some (not all) @tech{raw x-expressions} that would break TEI
 validity rules, including the additional requirements
 imposed by Digital Ricœur.
}

@defform[(tei-xexpr/c elem-name-id)]{
 Produces a contract similar to @racket[any-tei-xexpr/c], but
 which recognizes only elements named
 @racket[(#,(racket quote) elem-name-id)].

 Using @racket[(tei-xexpr/c elem-name-id)] produces the same contract as
 @racket[(dynamic-tei-xexpr/c (#,(racket quote) elem-name-id))],
 but @racket[tei-xexpr/c] expands to the specific contract at compile-time,
 and a syntax error is raised if @racket[(#,(racket quote) elem-name-id)]
 would not satisfy @racket[tei-element-name/c].
}

@defproc[(dynamic-tei-xexpr/c [name tei-element-name/c])
         flat-contract?]{
 Like @racket[tei-xexpr/c], but dispatches to the specific contract
 dynamically at run-time.
}

@defthing[tei-element-name/c flat-contract?]{
 A contract recognizing the names of valid
 Digital Ricœur TEI XML elements.
 All values which satisfy @racket[tei-element-name/c]
 also satisfy @racket[symbol?].
}







@section{Common Element Interface}

@deftogether[
 (@defpredicate[tei-element?]
   @defpredicate[content-containing-element?]
   @defpredicate[elements-only-element?]
   )]{
 This library uses @deftech{tei element structs}, a layer of
 abstraction over @tech{normalized x-expressions},
 to build higher-level interfaces to TEI XML documents.
 All @tech{tei element structs} are recognized by the predicate
 @racket[tei-element?].

 Internally, there is a distinct @deftech{tei element struct type}
 for each type of element in Digital @|Ricoeur|'s customized TEI schema
 (see @guidelines-secref["Formal_Specification"]
 in @(guidelines-doc) for a complete listing).
 However, the specific representations of most
 @tech{tei element struct types} are kept private to this library:
 for robustness against future changes to Digital @|Ricoeur|'s TEI schema,
 clients are urged to use high-level interfaces that abstract
 over the details of the document structure.

 Every @tech{tei element struct} satisfies either
 @racket[content-containing-element?] or @racket[elements-only-element?]
 (but not both) depending on whether the element type of which
 it is an instance may ever contain textual data directly.
}

@subsection{Traversing TEI Element Structs}
@margin-note{
 The accessors documented in this subsection are especially
 likely to expose brittle details that will break
 upon changes to Digital @|Ricoeur|'s schema for
 our TEI XML documents.
}
@deftogether[
 (@defproc[(tei-element-get-name [e tei-element?])
           tei-element-name/c]
   @defproc[(tei-element-get-attributes [e tei-element?])
            (listof (list/c symbol? string-immutable/c))]
   @defproc[(tei-element-get-body [e tei-element?])
            (listof (or/c tei-element? normalized-xexpr-atom/c))]
   @defproc[(tei-get-body/elements-only [e elements-only-element?])
            (listof tei-element?)]
   )]{
 All @tech{tei element struct types} support the common set of
 operations listed above for traversing an instance's attributes and
 contents; however, these functions are fairly low-level and
 should primarily be used to implement higher-level abstractions,
 ordinarily as part of this library.

 For a @tech{tei element struct} that satisfies
 @racket[elements-only-element?], the list returned by
 @racket[tei-element-get-body] will never contain any strings:
 any insignificant whitespace inside such elements is 
 dropped when the @tech{tei element struct} is constructed.
 However, the result of @racket[tei-element-get-body] may
 still be different from @racket[tei-get-body/elements-only],
 as the list returned by @racket[tei-element-get-body]
 may contain values satisfying @racket[normalized-comment/c]
 or @racket[normalized-p-i/c].
}

@deftogether[
 (@defform[#:kind "match expander"
           (tei-element name-pat attributes-pat body-pat)]
   @defform[#:kind "match expander"
            (content-containing-element name-pat attributes-pat body-pat)]
   @defform[#:kind "match expander"
            (elements-only-element name-pat
                                   attributes-pat
                                   body-pat
                                   maybe-elements-only)
            #:grammar
            [(maybe-elements-only code:blank
                                  (code:line #:elements-only body/elements-only-pat))]]
   )]{
 For the match expanders @racket[tei-element],
 @racket[content-containing-element], and @racket[elements-only-element],
 the patterns @racket[name-pat], @racket[attributes-pat], and
 @racket[body-pat] are matched against the results of
 @racket[tei-element-get-name], @racket[tei-element-get-attributes], and
 @racket[tei-element-get-body], respectively.
 A @racket[tei-element] pattern can match any @tech{tei element struct},
 whereas @racket[content-containing-element] and
 @racket[elements-only-element] patterns only match values that satisfy
 the corresponding predicate.
 If a @racket[body/elements-only-pat] pattern appears, it is matched
 against the result of @racket[tei-get-body/elements-only].
}




@subsection{Struct–X-Expression Conversion}
@defproc[(xexpr->element [xs any-tei-xexpr/c])
         tei-element?]{
 The primitive function for converting a @tech{raw xexpr}
 representation of a TEI XML element to a @tech{tei element struct}.
}

@defproc[(tei-element->xexpr [e tei-element?])
         (and/c any-tei-xexpr/c normalized-xexpr-element/c)]{
 Any @tech{tei element struct} may be converted to a
 @tech{normalized x-expression} using @racket[tei-element->xexpr].
 XML is the cannonical serialized form of a tei element struct:
 tei element structs are not serializable in the sense of
 @racketmodname[racket/serialize].
}

@defproc[(tei-element->xexpr* [e (or/c tei-element? normalized-xexpr-atom/c)])
         normalized-xexpr/c]{
 Like @racket[tei-element->xexpr], but also accepts @tech{normalized xexprs}
 satisfying @racket[normalized-xexpr-atom/c], which are returned directly.
}









@section{Specialized Element Interfaces}
@TODO/void[Prose saying to use segments and not these.]
@subsection{Elements with Responsible Parties}
@deftogether[
 (@defproc[(tei-element-can-have-resp? [e any/c]) any/c]
   @defproc[(tei-element-resp [e tei-element-can-have-resp?]
                              [default (or/c 'ricoeur #f) 'ricoeur])
            (if default
                symbol?
                (or/c symbol? #f))])]{
 A uniform interface for accessing the @attr{resp} attribute of
 @tag{div} and @tag{note} elements and the @attr{who} attribute of
 @tag{sp} elements.

 For implementation details, see @racket[declare-resp-field].
}


@subsection{Page-break Elements}
@defpredicate[tei-pb?]{
 Recognizes @tech{tei element structs} that represent
 @tag{pb} (page-break) elements.
}

@deftogether[
 (@defproc[(pb-get-kind [pb tei-pb?])
           (or/c 'none 'number 'roman 'other)]
   @defproc[(pb-get-numeric [pb tei-pb?])
            (maybe/c natural-number/c)]
   @defproc[(pb-get-page-string [pb tei-pb?])
            (maybe/c string-immutable/c)])]{
 This library groups page-breaks into several kinds based on
 their number, i.e. the @attr{n} attribute of the @tag{pb} element.
 The kind of number can be identified
 by the result of @racket[pb-get-kind]:
 @(itemlist
   @item{@racket['none]: The page was not numbered.}
   @item{@racket['number]: The page was numbered with an Arabic numeral.}
   @item{@racket['roman]: The page was numbered with a Roman numeral.}
   @item{@racket['other]: The page has a ``number''
  according to the @attr{n} attribute, but the @attr{n} attribute value
  is not in a format this library can understand.
  })

 When the kind is @racket['number] or @racket['roman],
 @racket[pb-get-numeric] returns a @racket[just] value containing
 the page number as a Racket integer.

 Unless the kind is @racket['none], @racket[pb-get-page-string] returns
 a @racket[just] value containing the raw string given as the @attr{n} attribute.

 Recall that a @tag{pb} element marks the beginning the specified page.
}

@defproc[(tei-get-page-breaks [elem tei-element?])
         (listof tei-pb?)]{
 Returns a list, in order, of all of the @tech{tei element structs}
 recursively contained by @racket[elem] that represent
 @tag{pb} (page-break) elements.
 If @racket[elem] itself represents a page-break element,
 the result is @racket[(list elem)].

 This function is most often used with @tech{TEI document} values.
}

@subsection{Footnote & Endnote Elements}
@defpredicate[tei-note?]{
 Recognizes @tech{tei element structs} that represent
 @tag{note} elements, which are used for footnotes and endnotes.
}

@defproc[(tei-note-get-place [note tei-note?])
         (or/c 'foot 'end)]{
 Indicates whether @racket[note] represents a footnote or an endnote. 
}

@defproc[(tei-note-get-n [note tei-note?])
         string-immutable/c]{
 Returns a string representing how @racket[note]
 was identified in the original, e.g. @racket["1"] or @racket["*"]. 
}

@defproc[(tei-note-get-transl? [note tei-note?])
         (or/c #f 'transl)]{
 Returns @racket['transl] if @racket[note] is a translation note.
}

@subsection{Chapter & Section Elements}
@defpredicate[div?]{
Recognizes @tech{tei element structs} that represent
 @tag{div} elements, which are used for chapters, sections, and
 other structural divisions.
}

@defproc[(div-get-n [elem div?])
         (maybe/c string-immutable/c)]{
 Returns either a @racket[just] value containing the
 string value of the @attr{n} attribute of the represented
 @tag{div} element,
 or @racket[(nothing)] if the @attr{n} attribute
 was not present.
}

@deftogether[
 (@defproc[(div-get-type [elem div?])
           div-type/c]
   @defthing[div-type/c flat-contract?
             #:value (or/c 'chapter 'part 'section 'dedication
                           'contents 'intro 'bibl 'ack 'index)])]{
 Returns a symbol corresponding to the @attr{type} attribute of
 the represented @tag{div} element.
 See the documentation for @tag{div} for details about the meanings
 of specific @attr{type} values.
}



