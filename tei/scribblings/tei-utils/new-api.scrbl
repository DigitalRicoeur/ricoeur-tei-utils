#lang scribble/manual

@title[#:style '(toc)]{Work-In-Progress API}
@;; ricoeur/tei/kernel/sans-lang
@;;   to support ricoeur/tei/kernel/lang/specification-lang
@(declare-exporting ricoeur/tei/kernel
                    ricoeur/tei/base
                    ricoeur/tei
                    #:use-sources (ricoeur/tei/kernel/sans-lang)
                    )


@(require "for-manual.rkt"
          (for-label ricoeur/tei/kernel/sans-lang
                     ))

@(local-table-of-contents)

@section{TEI Element Representation}
@deftogether[
 (@defpredicate[tei-element?]
   @defpredicate[content-containing-element?]
   @defpredicate[elements-only-element?]
   @defproc[(tei-element-get-name [e tei-element?])
            tei-element-name/c]
   @defproc[(tei-element-get-attributes [e tei-element?])
            (listof (list/c symbol? string?))]
   @defproc[(tei-element-get-body [e tei-element?])
            (listof (or/c tei-element? normalized-xexpr-atom/c))]
   @defproc[(tei-get-body/elements-only [e elements-only-element?])
            (listof tei-element?)]
   @defproc[(tei-element-to-xexpr [e tei-element?])
            (and/c any-tei-xexpr/c normalized-xexpr-element/c)])]{

 This library uses @deftech{tei element structs}, a layer of
 abstraction over @tech{normalized x-expressions},
 to build higher-level interfaces to TEI XML documents.
 All @tech{tei element structs} are recognized by the predicate
 @racket[tei-element?].

 Internally, there is a distinct @deftech{tei element struct type}
 for each type of element in Digital @|Ricoeur|'s customized TEI schema
 (see @guidelines-secref["Formal_Specification"]
 in @(guidelines-doc) for a complete listing).
 However, the specific representations of many
 @tech{tei element struct types} are kept private to this library:
 for robustness against future changes to Digital @|Ricoeur|'s TEI schema,
 clients are encouraged to use high-level interfaces that abstract
 over the details of the document structure.

 All @tech{tei element struct types} support the common set of
 operations listed above for traversing an instance's attributes and
 contents; however, these functions are fairly low-level and
 should primarily be used to implement higher-level abstractions,
 ordinarily as part of this library.

 Any @tech{tei element struct} may be converted to a
 @tech{normalized x-expression} using @racket[tei-element-to-xexpr].
 The XML representation is the cannonical serialized form:
 @tech{tei element structs} are not serializable in the sense of
 @racketmodname[racket/serialize].

 Every @tech{tei element struct} satisfies either
 @racket[content-containing-element?] or @racket[elements-only-element?]
 (but not both) depending on whether the element type of which
 it is an instance may ever contain textual data directly.
 For a @tech{tei element struct} that satisfies
 @racket[elements-only-element?], the list returned by
 @racket[tei-element-get-body] will never contain any strings:
 any insignificant whitespace inside such elements is 
 dropped when the @tech{tei element struct} is constructed.
 
}



@section{TEI Info Values}
@defpredicate[TEI-info?]{

 A @deftech{TEI info} value encapsulates some high-level
 information about a TEI XML document following
 Digital @|Ricoeur|'s schema.
 (It does not retain necessarily retain the actual content of
 the document.)
 The predicate @racket[TEI-info?] recognizes @tech{TEI info} values.

 All @tech{TEI info} values support the same high-level API,
 and new kinds of values (including class-based objects)
 can implement the @tech{TEI info} interface.
 
}

@defproc[(get-plain-TEI-info [info TEI-info?])
         plain-TEI-info?]{
 Converts any @tech{TEI info} value to a @deftech{plain TEI info} value,
 which serves as a cannonical form of the encapsulated information.
}

@TODO/scrbl[[TEI info: implement & document the actual methods]]

@subsection{Derived TEI Info}
@defthing[prop:TEI-info (struct-type-property/c
                         (-> any/c plain-TEI-info?))]{
 New kinds of values can support the @tech{TEI info} API 
 through the structure type property @racket[prop:TEI-info].
 The value for the @racket[prop:TEI-info] must be a function
 that, given an instance of the new structure type,
 returns a @tech{plain TEI info} value.
 The function should always return the same
 @tech{plain TEI info} value when called with the same argument:
 this requirement is not currently enforced, but may be
 in the future.

 Special support is provided for using class-based objects
 as @tech{TEI info} values via @racket[TEI-info-mixin].
}

@defpredicate[plain-TEI-info?]{
 Recognizes @tech{plain TEI info} values.
}


@subsubsection{Class-based Objects}
@defmixin[TEI-info-mixin () (TEI-info<%>)]{

 An instance of a class extended with @racket[TEI-info-mixin]
 can be used as a @tech{TEI info} value.
 This is the prefered way for class-based objects
 to support the @tech{TEI info} API.

 The resulting class will implement the @racket[TEI-info<%>]
 interface, which also makes some of the @tech{TEI info}
 functions available as methods for use with @racket[inherit],
 @racket[send], etc.
                                           
 @defconstructor/auto-super[([TEI-info TEI-info?])]{                  

  The @racket[TEI-info] initialization argument determines
  the behavior of @(this-obj) when used as a @tech{TEI info} value.
  
  Specifically, @racket[(get-plain-TEI-info #,(this-obj))]
  will return the same @tech{plain TEI info} value as
  @racket[(get-plain-TEI-info TEI-info)].
                                                    
}}

@definterface[TEI-info<%> ()]{

 An interface identifying classes which have been extended
 with @racket[TEI-info-mixin].
 Objects which implement @racket[TEI-info<%>] can be used
 as @tech{TEI info} values.

 In addition to the methods documented below,
 @racket[TEI-info<%>] also includes additional, private methods:
 @racket[TEI-info<%>] can only be implemented
 using @racket[TEI-info-mixin].

 
                              
}



@section{Base}
@;include-section["new-api/base-constructors.scrbl"]
@(declare-exporting ricour/tei/base
                    ricour/tei
                    #:use-sources (ricoeur/tei/base) ;don't understand why
                    )

@defproc[(file->TEI [file (and/c path-string? file-exists?)])
         TEI?]{
 Produces a @tech{tei element struct} representing
 the TEI XML document @racket[file].
}

@defproc[(read-TEI [in input-port? (current-input-port)])
         TEI?]{
 Produces a @tech{tei element struct} representing the TEI XML
 document read from @racket[in].
}

@defproc[(xexpr->element [xs any-tei-xexpr/c])
         tei-element?]{
 The primitive function for converting a @tech{raw xexpr}
 representation of a TEI XML element to a @tech{tei element struct}.
}

@defpredicate[TEI?]{
 Recognizes @tech{tei element structs} that represent
 the root @tag{TEI} element of a document.
}









@include-section["implementation.scrbl"]




