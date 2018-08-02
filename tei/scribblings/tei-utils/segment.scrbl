#lang scribble/manual

@title{Document Segments}
@(declare-exporting ricoeur/tei/base
                    ricoeur/tei)

@(require "for-manual.rkt"
          (for-label data/order
                     ))

@tech{Segments} are an approach to dividing the text of
a @tech{TEI document} into a linear stream of logical groupings
that share certain metadata, such as location and authorship.
Dividing a @tech{TEI document} into such groupings is a common requirement
for many applications: search (as in @racket[term-search]) has been our
initial motivating use-case, but the same process is needed to, for example,
plot trends in the use of a particular term over the course of a book.

This library implements the common functionality needed to divide a 
@tech{TEI document} into @tech{segments} with @racket[tei-document-segments]
and defines an extensible interface for working with @tech{segment metadata}.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Segment Basics}

@defpredicate[segment?]{
 A predicate recognizing @tech{segments}.

 A @deftech{segment} value represents a contiguous logical subdivision of
 a @tech{TEI document}.
 While the XML structure of @tech{TEI documents} involves nested and
 overlapping hierarchies, @tech{segments} present a linear view of a document.

 To a first approximation, a @tech{segment} might correspond to a paragraph.
 All of the textual content that falls within a @tech{segment} shares the
 same metadata: for example, a segment might come from chapter one, pages 2–3,
 and have been written by Paul @|Ricoeur|.
 In fact, @tech{segments} can be more granular than paragraphs:
 a paragraph with a footnote in the middle might be divided into
 several @tech{segments}.
 On the other hand, in a @tech{TEI document} for which we have not yet completed
 paragraph inference (see @racket[tei-document-guess-paragraphs]), @tech{segments}
 might be based on page breaks and could be longer than a paragraph.

 As the above suggests, a @tech{segment} is assosciated with a specific
 @tech{TEI document}: not just the identity of the @tech{instance},
 as might be determined by @racket[instance-title/symbol],
 but with the state of the @tech{TEI document} itself as reflected
 by @racket[tei-document-checksum].
 
 @tech{Segments} are a general, extensible way of managing this contextual
 information: concrete applications are likely to implement specialized
 representations, but these can support the @tech{segment} interface
 using @racket[prop:segment].
 This library defines two built-in kinds of @tech{segments}—@racket[base-segment]
 structs and @tech{segment metadata} values—along with a core function
 for dividing a @tech{TEI document} into @tech{segments}, @racket[tei-document-segments].
 A different part of this library uses the @tech{segment} interface
 to support location and context information for @tech{search result} values,
 even though a @tech{search result} is a more fine-grained level of organization
 within a @tech{segment}.
 Other application programs can similarly build on the common @tech{segment} interface.
}

@deftogether[
 (@defproc[(tei-document-segments [doc tei-document?])
           (listof base-segment?)]
   @defstruct*[base-segment ([meta segment-meta?]
                             [body (and/c string-immutable/c #px"\\S")])
               #:omit-constructor])]{
 The function @racket[tei-document-segments] splits a @tech{TEI document}
 @racket[doc] into a list of @tech{segments}:
 more specifically, instances of the @racket[base-segment] structure type.

 The result of @racket[tei-document-segments] is weakly cached to reduce
 the cost of repeated calls.
 
 In addition to metadata, a @racket[base-segment] value also contains the
 full textual data of the @tech{segment}, but this is not a requirement:
 most other kinds of @tech{segment} values will likely not wish to do so.
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Working with Segments}
@deftogether[
 (@defproc[(segment-get-meta [seg segment?])
           segment-meta?]
   @defpredicate[segment-meta?]
   @defproc[(segment-meta=? [a segment?] [b segment?])
            boolean?])]{
 Every @tech{segment} has an assosciated @deftech{segment metadata} value
 that encapsulates its location, authorship, and other information.
 A @tech{segment metadata} value can be recognized by the predicate
 @racket[segment-meta?] and is itself a @tech{segment}
 (and thus satisfies @racket[segment?]): the @tech{segment metadata} value
 contains all of the information needed by the @tech{segment} interface.
 Any @tech{segment} @racket[seg] can be converted to a plain
 @tech{segment metadata} value using @racket[segment-get-meta].
 
 In addition to being the most minimal representation of a @tech{segment},
 @tech{segment metadata} values can be serialized with
 @racketmodname[racket/serialize].

 The function @racket[segment-meta=?] tests @tech{segments} for
 equality based on their @tech{segment metadata} values:
 it will consider @tech{segments} of different specific types
 ``the same'' if they have equivalent @tech{segment metadata} values.
 Any @tech{segments} that are @racket[segment-meta=?] can
 be used interchangably for the purposes of the functions documented
 in this section.
}

@deftogether[
 (@defproc[(segment-resp-string [seg segment?])
           string-immutable/c]
   @defproc[(segment-by-ricoeur? [seg segment?])
            boolean?])]{
 Functions to access the ``responsible party'' for
 the @tech{segment} @racket[seg].
 
 Internally, @racket[segment-resp-string] obtains a string
 suitable for display to end-users
 naming the ``responsible party'' for the @tech{segment}
 (such as @Ricoeur, an editor, or a translator)
 using lower-level functions such as @racket[tei-element-resp]
 and @racket[instance-get-resp-string].

 The predicate @racket[segment-by-ricoeur?] recognizes only
 @tech{segments} by @Ricoeur himself.
}

@deftogether[
 (@defproc[(segment-page-spec [seg segment?])
           page-spec/c]
   @defthing[page-spec/c flat-contract?
             #:value (or/c (maybe/c string-immutable/c)
                           (list/c (maybe/c string-immutable/c)
                                   (maybe/c string-immutable/c)))])]{
 Returns the location of the @tech{segment} @racket[seg] in
 terms of pages.
 
 If the returned value is a two-element list, the @tech{segment} spans
 more than one page: the first element of such a list represents
 the page on which the @tech{segment} starts, and the second element
 the page on which it ends.
 Otherwise, if the returned value is not a list, the @tech{segment} is
 fully contained in a single page, and the value represents that page.

 In either case, a value of @racket[(nothing)] signifies that the @tag{pb}
 element it represents was not numbered (i.e. it had no @attr{n} attribute).
 A @racket[just] value contains the page number, taken from the value of
 the corresponding @tag{pb}'s @attr{n} attribute.
}

@deftogether[
 (@defproc[(segment-location-stack [seg segment?])
           location-stack?]
   @defproc[(location-stack->strings [location-stack location-stack?])
            (listof string-immutable/c)]
   @defpredicate[location-stack?])]{
 Returns the location of the @tech{segment} @racket[seg] in
 terms of the structure of the source @tech{TEI document}, with reference
 to chapters, sections, footnotes, and the like.
 The location is represented by an opaque @deftech{location stack} value,
 which is recognized by the predicate @racket[location-stack?].

 A @tech{location stack} can be converted to a list of strings suitable for
 display to end-users via @racket[location-stack->strings].
 The strings in the resulting list describe the location from the broadest
 level of organization to the narrowist
 (e.g. @racket['("chapter 1" "footnote 3")], though the precise textual
 content of the returned strings is unspecified).
}

@defproc[(segment-title/symbol [seg segment?])
         symbol?]{
 Returns the same symbol that would have been returned by
 @racket[instance-title/symbol] applied to the source @tech{TEI document}
 of the @tech{segment} @racket[seg].
}

@defthing[segment-order order?]{
 An @tech[#:doc '(lib "data/scribblings/data.scrbl")]{order}
 (in the sense of @racketmodname[data/order]) on @tech{segments}.
 It is an error to apply @racket[segment-order]'s comparison functions
 to @tech{segments} that do not come from the same @tech{TEI document},
 where ``the same'' encompases both @racket[instance-title/symbol] and
 @racket[tei-document-checksum].

 Sorting @tech{segments} according to @racket[segment-order]'s less-than
 relation places them in the order in which they occurred in the
 source @tech{TEI document}.
}

@defproc[(segment-counter [seg segment?])
         natural-number/c]{
 Returns an integer representing the position of
 the @tech{segment} @racket[seg] relative to other @tech{segments}
 from the same source @tech{TEI document}.
}
    
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Implementing New Types of Segments}
@defthing[prop:segment
          (struct-type-property/c
           (-> any/c segment-meta?))]{
 A structure type property for implementing new types of @tech{segments}.
                                                         
 The value for the property must be a function that accepts an
 instance of the new structure type and returns a
 @tech{segment metadata} value.
 An instance of the new structure type will satisfy @racket[segment?]
 and can be used with any of the functions above equivalently
 to using the returned @tech{segment metadata} value directly.

 The function given as a value for @racket[prop:segment] should always
 return the very same @tech{segment metadata} value when called with
 the same argument.
 This invariant is not currently checked, but may be in the future.
}


