#lang scribble/manual

@title[#:version ""]{Object System}

@(require (for-label (except-in racket
                                date
                                date?)
                     racket/unit
                     xml
                     data/maybe
                     ricoeur/tei
                     ricoeur/tei/xexpr/signatures
                     (submod ricoeur/tei/search/common private)
                     gregor
                     ))

Internally, this library represents TEI XML documents
as nested objects (from Racket's class-based object system),
which form a thin layer of abstraction over plain x-expressions.


@defproc[(read-TEI [in input-port? (current-input-port)]
                   [#:filename filename (or/c #f path-string?) #f])
         (is-a?/c TEI<%>)]{
 Produces a Racket object representing the TEI XML
 document read from @racket[in].

 If a @racket[filename] is provided, it is used (without the
 directory portion) for @(method TEI-info<%> get-filename).
}

@defproc[(file->TEI [file (and/c path-string? file-exists?)])
         (is-a?/c TEI<%>)]{
Produces a Racket object representing the TEI XML document @racket[file].
}

@defproc[(tag->element [tag any-tei-xexpr/c]
                       [#:filename filename (or/c #f path-string?) #f])
         (is-a?/c element<%>)]{
 Converts a TEI XML tag, represented as an x-expression,
 to a Racket object.

 If @racket[filename] is provided, it is used as with @racket[read-TEI]
 when the resulting object implements @racket[TEI-info<%>].
}

@deftogether[
 (@defproc[(maybe-date<? [a (maybe/c date?)]
                         [b (maybe/c date?)])
           any/c]
   @defproc[(maybe-date>? [a (maybe/c date?)]
                          [b (maybe/c date?)])
            any/c])]{
 Comparison functions on optional date values.
}

@section{General Interfaces}

The interfaces represented in this section contain methods
implemented by broad categories of TEI elements.

@definterface[element<%> ()]{
 TEI elements are represented by Racket objects implementing
 the @racket[element<%>] interface.

 @margin-note{The @racket[element<%>] interface and derived
  interfaces contain additional private methods to preserve
  certain invarialts, and thus may only be implemented by
  the objects provided from @racketmodname[ricoeur/tei].}

 @defmethod[(to-xexpr) any-tei-xexpr/c]{
  Returns the x-expression representation of @(this-obj).
 }

 @defmethod[(to-plain-text) string?]{
  Converts @(this-obj) to a plain-text string. The resulting
  string is @italic{not} the XML representation of
  @(this-obj): it is formated for uses that expect unstructured
  plain text. 
 }

 @defmethod[(get-name) symbol?]{
  Returns the name of the @(this-obj).
 }

 @defmethod[(get-attributes) (listof (list/c symbol? string?))]{
  Returns the attributes of @(this-obj) using the same
  representation used by x-expressions.
 }

 @defmethod[(get-body) (listof element-or-xexpr/c)]{
  Returns the contents of @(this-obj), with child elements
  represented by objects implementing @racket[element<%>].
 }

}

@defproc[(tei-element? [v any/c]) any/c]{
 A predicate recognizing Racket objects that implement the
 @racket[element<%>] interface.
}

@defthing[element-or-xexpr/c flat-contract?
          #:value (or/c (is-a?/c element<%>)
                        string?
                        symbol?
                        valid-char?
                        cdata?
                        comment?
                        p-i?)]{
 A contract recognizing objects implementing @racket[element<%>]
 and non-tag x-expressions.
}

@definterface[elements-only<%> (element<%>)]{
 Elements implementing this interface may not contain textual
 data directly.

 @defmethod[#:mode extend
            (get-body) (listof (or/c (is-a?/c element<%>)
                                     comment?
                                     p-i?))]{
  Like @(xmethod element<%> get-body), but the returned list may
  not contain strings, symbols, characters, or @racket[cdata].
 }

 @defmethod[(get-body/elements-only) (listof (is-a?/c element<%>))]{
  Like @(method elements-only<%> get-body), but the resulting list
  contains only the child elements: that is, it will not contain
  comments or processing instructions.
 }
}


@definterface[get-page-breaks<%> (element<%>)]{
 An interface implemented by objects from which page-breaks can
 be extracted. Note that @racket[pb<%>] is a sub-interface of
 @racket[get-page-breaks<%>] but not of @racket[TEI-body<%>].

 @defmethod[(get-page-breaks) (listof (is-a?/c pb<%>))]{
  Returns a list of objects representing the page-break elements
  recursively contained by @(this-obj)
 }
}

@(define warning:private-for-search
   @list{@bold{This method should be considered private.} While it
 is documented here for reference, its specification is inextricably tied
 to the specific implementation of @racket[prepare-pre-segments]
 and is subject to backwards-incompatable changes.
 })

@definterface[TEI-body<%> (get-page-breaks<%>)]{
 This interface is implemented by elements containing (all or part of)
 the body of the document. In most cases, these methods should be invoked
 on the top-level object implementing @racket[TEI<%>].

 @defmethod[(to-pre-segments [this any/c]
                             [pre-segment-accumulator? (-> any/c any/c)]
                             [call-with-metadata
                              (->* ((-> any))
                                   (#:resp #rx"#.+"
                                    #:location location-stack-entry/c)
                                   any)]
                             [accumulator
                              (letrec ([acc/c
                                        (and/c pre-segment-accumulator?
                                               (-> #:body string?
                                                   #:page
                                                   (or/c (maybe/c string?)
                                                         (list/c (maybe/c string?)
                                                                 (maybe/c string?)))
                                                   (recursive-contract acc/c)))])
                                acc/c)]
                             [init-pb (is-a?/c pb<%>)])
            (values acc/c (is-a?/c pb<%>))]{
  @warning:private-for-search

  This method is responsible for calling the successive @tech{pre-segment accumulators}
  for the contents of @(this-obj), including calling the
  @(method TEI-body<%> to-pre-segments) methods of its children.
  (The exact implementation is specialized by type of element to handle, for example,
  ignoring indeces and tables of contents, as well as other special cases.)
  It is called by @(xmethod TEI<%> do-prepare-pre-segments).

  Internally, it uses implementations declared using @racket[pubment] and @racket[augride]
  to dispatch to @(method TEI-body<%> to-pre-segments/add-metadata)
  in a non-overridable part of its implementation.
 }
 @defmethod[#:mode pubment
            (to-pre-segments/add-metadata [pre-segment-accumulator? (-> any/c any/c)]
                                          [call-with-metadata
                                           (->* ((-> any))
                                                (#:resp #rx"#.+"
                                                 #:location location-stack-entry/c)
                                                any)]
                                          [thunk (-> (values pre-segment-accumulator?
                                                             (is-a?/c pb<%>)))])
            (-> (values pre-segment-accumulator?
                        (is-a?/c pb<%>)))]{
  @warning:private-for-search

  This method is called in a non-overridable part of the implementation of
  @(method TEI-body<%> to-pre-segments), which supplies it with a @racket[thunk]
  argument that will do the actual work of that method.
  The implementation of @(method TEI-body<%> to-pre-segments/add-metadata)
  is responsible for for using the @racket[call-with-metadata] argument to
  install any metadata pertaining to @(this-obj) during the dynamic extent of
  @racket[thunk].
 }
}


@definterface[guess-paragraphs<%> (TEI-body<%>)]{
 Extends @racket[TEI-body<%>] with a method for automatically
 converting child "anonymous block" elements (see @racket[ab<%>])
 to paragraphs. In most cases, this method should be invoked
 on the top-level object implementing @racket[TEI<%>].

 @defmethod[(guess-paragraphs [#:mode mode (or/c 'blank-lines 'line-breaks) 'blank-lines])
            (is-a?/c (object-interface #,(this-obj)))]{
  Returns an object like @(this-obj), but with any recursively conained
  "anonymous block" elements (see @racket[ab<%>]) replaced by
  paragraph and page-break elements (see @racket[p<%>] and @racket[pb<%>]).
  Children of the "anonymous block" elements are preserved.

  When mode is @racket['blank-lines] (the default),
  @(method guess-paragraphs<%> guess-paragraphs) assumes a blank line between
  blocks of text is used to indicate a paragraph; otherwise, it assumes
  every line-break begins a new paragraph. In both cases, entirely empty
  paragraphs are ommited, and freestanding page-breaks are allowed.

  Using @(method guess-paragraphs<%> guess-paragraphs) does not eliminate
  the need for manual review: for example, it will identify as "paragraphs"
  segments of text that should be represented as headings, list items, or
  footnotes. However, it saves a great deal of time for the common cases.
 }
}

@defproc[(guess-paragraphs? [v any/c]) any/c]{
 A predicate recognizing Racket objects that implement the
 @racket[guess-paragraphs<%>] interface
}

@definterface[TEI-info<%> ()]{
 An interface implemented by objects encapsulating information about a
 TEI XML document, notably objects implementing @racket[TEI<%>] or
 @racket[teiHeader<%>].

 @margin-note{Unlike other interfaces in this section, @racket[TEI-info<%>]
  is not derived from @racket[element<%>], so it may be freely implemented
  by client modules. However, additional required methods may be added
  to @racket[TEI-info<%>] without warning.}
 
 @defmethod[(get-title) string?]{
  Returns the title of the document, including subtitles.
 }
 @defmethod[(get-filename) (or/c #f string?)]{
  Returns the file name (without directory) used to instantiate
  @(this-obj), or @racket[#f] if none is known.
  See alse @racket[read-TEI] and @racket[file->TEI].
 }
 @defmethod[(get-publication-date) date?]{
  Returns the publication date of the specific version of the work
  on which the TEI document is based.
 }
 @defmethod[(get-original-publication-date) date?]{
  Returns the date when the work (as a whole) was first published,
  which may or may not be the same as the result of
  @(method TEI-info<%> get-publication-date).
 }
 @defmethod[(get-citation) string?]{
  Returns the human-readable citation for the work
 }
 @defmethod[(get-resp-string [resp symbol?])
            string?]{
  Returns a string naming the author or editor whose @tt{id}
  attribute is the string form or @racket[resp], raising
  an exception if none exists.
 }
}



@section{Element-specific Interfaces}

The interfaces in this section identify objects that correspond to
specific TEI elements. Some add additional element-specific methods;
the others serve merely to identify elements convieniently.

@definterface[TEI<%> (element<%>
                      TEI-info<%>
                      TEI-body<%>
                      guess-paragraphs<%>
                      elements-only<%>)]{
 The object representing the top-level
 @litchar{<TEI>}@tt{...}@litchar{</TEI>} element implements this
 interface.

 @defmethod[(get-teiHeader) (is-a?/c teiHeader<%>)]{
  Returns an object representing the @tt{teiHeader} element
  from the document
 }

 @defmethod[(write-TEI [out output-port? (current-output-port)])
            any]{
  Writes the XML representation of @(this-obj) to @racket[out],
  prettyprinted using @racket[call/prettyprint-xml-out].

  Use @(method TEI<%> write-TEI) rather than 
  @racket[(write-xexpr (send #,(this-obj) #,(method element<%> to-xexpr))
                       out)] because @(method TEI.2<%> write-TEI)
  also writes an XML declaration and appropriate @tt{DOCTYPE} declaration.
 }

 @defmethod[(get-md5) string?]{
  Returns the md5 checksum of @(this-obj). The checksum is based on the
  same standardized XML representation used by @(method TEI<%> write-TEI).
 }
                
 @defmethod[(do-prepare-pre-segments [pre-segment-accumulator? (-> any/c any/c)]
                                     [call-with-metadata
                                      (->* ((-> any))
                                           (#:resp #rx"#.+"
                                            #:location location-stack-entry/c)
                                           any)]
                                     [title->accumulator
                                      (letrec ([acc/c
                                                (and/c pre-segment-accumulator?
                                                       (-> #:body string?
                                                           #:page
                                                           (or/c (maybe/c string?)
                                                                 (list/c (maybe/c string?)
                                                                         (maybe/c string?)))
                                                           (recursive-contract acc/c)))])
                                        (-> string? acc/c))])
            pre-segment-accumulator?]{
  @warning:private-for-search

  This signature of this method is complex because the modules implementing
  the object hierarchy do not depend on the modules providing the search implementation
  and because the search implementation needs to maintain certain unspecified
  internal invariants about the precise structure of the @racket[pre-segment-meta]
  field.
  
  The @racket[pre-segment-accumulator?] argument is supplied solely so that it can
  be used by the contract.
  It defines a notion of a @deftech{pre-segment accumulator}, an opaque value which
  may be called as a function to record some @racket[#:body] and @racket[#:page]
  for the next @racket[pre-segment] in the document (so order does matter)
  and return a new value like itself for further accumulating. It is responsible for
  populating the other fields of a @racket[pre-segment], perhaps with metadata installed
  by the @racket[call-with-metadata] argument.

  The @racket[call-with-metadata] argument is a function that installs some
  metadata for use by @tech{pre-segment accumulators} within the dynamic extent of
  the thunk supplied as its by-position argument.
  It is used by @(xmethod TEI-body<%> to-pre-segments/add-metadata).

  The @racket[title->pre-segment-accumulator] argument is called with the title
  of the document and must return an initial @tech{pre-segment accumulator}.

  The result is the final @tech{pre-segment accumulator} produced by traversing
  body of the document (in order) with calls to @(xmethod TEI-body<%> to-pre-segments).
  The actual @racket[pre-segment] values are extracted by @racket[prepare-pre-segments].
 }
}

@definterface[teiHeader<%> (TEI-info<%>)]{
 This interface identifies objects representing
 @litchar{<teiHeader>}@tt{...}@litchar{</teiHeader>} elements.
}

@definterface[pb<%> (get-page-breaks<%>)]{
 This interface identifies objects representing pagebreak elements.

 @defmethod[(get-page-string) (maybe/c string?)]{
  Returns a @racket[just] value containing the original string
  from the element's @litchar{n} attribute or @racket[nothing]
  if none was present.
 }

 @defmethod[(get-kind) (or/c 'none 'number 'roman 'other)]{
  Returns a symbol repesenting the kind of page number
 }

 @defmethod[(get-numeric) (maybe/c number?)]{
  Returns an optional value representing the page number string converted
  to a number, if possible 
 }
}

@definterface[p<%> (TEI-body<%>)]{
 An interface identifying paragraph elements.
}

@definterface[ab<%> (TEI-body<%>)]{
 An interface identifying "anonymous block" elements.

 @defmethod[(do-guess-paragraphs [#:mode mode (or/c 'blank-lines 'line-breaks)
                                  'blank-lines])
            (listof (or/c (is-a?/c pb<%>)
                          (is-a?/c p<%>)))]{
  Used to implement @(xmethod guess-paragraphs<%> guess-paragraphs)
 }
}






