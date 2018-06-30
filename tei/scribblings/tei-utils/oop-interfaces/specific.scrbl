#lang scribble/manual

@title{Element-specific Interfaces}

@(require (only-in "../for-manual.rkt"
                   TODO
                   TODO/void
                   TODO/scrbl)
          (for-label (except-in racket
                                date
                                date?)
                     racket/unit
                     xml
                     data/maybe
                     ricoeur/tei/oop
                     (submod ricoeur/tei/oop/old-high-level/search/common private)
                     gregor
                     ))

The interfaces in this section identify objects that correspond to
specific TEI elements. Some add additional element-specific methods;
the others serve merely to identify elements convieniently.

@section{@tt{TEI<%>}}
@definterface[TEI<%> (element<%>
                      TEI-info<%>
                      get-page-breaks<%>
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
                       out)] because @(method TEI<%> write-TEI)
  also writes an XML declaration and appropriate @tt{DOCTYPE} declaration.
 }

 @defmethod[(get-md5) string?]{
  Returns the md5 checksum of @(this-obj). The checksum is based on the
  same standardized XML representation used by @(method TEI<%> write-TEI).
 }
  
}

@section{@tt{teiHeader<%>}}
@definterface[teiHeader<%> (TEI-info<%>)]{
 This interface identifies objects representing
 @litchar{<teiHeader>}@tt{...}@litchar{</teiHeader>} elements.
}

@section{@tt{pb<%>}}
@definterface[pb<%> (get-page-breaks<%>)]{
 This interface identifies objects representing page-break elements.

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

@section{@tt{note<%>}}
@definterface[note<%> (guess-paragraphs<%>
                       get-page-breaks<%>)]{
 An interface identifying footnote and endnote elements.
 @defmethod[(get-place) (->m (or/c 'foot 'end))]{
  Indicates whether @(this-obj) represents a footnote or an endnote.
 }
 @defmethod[(get-n) (->m string?)]{
  Returns a string representing how @(this-obj) was identified in
  the original, e.g. @racket["1"] or @racket["*"].
 }
 @defmethod[(get-transl?) (->m (or/c #f 'transl))]{
  Returns @racket['transl] if @(this-obj) is a translation note.
 }
}

@section{@tt{p<%>}}
@definterface[p<%> (get-page-breaks<%>)]{
 An interface identifying paragraph elements.
}

@section{@tt{ab<%>}}
@definterface[ab<%> (get-page-breaks<%>)]{
 An interface identifying "anonymous block" elements.

 @defmethod[(do-guess-paragraphs [#:mode mode (or/c 'blank-lines 'line-breaks)
                                  'blank-lines])
            (listof (or/c (is-a?/c pb<%>)
                          (is-a?/c p<%>)))]{
  Used to implement @(xmethod guess-paragraphs<%> guess-paragraphs)
  @(TODO/void do-guess-paragraphs in ab<%>
              #: Should this be private?)
 }
}



