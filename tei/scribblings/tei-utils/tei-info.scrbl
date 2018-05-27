#lang scribble/manual

@title{TEI Info Values}
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

@TODO/void[Split TEI-info interface into a bibliographic part
           and a document-specific part.]
@bold{TODO:} I think it might make sense in connection with 
this ongoing round of breaking API changes
to split the TEI-info interface into a bibliographic information part
and a document-specific part, so that the bibliographic part
might eventually come from our bibliographic database rather than
the TEI XML files.
The document-specific part might eventually contain the
value from @racket[tei-document-md5] as well as
@racket[tei-document-paragraphs-status].



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

@defproc[(tei-title [info TEI-info?])
         string?]{
 Returns the title of the document, including subtitles.
}

@defproc[(tei-citation [info TEI-info?])
         string?]{
 Returns the human-readable citation for the work.
}
 
@defproc[(tei-publication-date [info TEI-info?])
         date?]{
 Returns the publication date of the specific version of the work
 on which the TEI document is based. 
}
 
@defproc[(tei-orig-publication-date [info TEI-info?])
         date?]{
 Returns the date when the work (as a whole) was first published,
 which may or may not be the same as
 the result of @racket[tei-publication-date]. 
}

@defproc[(tei-publication-original? [info TEI-info?])
         any/c]{
 Indicates whether the version represented by @racket[info]
 was the first version of the work as a whole to be published.
 If @racket[tei-publication-original?] returns a non-false value,
 @racketblock[(equal? (tei-publication-date info)
                      (tei-orig-publication-date info))]
 will allways be @racket[#t]; however, it is possible for a
 non-original version to be publised in the same year as the
 original, so the inverse does not hold.
}

@defproc[(tei-book/article [info TEI-info?])
         (or/c 'book 'article)]{
 Indicates whether @racket[info] represents a book or an article.
}

@deftogether[
 @(@defproc[(tei-document-paragraphs-status [info TEI-info?])
            guess-paragraphs-status/c]
    @defthing[guess-paragraphs-status/c flat-contract?
              #:value (or/c 'todo
                            'line-breaks
                            'blank-lines
                            'done
                            'skip)])]{
 Returns a symbol indicating whether paragraph-guessing
 has been performed for the document represented by
 @racket[info].
}


@defproc[(tei-get-resp-string [info TEI-info?] [resp symbol?])
         string?]{
 Returns a string naming the author or editor whose
 @attr{xml:id} attribute value is the string form of @racket[resp],
 raising an exception if none exists.
}


@section{Derived TEI Info}
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


@subsection{Class-based Objects}
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
 
 @defmethod[#:mode public-final
            (get-title)
            string?]{
  Like @racket[(tei-title #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-citation)
            string?]{
  Like @racket[(tei-citation #,(this-obj))].
 }
 
 @defmethod[#:mode public-final
            (get-orig-publication-date)
            date?]{
  Like @racket[(tei-orig-publication-date #,(this-obj))].
 }
 
 @defmethod[#:mode public-final
            (get-publication-date)
            date?]{
  Like @racket[(tei-publication-date #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-publication-original?)
            any/c]{
  Like @racket[(tei-publication-original? #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-book/article)
            (or/c 'book 'article)]{
  Like @racket[(tei-book/article #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-guess-paragraphs-status)
            guess-paragraphs-status/c]{
  Like @racket[(tei-guess-paragraphs-status #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-resp-string [resp symbol?])
            string?]{
  Like @racket[(tei-get-resp-string #,(this-obj) resp)].
 }
}







