#lang scribble/manual

@title{Bibliographic Information}
@;; ricoeur/tei/kernel/sans-lang
@;;   to support ricoeur/tei/kernel/lang/specification-lang
@(declare-exporting ricoeur/tei/base
                    ricoeur/tei
                    #:use-sources (ricoeur/tei/kernel/sans-lang)
                    )

@(require "for-manual.rkt"
          (for-label ricoeur/tei/kernel/sans-lang
                     ))

@margin-note{
 In the medium- to long-term, we plan to transition to
 managing the corpus-level bibliographic data in a database,
 rather than the TEI documents.
 That transition will likely result in changes to this interface.
}

@TODO/void[Explain instance in prose.]

@defpredicate[instance-info?]{
 An @deftech{instance info} value encapsulates bibliographic
 information about an @deftech{instance}.
 The predicate @racket[instance-info?] recognizes @tech{instance info} values.

 All @tech{instance info} values support the same high-level API,
 and new kinds of values (including class-based objects)
 can implement the @tech{instance info} interface.
 
}

@defproc[(get-plain-instance-info [info instance-info?])
         plain-instance-info?]{
 Converts any @tech{instance info} value to a @deftech{plain instance info} value,
 which serves as a cannonical form of the encapsulated information.
}

@defproc[(instance-title [info instance-info?])
         string?]{
 Returns the title of the document, including subtitles.
}

@defproc[(instance-citation [info instance-info?])
         string?]{
 Returns the human-readable citation for the work.
}
 
@defproc[(instance-publication-date [info instance-info?])
         date?]{
 Returns the publication date of the specific version of the work
 on which the TEI document is based. 
}
 
@defproc[(instance-orig-publication-date [info instance-info?])
         date?]{
 Returns the date when the work (as a whole) was first published,
 which may or may not be the same as
 the result of @racket[instance-publication-date]. 
}

@defproc[(instance-publication-original? [info instance-info?])
         any/c]{
 Indicates whether the version represented by @racket[info]
 was the first version of the work as a whole to be published.
 If @racket[instance-publication-original?] returns a non-false value,
 @racketblock[(equal? (instance-publication-date info)
                      (instance-orig-publication-date info))]
 will allways be @racket[#t]; however, it is possible for a
 non-original version to be publised in the same year as the
 original, so the inverse does not hold.
}

@defproc[(instance-book/article [info instance-info?])
         (or/c 'book 'article)]{
 Indicates whether @racket[info] represents a book or an article.
}

@defproc[(instance-get-resp-string [info instance-info?] [resp symbol?])
         string?]{
 Returns a string naming the author or editor whose
 @attr{xml:id} attribute value is the string form of @racket[resp],
 raising an exception if none exists.
}


@section{Derived Instance Info}
@defthing[prop:instance-info (struct-type-property/c
                              (-> any/c plain-instance-info?))]{
 New kinds of values can support the @tech{instance info} API 
 through the structure type property @racket[prop:instance-info].
 The value for the @racket[prop:instance-info] must be a function
 that, given an instance of the new structure type,
 returns a @tech{plain instance info} value.
 The function should always return the same
 @tech{plain instance info} value when called with the same argument:
 this requirement is not currently enforced, but may be
 in the future.

 Special support is provided for using class-based objects
 as @tech{instance info} values via @racket[instance-info-mixin].
}

@defpredicate[plain-instance-info?]{
 Recognizes @tech{plain instance info} values.
}


@subsection{Class-based Objects}
@defmixin[instance-info-mixin () (instance-info<%>)]{

 An instance of a class extended with @racket[instance-info-mixin]
 can be used as an @tech{instance info} value.
 This is the prefered way for class-based objects
 to support the @tech{instance info} API.

 The resulting class will implement the @racket[instance-info<%>]
 interface, which also makes some of the @tech{instance info}
 functions available as methods for use with @racket[inherit],
 @racket[send], etc.
                                           
 @defconstructor/auto-super[([instance-info instance-info?])]{                  

  The @racket[instance-info] initialization argument determines
  the behavior of @(this-obj) when used as an @tech{instance info} value.
  
  Specifically, @racket[(get-plain-instance-info #,(this-obj))]
  will return the same @tech{plain instance info} value as
  @racket[(get-plain-instance-info instance-info)].
                                                    
}}

@definterface[instance-info<%> ()]{

 An interface identifying classes which have been extended
 with @racket[instance-info-mixin].
 Objects which implement @racket[instance-info<%>] can be used
 as @tech{instance info} values.

 In addition to the methods documented below,
 @racket[instance-info<%>] also includes additional, private methods:
 @racket[instance-info<%>] can only be implemented
 using @racket[instance-info-mixin].
 
 @defmethod[#:mode public-final
            (get-title)
            string?]{
  Like @racket[(instance-title #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-citation)
            string?]{
  Like @racket[(instance-citation #,(this-obj))].
 }
 
 @defmethod[#:mode public-final
            (get-orig-publication-date)
            date?]{
  Like @racket[(instance-orig-publication-date #,(this-obj))].
 }
 
 @defmethod[#:mode public-final
            (get-publication-date)
            date?]{
  Like @racket[(instance-publication-date #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-publication-original?)
            any/c]{
  Like @racket[(instance-publication-original? #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-book/article)
            (or/c 'book 'article)]{
  Like @racket[(instance-book/article #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-resp-string [resp symbol?])
            string?]{
  Like @racket[(instance-get-resp-string #,(this-obj) resp)].
 }
}







