#lang scribble/manual

@title{Bibliographic Information}
@;; ricoeur/tei/kernel
@;;   to support ricoeur/tei/kernel/lang/specification-lang
@(declare-exporting ricoeur/tei/base
                    ricoeur/tei
                    #:use-sources (ricoeur/tei/kernel)
                    )

@(require "for-manual.rkt"
          (for-label ricoeur/tei/kernel))

This section documents an API for accessing bibliographic
information about @deftech{instances}.
An @tech{instance} is an abstraction for the most specific level 
of bibliographic information with which Digital @Ricoeur is concerned.
The term ``instance'' is drawn from the 
@link["https://www.loc.gov/bibframe/docs/bibframe2-model.html"]{BibFrame}
model for bibliographic data.

Concretely, @Ricoeur's publications exist as particular physical
objects, such as a specific copy of a book,
housed on a particular shelf in a particular library.
The ``@tech{instance}'' abstraction refers to all concrete copies
that are ``the same'' for Digital @Ricoeur's purposes:
that is, they are bibliographically identical, even to the extent
of having exactly the same pagination.
Digital @Ricoeur aims to prepare one TEI XML file
for each @tech{instance}.

An @tech{instance} is an abstraction, not a kind of Racket value,
but many kinds of Racket values (particularly @tech{TEI document}
values) are assosciated with an @tech{instance}.
Racket values that encapsulate bibliographic information about a
particular @tech{instance} are refered to as @tech{instance info}
values. This library defines a common interface for working with
@tech{instance info} values, and clients can implement additional
types of @tech{instance info} values that will support the same
interface.

In the medium to long term, Digital @Ricoeur plans to transition to
managing the corpus-level bibliographic data about @tech{instances}
in a database, rather than the TEI XML files.
That transition will likely result in changes to this interface.


@defpredicate[instance-info?]{
 An @deftech{instance info} value encapsulates bibliographic
 information about an @tech{instance}.
 The predicate @racket[instance-info?] recognizes @tech{instance info} values.

 All @tech{instance info} values support the same high-level API,
 and new kinds of values (including class-based objects)
 can implement the @tech{instance info} interface.
 (See @racket[prop:instance-info] and @racket[instance-info-mixin].)
}

@defform[#:kind "match expander"
         (instance-info kw-pat ...)
         #:grammar [(kw-pat (code:line #:title title-pat)
                            (code:line #:title/symbol title/symbol-pat)
                            (code:line #:citation citation-pat)
                            (code:line #:orig-publication-date orig-publication-date-pat)
                            (code:line #:publication-date publication-date-pat)
                            (code:line #:publication-original? publication-original?-pat)
                            (code:line #:language language-pat)
                            (code:line #:book/article book/article-pat))]]{
 Matches any @tech{instance info} value, then matches any
 sub-patterns against the values that would be returned by the
 corresponding procedures documented below.

 Each keyword may appear at most once.
}

@defproc[(get-plain-instance-info [info instance-info?])
         plain-instance-info?]{
 Converts any @tech{instance info} value to a @deftech{plain instance info} value,
 which serves as a cannonical representation of the encapsulated information.
 
 In addition to being needed with @racket[prop:instance-info]
 to implement the @tech{instance info} interface for new kinds of values,
 @racket[get-plain-instance-info] can be useful to keep only a minimal
 representation of the @tech{instance info} reachable,
 rather than, say, an entire @tech{TEI document} value.
}

@defproc[(instance-title [info instance-info?])
         string-immutable/c]{
 Returns the title of the @tech{instance}, including subtitles.

 See also @racket[title<?].
}

@defproc[(instance-title/symbol [info instance-info?])
         symbol?]{
 Equivalent to @racket[(string->symbol (instance-title info))].
}

@defproc[(instance-citation [info instance-info?])
         string-immutable/c]{
 Returns a human-readable citation describing the @tech{instance}.
}
 
@defproc[(instance-publication-date [info instance-info?])
         date?]{
 Returns the publication date of the @tech{instance}. 
}
 
@defproc[(instance-orig-publication-date [info instance-info?])
         date?]{
 Returns the date when the work (as a whole) was first published,
 which may or may not be the same as
 the result of @racket[instance-publication-date]. 
}

@defproc[(instance-publication-original? [info instance-info?])
         boolean?]{
 Indicates whether the version represented by @racket[info]
 was the first @tech{instance} of the work as a whole to be published.
 If @racket[instance-publication-original?] returns a non-false value,
 @racketblock[(equal? (instance-publication-date info)
                      (instance-orig-publication-date info))]
 will allways be @racket[#true]; however, it is possible for a
 non-original @tech{instance} to be publised in the same year as the
 original, so the inverse does not hold.
}

@deftogether[
 (@defproc[(instance-language [info instance-info?])
           language-symbol/c]
   @defthing[language-symbol/c flat-contract?
             #:value (or/c 'en 'fr 'de)])]{
 Identifies the primary language of the @tech{instance}.
 English, French, and German are currently supported.

 The contract @racket[language-symbol/c] recognizes
 symbols that identify one of the supported languages.
 The symbols are chosen from the IANA
 @link["https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry"]{
  Language Subtag Registry}, as specified by
 @link["https://tools.ietf.org/html/bcp47"]{BCP 47}.
}

@defproc[(instance-book/article [info instance-info?])
         (or/c 'book 'article)]{
 Indicates whether the @tech{instance} is a book or an article.
}

@defproc[(instance-get-resp-string [info instance-info?] [resp symbol?])
         string-immutable/c]{
 Returns a string, suitable for display to end users,
 naming the @tag{author} or @tag{editor} specified by the TEI XML element
 with an @attr{xml:id} attribute value of @racket[(symbol->string resp)].
 An exception is raised if @racket[resp] does not identify such an element
 in Digital @Ricoeur's TEI XML document for the @tech{instance}
 represented by @racket[info].

 This is a fairly low-level function:
 for most purposes, @racket[segment-resp-string] is preferred.
}


@section{Instance Sets}
@defproc[(instance-set? [v any/c]) any/c]{
 Recognizes @deftech{instance set} values.

 An @tech{instance set} is an immutable set,
 in the sense of @racketmodname[racket/set],
 of values which support the @tech{instance info} interface
 (i.e. values for which @racket[instance-info?] returns @racket[#true]).
 All members of an instance set will be distinct in terms
 of @racket[instance-title/symbol].
}

@defproc[(instance-set [init (stream/c instance-info?) '()])
         (instance-set/c)]{
 Constructs an @tech{instance set} containing the
 @tech{instance info} values from @racket[init].
}

@defproc[(instance-set* [info instance-info?] ...)
         (instance-set/c)]{
 Equivalent to @racket[(instance-set (list info ...))].
}

@deftogether[
 (@defform[(for/instance-set (for-clause ...) body-or-break ... body)]
   @defform[(for*/instance-set (for-clause ...) body-or-break ... body)])]{
 Like @racket[for/list] and @racket[for*/list], respectively,
 except that the last @racket[body] expression must produce an
 @tech{instance info} value, and the result of the iteration
 is an @tech{instance set} of the @tech{instance info} values.
}

@defproc[(instance-set->plain [st (instance-set/c)])
         (instance-set/c)]{
 Produces an @tech{instance set} like @racket[st],
 but with every member of the set converted to
 a @tech{plain instance info} value using @racket[get-plain-instance-info].
 If @racket[st] already contains only @tech{plain instance info} values,
 it may or may not be returned directly.
}

@defproc[(instance-set-ref [st (instance-set/c)]
                           [title/symbol symbol?])
         instance-info?]{
 Returns the member of @racket[st] for which
 @racket[instance-title/symbol] would return @racket[title/symbol].
 If no such member exists, an exception is raised.
}

@defproc[(instance-set-try-ref [st (instance-set/c)]
                               [title/symbol symbol?])
         (or/c #f instance-info?)]{
 Like @racket[instance-set-ref], but returns @racket[#false]
 when @racket[title/symbol] is not found instead of
 raising an exception.
}

@defproc*[([(instance-set/c [elem/c chaperone-contract?])
            chaperone-contract?]
           [(instance-set/c)
            flat-contract?])]{
 Constructs a contract recognizing @tech{instance sets}
 where the elements of the set satisfy @racket[elem/c].
 If @racket[elem/c] is a flat contract, the result will be a flat
 contract; otherwise, the result will be a chaperone contract.

 The contract produced by @racket[(instance-set/c)] accepts all
 @tech{instance sets}.
 Using @racket[(instance-set/c)] may provide better error reporting
 than using @racket[instance-set?] as a contract, and it may
 be checked more efficiently than @racket[(instance-set/c any/c)]
 or @racket[(instance-set/c instance-info?)],
 but any of those variants would accept the same values.
}

@defproc[(in-instance-set [st (instance-set/c)]) sequence?]{
 Explicitly converts an @tech{instance set} to a sequence for use
 with @racket[for]-like forms.
 An @racket[in-instance-set] application may provide better
 performance when it appears directly in a @racket[for] clause.
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

 An object that is an instance (in the sense of @racketmodname[racket/class])
 of a class extended with @racket[instance-info-mixin]
 can be used as an @tech{instance info} value.
 This is the prefered way for class-based objects
 to support the @tech{instance info} API.

 The class returned by @racket[instance-info-mixin]
 will implement the @racket[instance-info<%>]
 interface, which also makes some of the @tech{instance info}
 functions available as methods for use with @racket[inherit],
 @racket[send], etc.
 Note that all such methods are declared with @racket[public-final].
                                           
 @defconstructor/auto-super[([instance-info instance-info?])]{                  

  The @racket[instance-info] initialization argument determines
  the behavior of @(this-obj) when used as an @tech{instance info} value.
  
  Specifically, @racket[(get-plain-instance-info #,(this-obj))]
  will return the same @tech{plain instance info} value as
  @racket[(get-plain-instance-info instance-info)].
                                                    
}}

@definterface[instance-info<%> ()]{

 Identifies classes returned by @racket[instance-info-mixin].
 Objects which satisfy @racket[(is-a?/c instance-info<%>)] can be used
 as @tech{instance info} values.

 In addition to the methods documented below,
 @racket[instance-info<%>] also includes additional, private methods:
 @racket[instance-info<%>] can only be implemented
 using @racket[instance-info-mixin].
 
 @defmethod[#:mode public-final
            (get-title)
            string-immutable/c]{
  Like @racket[(instance-title #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-title/symbol)
            symbol?]{
  Like @racket[(instance-title/symbol #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-citation)
            string-immutable/c]{
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
            boolean?]{
  Like @racket[(instance-publication-original? #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-language)
            (or/c 'en 'fr 'de)]{
  Like @racket[(instance-language #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-book/article)
            (or/c 'book 'article)]{
  Like @racket[(instance-book/article #,(this-obj))].
 }

 @defmethod[#:mode public-final
            (get-resp-string [resp symbol?])
            string-immutable/c]{
  Like @racket[(instance-get-resp-string #,(this-obj) resp)].
 }
}







