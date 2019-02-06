#lang scribble/manual

@title{Search Implementation}
@defmodule[(submod ricoeur/tei/search/common private)]

@(require "../for-manual.rkt"
          (for-label (submod ricoeur/tei/search/common private)
                     (only-in ricoeur/tei/search/noop noop@)
                     ricoeur/tei/search/regexp
                     ricoeur/tei/search/postgresql
                     ))

This section documents the common utilities used to
implement @racketmodname[ricoeur/tei]'s search feature
(which is used through functions like @racket[term-search]),
including everything necessary to implement new kinds of
@tech{search backends}.

@defproc[(tei-document->excerpt-max-allow-chars [doc tei-document?])
         exact-positive-integer?]{
 Returns the maximum total number of characters allowed in
 @tech{search result} excerpts in a @tech{document search result}
 value for the @tech{TEI document} @racket[doc].
 This maximum applies per search query; it is defined 
 internally as a percentage of the total length of @racket[doc].

 The result of @racket[tei-document->excerpt-max-allow-chars] is
 cached to amortize the cost of calling it multiple
 times on the same @tech{TEI document}.
}

@definterface[searchable-document-set<%> ()]{
 Internally, a @tech{searchable document set} is an
 object (in the sense of @racketmodname[racket/class])
 that satisfies @racket[(is-a?/c searchable-document-set<%>)].

 Adding a new type of @tech{search backend} typically means
 defining a new class that implements this interface.
 
 @defmethod[(do-term-search [norm-term normalized-term?]
                            [#:ricoeur-only? ricoeur-only? any/c]
                            [#:languages languages (set/c language-symbol/c #:cmp 'eq #:kind 'immutable)]
                            [#:book/article book/article (or/c 'any 'book 'article)]
                            [#:exact? exact? any/c])
            (instance-set/c document-search-results?)]{
  The method used to implement @racket[searchable-document-set-do-term-search].

  There are a few notable differences between
  @method[searchable-document-set<%> do-term-search] and all of
  the higher-level search functions or methods:
  @itemlist[
 #:style 'ordered
 @item{All of the keyword arguments are mandatory.
    As there are several different classes that implement
    @racket[searchable-document-set<%>], copying the default
    values correctly to every definition would be unpleasant
    and error-prone.
   }
 @item{The search term is passed as a @tech{normalized term}
    value, rather than a string satisfying @racket[term/c].
    This prevents @method[searchable-document-set<%> do-term-search]
    from being called except by
    @racket[searchable-document-set-do-term-search],
    which allows the implementation of
    @racket[searchable-document-set-do-term-search]
    to rely on the fact that it will be able to interpose on calls.
    In fact, the implementation does do some normalization
    when constructing a @tech{normalized term} value,
    and it can guarantee that it will always have the
    chance to do so.
   }
 @item{The @racket[languages] argument is normalized:
    rather than being passed as a @racket[search-languages/c]
    value, which is designed for the convienience of clients,
    it is given as an immutable set of @racket[language-symbol/c]
    symbols.
    This allows @racket[searchable-document-set-do-term-search]
    to take sole responsibility for handling @racket['any]
    and lists with duplicate symbols,
    rather than placing that burden on every class that implements
    @racket[searchable-document-set<%>].
    }]}}
 
@deftogether[
 (@defpredicate[normalized-term?]
   @defproc[(normalized-term-string [norm-term normalized-term?])
            (and/c term/c trimmed-string-px)]
   @defproc[(pregexp-quote-normalized-term [norm-term normalized-term?]
                                           [#:exact? exact? any/c])
            string-immutable/c])]{
 A @deftech{normalized term} value, recognized by the predicate
 @racket[normalized-term?], is used by
 @racket[searchable-document-set-do-term-search] to wrap the
 search term it passes to the @method[searchable-document-set<%> do-term-search]
 method of @racket[searchable-document-set<%>].
 The function @racket[normalized-term-string] extracts the
 string from a @tech{normalized term}.

 The function @racket[pregexp-quote-normalized-term] produces
 a string suitable to be passed to @racket[pregexp] to
 construct a regular expression recognizing the encapsulated term.
 (Some backend implementations combine the resulting string with
 additional regular expression syntax.)
 When @racket[exact?] is non-false, the resulting string
 will produce a regular expression that will match only
 exact occurances of the term delimited by a word boundry.
 (The precise definition of a word boundry is unspecified
 and specific to @racket[pregexp-quote-normalized-term].)
 
 Because the constructor for @tech{normalized term} values
 is not exported, the wrapper can serve as a guarantee of some
 invariants: for example, that the argument to
 @racket[pregexp-quote-normalized-term] will always have been normalized.
 This is particularly important as certain properties of search
 strings can have security implications, especially with
 less sophisticated backends.
}

@section{Constructing Search Results} 
@defproc[(segment-make-search-results [seg segment?]
                                      [excerpts
                                       (listof (maybe/c (and/c string-immutable/c
                                                               #px"[^\\s]")))])
         (listof search-result?)]{
 Returns a @tech{search result} value for each element of
 the @racket[excerpts] list.
 The excerpts should be given in the order in which they
 occur within the @tech{segment}.
}

@defproc[(search-result-nullify-excerpt [result search-result?])
         search-result?]{
 Returns a @tech{search result} like @racket[result],
 but which will return @racket[(nothing)] as its
 @racket[search-result-excerpt].
}

@defproc[(make-document-search-results [info instance-info?]
                                       [results (non-empty-listof search-result?)])
         document-search-results?]{
 Constructs a @tech{document search results} value encapsulating
 the @racket[results].

 All of the @racket[results] must be from the same @tech{TEI document}
 and must be consistent with the @tech{instance info} value @racket[info].
 Otherwise, an exception is raised.
}

@section{Implementing Search Backend Types}

@;subsection{@racket[search^] Signature}
@defsignature[search^ ()]{
 @signature-desc{
  Adding support for a new type of @tech{search backend}
  means defining a @racket[unit] that exports @racket[search^].
  The units for each basic type of @tech{search backend}
  are then knit together using @racket[define-compound-search-unit]
  and @racket[define-lazy-search-unit] to create a composite
  unit which binds @racket[search-backend/c] and
  @racket[initialize-search-backend]
  via @racket[define-values/invoke-unit/infer].
 }
 @defthing[search-backend/c contract?]{
  A @racket[search^] unit should define @(sigelem search^ search-backend/c)
  as a contract recognizing the new type of @tech{search backend}
  value it wants to support.

  A @racket[search^] unit's @tech{search backend} implementation
  need only provide a basic contract and initialize it eagerly in
  @(sigelem search^ initialize-search-backend).
  The additional variants permitted by the final,
  public @racket[search-backend/c] (see @racket[lazy+eager-search-backend/c])
  are added using @racket[define-lazy-search-unit].
 }
 @defproc[(initialize-search-backend [backend #,(sigelem search^ search-backend/c)]
                                     [docs (instance-set/c tei-document?)])
          searchable-document-set?]{
  The @racket[search^] unit's @(sigelem search^ initialize-search-backend)
  will be called with a @racket[backend] @tech{search backend} value
  satisfying the unit's specific definiton of @(sigelem search^ search-backend/c).
  The unit's implementation of @(sigelem search^ initialize-search-backend)
  is responsible for returning a @tech{searchable document set}:
  that is, an instance of a class that implements @racket[searchable-document-set<%>].

  Typically, @(sigelem search^ initialize-search-backend) will be
  a wrapper around a constructor for a unit-specific
  @racket[searchable-document-set<%>] class, and the unit's
  notion of a @(sigelem search^ search-backend/c) value will 
  to encapsulate all of the other data needed to initialize the class.

  However, this is not mandatory.
  The implementation of @(sigelem search^ initialize-search-backend)
  from @racket[noop@], for example, ignores its arguments and
  always returns the singleton object @racket[noop-searchable-document-set].
 }
 @defthing[initialize-search-backend/c contract?
           #:value (-> #,(sigelem search^ search-backend/c)
                       (instance-set/c tei-document?)
                       searchable-document-set?)]{
  The @racket[search^] signature uses @racket[define-values-for-export]
  to define @(sigelem search^ initialize-search-backend/c)
  as the contract for that unit's implementation of
  @(sigelem search^ initialize-search-backend).
}}

@;subsection{Using @racket[search^] Units}

@defform[(define-compound-search-unit compound-search-unit-id
           member-search-unit-id ...+)]{
 Defines @racket[compound-search-unit-id] as a unit exporting
 the signature @racket[search^].
 
 The new unit's implementation of @(sigelem search^ search-backend/c)
 applies @racket[or/c] to the implementations from each
 of the @racket[member-search-unit-id] units.
 Likewise, the new unit's implementation of @(sigelem search^ initialize-search-backend)
 inspects the given @tech{search backend} value and dispatches to
 the implementation of @(sigelem search^ initialize-search-backend)
 from the coresponding @racket[member-search-unit-id] unit.
}

@defform[(define-lazy-search-unit lazy-search-unit-id
           eager-search-unit-id)]{
 Defines @racket[lazy-search-unit-id] as a unit exporting
 the signature @racket[search^].
 The new @racket[lazy-search-unit-id] will define
 @(sigelem search^ search-backend/c)
 as @racket[(lazy+eager-search-backend/c base/c)],
 where @racket[base/c] is the @(sigelem search^ search-backend/c)
 implemented by @racket[eager-search-unit-id].

 In the @racket['eager] case, @racket[lazy-search-unit-id] will
 simply dispatch to @racket[eager-search-unit-id]'s implementation
 of @(sigelem search^ initialize-search-backend).
 Otherwise, @racket[lazy-search-unit-id] will return a proxy
 @tech{searchable document set} which calls
 @racket[eager-search-unit-id]'s @(sigelem search^ initialize-search-backend)
 in a background thread.
}

@subsection{Basic @racket[search^] Units}
@subsubsection[#:style '(hidden toc-hidden)]{Noop Search Backend}
@defmodule[ricoeur/tei/search/noop]
@defthing[noop@
          (unit/c (import)
                  (export search^))]

@subsubsection[#:style '(hidden toc-hidden)]{Regexp Search Backend}
@defmodule[ricoeur/tei/search/regexp]
@defthing[regexp@
          (unit/c (import)
                  (export search^))]

@subsubsection[#:style '(hidden toc-hidden)]{PostgreSQL Search Backend}
@defmodule[ricoeur/tei/search/postgresql]
@defthing[postgresql@
          (unit/c (import)
                  (export search^))]
