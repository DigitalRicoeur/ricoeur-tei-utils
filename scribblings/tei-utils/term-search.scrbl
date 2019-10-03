#lang scribble/manual

@title{``Term Search'' Tool}
@defmodule[ricoeur/term-search]

@(require "for-manual.rkt")

@deftogether[
 (@defproc[(term-search [term term/c]
                        [#:ricoeur-only? ricoeur-only? any/c #t]
                        [#:languages languages search-languages/c 'any]
                        [#:book/article book/article (or/c 'any 'book 'article) 'any]
                        [#:exact? exact? any/c #f])
           (instance-set/c document-search-results?)]
   @defthing[term/c flat-contract?
             #:value (and/c string-immutable/c #px"[^\\s]")]
   @defthing[search-languages/c flat-contract?
             #:value (or/c 'any language-symbol/c (listof language-symbol/c))])]{
 Searches for @racket[term], an immutable string containing at least one non-whitespace
 character, in the @tech{TEI documents} encapsulated by @racket[(current-corpus)].
 If @racket[(current-corpus)] is not a @racket[term-search-corpus<%>],
 always returns @racket[(instance-set)].
 
 If @racket[languages] is a list of symbols,
 results will only be returned from TEI documents for which
 @racket[instance-language] would have produced one of the
 symbols in the @racket[languages] list.
 If it is a single symbol satisfying @racket[language-symbol/c],
 it is treated like @racket[(list languages)].
 Otherwise, if @racket[languages] is @racket['any] (the default),
 documents in all languages will be searched.
 Use @racket['any] rather than listing all currently-supported
 languages so that, when support for additional languages is added
 to this library, they will be included automatically.
 
 If @racket[book/article] is @racket['book] or @racket['article],
 only results from TEI documents that would have returned the
 same symbol from @racket[instance-book/article] will be returned.
 If @racket[book/article] is @racket['any] (the default),
 all TEI documents will be searched.

 If @racket[ricoeur-only?] is non-false (the default),
 results will only be returned from passages by Paul @|Ricoeur|.
 Otherwise, results from passages by editors @etc will also
 be included.
 See @racket[segment-by-ricoeur?] for more details.

 If @racket[exact?] is @racket[#false] (the default),
 @racket[term-search] will try to find matches for lexical
 variants of @racket[term].
 The precise details of how lexical variants are matched
 are unspecified and depend on the specific @tech{search backend}
 used by the @tech{corpus object}.
 If @racket[exact?] is non-false, lexical variants are ignored
 and only exact matches for @racket[term] are returned.
}

@deftogether[
 (@defthing[search-backend/c contract?
            #:value (lazy+eager-search-backend/c
                     (or/c 'noop 'regexp (postgresql-data-source/c)))]
   @defproc[(lazy+eager-search-backend/c [base/c contract])
            contract?
            #:value (or/c base/c (list/c 'eager base/c))])]{
 The contract @racket[search-backend/c]
 recognizes @deftech{search backends}.

 A @tech{search backend} specifies both the underlying
 search implementation to be used for functions like
 @racket[term-search] and the strategy by which the implementation
 should be initialized.
 The actual initialization is handled either by a @tech{corpus object}
 or by direct use of @racket[initialize-search-backend].

 If the @tech{search backend} is a list beginning with @racket['eager],
 the search implementation is initialized synchronously,
 which is especially useful for debugging.
 Otherwise, the search implementation is initialized in
 a background thread, which can provide a substantial improvement
 in startup time.

 The @racket[base/c] portion of the @tech{search backend} value
 determines what underlying search implementation is used:
 @itemlist[
 @item{@racket['noop] indicates a trivial implementation
   that never returns any search results.
  }
 @item{@racket['regexp] specifies a simplistic regular-expression-based
   search implemented in pure Racket, with no system-level dependencies.
   The performance of @racket['regexp]-based @tech{search backends}
   is extremely slow for large corpora.
   Even for development, using @racket['regexp] with Digital @Ricoeur's
   full set of digitized documents is not viable.
  }
 @item{A value satisfying @racket[(postgresql-data-source/c)]
   indicates a production-quality implementation using PostgreSQL’s
   full-text search feature by connecting to the given database.
   @bold{Note that initializing a PostgreSQL
    search backend will perform destructive modifications
    to the database.}
   The specified database should be dedicated completely to use by the 
   constructed @tech{corpus object} or @tech{searchable document set}:
   it should not be relied upon for other purposes, and multiple
   @tech{corpus objects} or @tech{searchable document sets}
   should not use the same database at the same time.
   }]
}

@defproc[(postgresql-data-source/c) contract?]{
 Returns an impersonator contract recognizing values
 created by @racket[postgresql-data-source] with
 sufficient arguments to be used with @racket[dsn-connect]
 without needing to supply any additional arguments.
 That is, at least the @racket[#:user] and @racket[#:database]
 arguments are required.

 Values satisfying @racket[(postgresql-data-source/c)] can
 be used as @tech{search backends}.

 Aside from checking that the @racket[data-source] value
 is well-formed and contains sufficient arguments,
 the primary purpose of @racket[(postgresql-data-source/c)]
 is to prevent mutation.
 Mutators like @racket[set-data-source-args!] raise exceptions
 when applied to values protected by @racket[(postgresql-data-source/c)].
 In addition, the first time @racket[(postgresql-data-source/c)]
 encounters a given @racket[data-source] value, the contract
 copies it (to prevent it from being mutated through another reference)
 and coerces any strings in the
 @racket[data-source-args] field to immutable strings.
 Therefore, values protected by @racket[(postgresql-data-source/c)]
 may not be @racket[eq?] or even @racket[equal?] to their originals.
}


@defproc[(corpus-do-term-search [corpus (is-a?/c term-search-corpus<%>)]
                                [term term/c]
                                [#:ricoeur-only? ricoeur-only? any/c #t]
                                [#:languages languages search-languages/c 'any]
                                [#:book/article book/article (or/c 'any 'book 'article) 'any]
                                [#:exact? exact? any/c #f])
         (instance-set/c document-search-results?)]{
 Like @racket[term-search],  but using @racket[corpus] instead of @racket[(current-corpus)].
}


@defmixin[term-search-corpus-mixin [corpus<%>] [term-search-corpus<%>]]{

 ...
                                                                       
 @defconstructor/auto-super[([search-backend search-backend/c '(eager noop)])]{
  Constructs a @tech{corpus object} encapsulating @racket[docs].

  The @racket[search-backend] argument is used as the
  corpus object's @tech{search backend} and affects the behavior
  of @method[term-search-corpus-mixin term-search].
  See @racket[search-backend/c] for more details.
 }
 @defmethod[#:mode public-final
            (term-search [term term/c]
                         [#:ricoeur-only? ricoeur-only? any/c #t]
                         [#:languages languages search-languages/c 'any]
                         [#:book/article book/article (or/c 'any 'book 'article) 'any]
                         [#:exact? exact? any/c #f])
            (instance-set/c document-search-results?)]{
  Implements @racket[term-search] and
  @racket[corpus-do-term-search].
 }
}

@definterface[term-search-corpus<%> (corpus<%>)]{
 ...
}

@section{Search Results}
@deftogether[
 (@defpredicate[document-search-results?]
   @defproc[(document-search-results-count [doc-results document-search-results?])
            exact-positive-integer?]
   @defproc[(document-search-results-results [doc-results document-search-results?])
            (non-empty-listof search-result?)]
   @defform[#:kind "match expander"
            (document-search-results kw-pat ...)
            #:grammar [(kw-pat (code:line #:count count-pat)
                               (code:line #:results results-pat))]])]{
 A @deftech{document search results} value, recognized by
 the predicate @racket[document-search-results?], encapsulates
 the results of a function like @racket[term-search]
 from a single @tech{TEI document}.
 Document search results values also serve as @tech{instance info}
 values for bibliographic information.

 A @tech{document search result} value will always contain at
 least one @tech{search result}.

 The function @racket[document-search-results-count] is equivalent to
 @racket[(compose1 length document-search-results-results)],
 but @racket[document-search-results-count] (and the corresponding
 @racket[match] pattern with @racket[document-search-results]) is
 cached for efficiency of repeated calls
}

@deftogether[
 (@defpredicate[search-result?]
   @defproc[(search-result-excerpt [search-result search-result?])
            (maybe/c (and/c string-immutable/c
                            trimmed-string-px))]
   @defform[#:kind "match expander"
            (search-result excerpt-pat)])]{
 A @deftech{search result} value, recognized by @racket[search-result?],
 represents an individual match from a function like @racket[term-search].
 Search result values are also @tech{segments}, though
 a given @tech{document search result} may contain multiple
 @tech{search results} that are the same according to
 @racket[segment-meta=?] if there was more than one match for
 the search term within a single @tech{segment}.

 A search result's excerpt may be @racket[(nothing)] if
 there were too many results for the search term from that
 @tech{TEI document} to return excerpts for all of them.

 The @racket[trimmed-string-px] part of the contract on the result
 of @racket[search-result-excerpt] guaranties that, 
 if the returned excerpt is not @racket[(nothing)],
 the contained string will be non-empty and
 will neither start nor end with whitespace.

 See also @racket[search-result-<?] and @racket[search-result->?].
}

@deftogether[
 (@defproc[(search-result-<? [a search-result?] [b search-result?])
           boolean?]
   @defproc[(search-result->? [a search-result?] [b search-result?])
            boolean?])]{
 Ordering functions on @tech{search results}, such as might be useful
 with @racket[sort].
 These are more fine-grained than functions based on
 @racket[segment-order] would be: if @racket[a] and @racket[b] are
 from the same @tech{segment} according to @racket[segment-meta=?],
 these functions will sort them according to their relative position
 within the segment.

 Sorting @tech{search results} with @racket[search-result-<?] will
 put them in the order in which they appeared in the original
 @tech{TEI document}.

 It is an error to use @racket[search-result-<?] or
 @racket[search-result->?] on @tech{search results} that did not
 come from the same @tech{document search result} value.
}

@section{Searching Without a Corpus Object}
@deftogether[
 (@defproc[(initialize-search-backend [backend search-backend/c]
                                      [docs (instance-set/c tei-document?)])
           searchable-document-set?]
   @defpredicate[searchable-document-set?])]{
 While @tech{corpus objects} are generally the preferred way to
 use this library's search functions, searching @tech{TEI documents}
 without a corpus object is possible by creating lower-level
 @deftech{searchable document sets} directly.

 A @tech{searchable document set} is recognized by the predicate
 @racket[searchable-document-set?] and can be created using
 @racket[initialize-search-backend], which takes a
 @tech{search backend}, just like @racket[corpus%], and an
 @tech{instance set} of @tech{TEI documents} to be searched.
 (In fact, @racket[corpus%] implements @method[term-search-corpus-mixin term-search]
 by creating a @tech{searchable document set} internally.)

 As with creating an instance of @racket[corpus%],
 creating a new @tech{searchable document set} with
 @racket[initialize-search-backend] involves an appreciable
 amount of overhead, so creating redundant values should
 be avoided.
}

@defthing[noop-searchable-document-set searchable-document-set?]{
 A trivial @tech{searchable document set} which never returns any
 results. Calling @racket[initialize-search-backend] with a
 @tech{search backend} of @racket['(eager noop)] always
 returns @racket[noop-searchable-document-set].
}

@defproc[(searchable-document-set-do-term-search
          [searchable-document-set searchable-document-set?]
          [term term/c]
          [#:ricoeur-only? ricoeur-only? any/c #t]
          [#:languages languages search-languages/c 'any]
          [#:book/article book/article (or/c 'any 'book 'article) 'any]
          [#:exact? exact? any/c #f])
         (instance-set/c document-search-results?)]{
 Like @racket[corpus-do-term-search], but uses the
 @tech{searchable document set} @racket[searchable-document-set].
}
