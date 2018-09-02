#lang scribble/manual

@title{High-level Corpus Functionality}
@(declare-exporting ricoeur/tei)

@(require "for-manual.rkt")

The bindings documented in this section are provided
by @racketmodname[ricoeur/tei], but @bold{not}
by @racketmodname[ricoeur/tei/base].

Many applications work with entire collections
of @tech{TEI documents} at least as often as with
individual documents.
This library provides @deftech{corpus objects}
(instances of @racket[corpus%] or a subclass)
to bundle collections of TEI documents with
related functionality.
Corpus objects provide a particularly convienient
way to use this library's search functions, like
@racket[term-search], though both parts of the
library can be used independently.

@section{Working with Corpus Objects}
@defparam[current-corpus corpus
          (is-a?/c corpus%)
          #:value empty-corpus]{
 Contains a @tech{corpus object} for use by high-level functions
 like @racket[get-instance-info-set], @racket[get-checksum-table],
 and @racket[term-search].

 In practice, this parameter should usually be initialized
 with a @racket[directory-corpus%] instance.
}

@defproc[(get-instance-info-set)
         (instance-set/c)]{
 Returns an @tech{instance set} containing an @tech{instance info}
 value for each @tech{TEI document} encapsulated by
 @racket[(current-corpus)].

 Note that the returned @tech{instance set} @bold{does not}
 contain the TEI document values with which the corpus
 was created.
 @tech{Corpus objects} generally avoid retaining their
 encapsulated TEI document values after initialization.
 Currently, the result of @racket[(get-instance-info-set)]
 always satisfies @racket[(instance-set/c plain-instance-info?)],
 but that is not guaranteed to be true in future versions
 of this library.
}

@defproc[(get-checksum-table)
         (hash/c symbol?
                 symbol?
                 #:immutable #t)]{
 Returns an immutable hash table summarizing the identity of the
 @tech{TEI documents} encapsulated by @racket[(current-corpus)].

 For each TEI document @racket[doc], the returned hash table
 will have a key of @racket[(instance-title/symbol doc)]
 mapped to the value @racket[(tei-document-checksum doc)].
 Thus, any two @tech{corpus objects} that return @racket[equal?]
 hash tables, even across runs of the program, are guaranteed
 to encapsulate the very same TEI documents.
}

@deftogether[
 (@defproc[(term-search [term term/c]
                        [#:ricoeur-only? ricoeur-only? any/c #t]
                        [#:book/article book/article (or/c 'any 'book 'article) 'any]
                        [#:exact? exact? any/c #f])
           (instance-set/c document-search-results?)]
   @defthing[term/c flat-contract?
             #:value (and/c string-immutable/c #px"[^\\s]")])]{
 Searches for @racket[term], an immutable string containing at least one non-whitespace
 character, in the @tech{TEI documents} encapsulated by @racket[(current-corpus)].

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
 (@defproc[(corpus-get-instance-info-set [corpus (is-a?/c corpus%)])
           (instance-set/c)]
   @defproc[(corpus-get-checksum-table [corpus (is-a?/c corpus%)])
            (hash/c symbol?
                    symbol?
                    #:immutable #t)]
   @defproc[(corpus-do-term-search [corpus (is-a?/c corpus%)]
                                   [term term/c]
                                   [#:ricoeur-only? ricoeur-only? any/c #t]
                                   [#:book/article book/article (or/c 'any 'book 'article) 'any]
                                   [#:exact? exact? any/c #f])
            (instance-set/c document-search-results?)])]{
 Like @racket[get-instance-info-set], @racket[get-checksum-table],
 and @racket[term-search], respectively,
 but using @racket[corpus] instead of @racket[(current-corpus)].
}


@section{Creating Corpus Objects}
@defclass[corpus% object% ()]{
 A @tech{corpus object} is an instance of @racket[corpus%]
 or of a subclass of @racket[corpus%].

 For many purposes, @racket[directory-corpus%] offers more
 convienient initialization than @racket[corpus%].

 Note that creating a new instance of @racket[corpus%]
 involves a fair amount of overhead,
 so creating redundant values should be avoided.
 Reusing @tech{corpus objects} may also improve 
 search performance through caching, for example.
 
 @defconstructor[([docs (instance-set/c tei-document?) (instance-set)]
                  [search-backend search-backend/c '(eager noop)])]{
  Constructs a @tech{corpus object} encapsulating @racket[docs].

  The @racket[search-backend] argument is used as the
  corpus object's @tech{search backend} and affects the behavior
  of @method[corpus% term-search].
  See @racket[search-backend/c] for more details.
 }
 @defmethod[#:mode public-final
            (get-instance-info-set)
            (instance-set/c)]{
  Implements @racket[get-instance-info-set] and
  @racket[corpus-get-instance-info-set].
 }
 @defmethod[#:mode public-final
            (get-checksum-table)
            (hash/c symbol?
                    symbol?
                    #:immutable #t)]{
  Implements @racket[get-checksum-table] and
  @racket[corpus-get-checksum-table].
 }
 @defmethod[#:mode public-final
            (term-search [term term/c]
                         [#:ricoeur-only? ricoeur-only? any/c #t]
                         [#:book/article book/article (or/c 'any 'book 'article) 'any]
                         [#:exact? exact? any/c #f])
            (instance-set/c document-search-results?)]{
  Implements @racket[term-search] and
  @racket[corpus-do-term-search].
 }
}

@defthing[empty-corpus (is-a?/c corpus%)]{
 An empty @tech{corpus object} used as the default value of
 the @racket[current-corpus] parameter.

 With @racket[empty-corpus], @method[corpus% get-instance-info-set]
 always returns @racket[(instance-set)],
 @method[corpus% get-checksum-table] always returns @racket[#hasheq()],
 and @method[corpus% term-search] always returns @racket[(instance-set)].
}

@defthing[search-backend/c contract?
          #:value (let ([base/c (or/c 'noop 'regexp (postgresql-data-source/c))])
                    (or/c base/c
                          (list/c 'eager base/c)))]{
 A contract recognizing @deftech{search backends}.

 A @tech{search backend} specifies both the underlying
 search implementation to be used for functions like
 @racket[term-search] and the strategy by which the implementation
 should be initialized, either by a @tech{corpus object}
 or by a direct use of @racket[initialize-search-backend].

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

@defclass[directory-corpus% corpus% ()]{
 Extends @racket[corpus%] for the common case of using 
 @tech{TEI documents} from some directory in the filesystem.
 
 @defconstructor[([path (and/c path-string-immutable/c
                               directory-exists?)]
                  [search-backend search-backend/c '(eager noop)])]{
  Constructs a @tech{corpus object} from every file in @racket[path],
  including recursive subdirectories, that is recognized by
  @racket[xml-path?].
  If any such file is not a valid and well-formed TEI XML file
  satisfying Digital Ricœur’s specification,
  it will be silently ignored.
  If more than one of the resulting @tech{TEI document} values
  correspond to the same @tech{instance},
  one will be chosen in an unspecified manner and the others
  will be silently ignored.

  If @racket[path] is a relative path, it is resolved relative to
  @racket[(current-directory)].

  The @racket[search-backend] argument determines the
  @tech{search backend} as with @racket[corpus%].
 }
}

@section{Deriving New Corpus Classes}
@defproc[(make-corpus-mixin [key member-name-key?])
         (let ()
           (define-member-name initialize-this key)
           (and/c (make-mixin-contract corpus%)
                  (-> (class/c
                       (absent initialize-this))
                      (class/c
                       (override
                         [initialize-this
                          (->m (instance-set/c tei-document?) any)])))))]{
 Clients of this library may want to extend the @tech{corpus object}
 system to support additional features by implementing
 new classes derived from @racket[corpus%].
 Many such extensions will need access to the full @tech{instance set}
 of @tech{TEI documents} to be encapsulated by the @tech{corpus object}
 for use during the initialization of their derived classes.
 However, @racket[corpus%] does not keep its @tech{TEI documents}
 reachable after its initialization, and derived classes are
 encouraged to follow this practice, as @tech{TEI document} values
 can be large.

 The function @racket[make-corpus-mixin] gives clients of this
 library a hook to access the @tech{TEI documents} during
 initialization without making them reachable afterwords.
 It creates a helper mixin that adds an @racket[abstract]
 implementation of the method specified by @racket[key]
 (which would generally be created with @racket[(generate-member-key)])
 to its base class. The resulting mixin arranges with
 the implementation of @racket[corpus%] to have the new method
 called exactly once, during @racket[corpus%]'s initialization,
 with the full @tech{instance set} of @tech{TEI documents} as
 its argument.

 @examples[
 #:eval (make-tei-eval) #:once
 (define initialize-this-method-key (generate-member-key))
 (define-member-name initialize-this initialize-this-method-key)
 (define printing-corpus%
   (class ((make-corpus-mixin initialize-this-method-key)
           corpus%)
     (super-new)
     (define/override-final (initialize-this docs)
       (printf "These are the docs!\n  ~v\n"
               (set->list docs)))))
 (new printing-corpus%)
 ]}




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
 (In fact, @racket[corpus%] implements @method[corpus% term-search]
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
          [#:book/article book/article (or/c 'any 'book 'article) 'any]
          [#:exact? exact? any/c #f])
         (instance-set/c document-search-results?)]{
 Like @racket[corpus-do-term-search], but uses the
 @tech{searchable document set} @racket[searchable-document-set].
}





