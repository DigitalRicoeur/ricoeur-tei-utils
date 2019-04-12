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

@deftogether[(@defproc[(get-checksum-table)
                       checksum-table/c]
               @defthing[checksum-table/c flat-contract?
                         #:value (hash/c symbol?
                                         symbol?
                                         #:immutable #t)])]{
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
 (@defproc[(corpus-get-instance-info-set [corpus (is-a?/c corpus%)])
           (instance-set/c)]
   @defproc[(corpus-get-checksum-table [corpus (is-a?/c corpus%)])
            checksum-table/c]
   @defproc[(corpus-do-term-search [corpus (is-a?/c corpus%)]
                                   [term term/c]
                                   [#:ricoeur-only? ricoeur-only? any/c #t]
                                   [#:languages languages search-languages/c 'any]
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
            checksum-table/c]{
  Implements @racket[get-checksum-table] and
  @racket[corpus-get-checksum-table].
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

@defthing[empty-corpus (is-a?/c corpus%)]{
 An empty @tech{corpus object} used as the default value of
 the @racket[current-corpus] parameter.

 With @racket[empty-corpus], @method[corpus% get-instance-info-set]
 always returns @racket[(instance-set)],
 @method[corpus% get-checksum-table] always returns @racket[#hasheq()],
 and @method[corpus% term-search] always returns @racket[(instance-set)].
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
@defform[(corpus-mixin [from<%> ...] [to<%> ...]
           class-clause ...+)
         #:contracts ([from<%> interface?]
                      [to<%> interface?])]{
 Clients of this library will want to extend the @tech{corpus object}
 system to support additional features by implementing
 new classes derived from @racket[corpus%].
 There are two main points where derived classes will want to interpose
 on @racket[corpus%]'s initialization:
 @(itemlist #:style 'ordered
            @item{A few classes, like @racket[directory-corpus%],
  will want to supply an alternate means of constructing
  the full @tech{instance set} of @tech{TEI documents}
  to be encapsulated by the @tech{corpus object}.
  This is easily done using standard features of the @racketmodname[racket/class]
  object system, such as @racket[init] and @racket[super-new], to control the
  initialization of the base class.
 }
            @item{More often, derived classes will want to use the complete
  @tech{instance set} of @tech{TEI documents} to initialize some extended functionality:
  for example, @racket[corpus%] itself extends a primitive, unexported class this way
  to initialize a @tech{searchable document set}.
  The @racketmodname[ricoeur/tei] library provides special support
  for these kinds of extensions.
  })

 A key design consideration is that @racket[corpus%] instance does
 not keep its @tech{TEI documents} reachable after its initialization,
 as @tech{TEI document} values can be rather large.
 Derived classes are urged to follow this practice:
 they should initialize whatever state they need for their extended functionality,
 but they should allow the @tech{TEI documents} to be garbage-collected
 as soon as possible.

 Concretely, this means that @racket[corpus%] does not store
 the @tech{instance set} of @tech{TEI documents}
 in a @seclink["clfields" #:doc '(lib "scribblings/reference/reference.scrbl")]{field}
 (neither public nor private), as objects' fields are reachable after initialization. 

 The @racket[corpus-mixin] form is like @racket[mixin], but it cooperates
 with the @racket[super-docs] and @racket[super-docs-evt] forms
 to provide access to the @tech{instance set} of @tech{TEI documents} as
 a ``virtual'' initialization variable.
 (The @racket[corpus<%>] interface is implicitly included among
 @racket[corpus-mixin]'s @racket[from<%>] interfaces.)

 @defsubform[(super-docs)]{
  Within a @racket[corpus-mixin] form, evaluates to the full @tech{instance set}
  of @tech{TEI documents} to be encapsulated by the @tech{corpus object} as a
  ``virtual'' @seclink["clinitvars" #:doc '(lib "scribblings/reference/reference.scrbl")]{
   initialization variable}: using @racket[(super-docs)] anywhere that an
  initialization variable is not allowed is a syntax error.
  
  The @tech{instance set} of @tech{TEI documents} is created by the @racket[corpus%]
  constructor: evluating @racket[(super-docs)] before the superclass constructor
  has been called (e.g. via @racket[super-new]) will raise an exception,
  analagous to accessing an uninitialized field.
 }
 @defsubform[(super-docs-evt)]{
  Within a @racket[corpus-mixin] form, similar to @racket[super-docs],
  but produces a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
   synchronizable event} which produces the @tech{instance set}
  of @tech{TEI documents} as its @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
   synchronization result}.

  Unlike @racket[(super-docs)], @racket[(super-docs-evt)] may be evaluated before
  the superclass constuctor is called and may immediately be used with @racket[sync]
  in a background thread (e.g. via @racket[delay/thread]).
  The event will become @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
   ready for synchronization} when the @racket[corpus%] constructor is called.
  Note that @racket[(begin (sync (super-docs-evt)) (super-new))] will block forever.

  The events produced by @racket[(super-docs-evt)] can be recognzed by
  the predicate @racket[super-docs-evt?] and satisfy the contract
  @racket[(evt/c (instance-set/c tei-document?))].
 }

 @examples[
 #:eval (make-tei-eval) #:once
 (define printing-corpus-mixin
   (corpus-mixin [] []
     (super-new)
     (printf "These are the docs!\n  ~v\n"
             (set->list (super-docs)))))
 (new (printing-corpus-mixin corpus%))
 ]}

@defproc[(super-docs-evt? [v any/c]) any/c]{
 Recognizes values produced by @racket[super-docs-evt].
}

@defthing[corpus<%> interface?]{
 Equivalent to @racket[(class->interface corpus%)].
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
          [#:languages languages search-languages/c 'any]
          [#:book/article book/article (or/c 'any 'book 'article) 'any]
          [#:exact? exact? any/c #f])
         (instance-set/c document-search-results?)]{
 Like @racket[corpus-do-term-search], but uses the
 @tech{searchable document set} @racket[searchable-document-set].
}





