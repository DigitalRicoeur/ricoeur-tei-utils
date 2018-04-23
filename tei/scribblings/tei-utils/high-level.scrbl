#lang scribble/manual

@title{High-level Interface}
@defmodule[ricoeur/tei #:link-target? #f]

@(require (for-label (except-in racket
                                date
                                date?)
                     racket/unit
                     xml
                     data/maybe
                     ricoeur/tei
                     ricoeur/tei/xexpr/signatures
                     ricoeur/lib
                     db
                     json
                     (only-in ricoeur/tei/search
                              search-documents
                              searchable-document-set?
                              regexp-searchable-document-set
                              postgresql-searchable-document-set)
                     (submod ricoeur/tei/search/common private)
                     setup/matching-platform
                     gregor
                     ))



The bindings documented in this section are provided by
@racketmodname[ricoeur/tei], but not @racketmodname[ricoeur/tei/base].

@defparam[current-corpus corpus (is-a?/c corpus%)
          #:value empty-corpus]{
 This library uses @deftech{corpus} objects to encapsulate
 collections of TEI documents. High-level functions, such
 as @racket[term-search] and @racket[list-TEI-info],
 invoke methods of the object in
 the @racket[current-corpus] parameter.

 In practice, this parameter should usually be initialized
 with a @racket[directory-corpus%] instance.
}

@defproc[(list-TEI-info) (listof (is-a?/c TEI-info<%>))]{
 Returns a list of @racket[TEI-info<%>] objects for all of the
 documents in the @racket[current-corpus].
}

@defproc[(get-checksum-table) (hash/c string?
                                      string?
                                      #:immutable #t)]{
 Returns an immutable hash table with an entry for every document in the
 @racket[current-corpus], where the keys are the titles of
 the documents (as reported by @(method TEI-info<%> get-title))
 and the values are their md5 checksums (as reported by @(method TEI<%> get-md5)).
}

@defclass[corpus% object% ()]{
 The root class for all @tech{corpus} objects.

 Note that creating a new instance of this class involves a
 fair amount of overhead, so creating redundant values should be avoided.
 (This also improves search performance through caching, for example.)
 
 @defconstructor[([docs (listof (is-a?/c TEI<%>))]
                  [search-backend (or/c #f 'noop postgresql-data-source/c) #f])]{
  Constructs a @tech{corpus} object encapsulating the TEI documents @racket[docs]
  which uses the @tech{search backend} specified by @racket[search-backend]
  to implement @racket[term-search].

  The @racket[docs] should all have distinct titles (in the sense of
  @(method TEI-info<%> get-title)).
  Duplicates will be ignored in an unspecified manner.

  Currently, three types of @deftech{search backend} are supported:
  @itemlist[@item{
               A value of @racket[#f] specifies using a simplistic
               regular-expression-based backend implemented in pure
               Racket.}
            @item{
               Using @racket['noop] as a backend will cause
               @(method corpus% •term-search) to always return 
               @racket['()]}
            @item{
               A @racket[postgresql-data-source/c] value will construct a
               backend using PostgreSQL's full-text search feature by
               connecting to the given database. @bold{Note that initializing
                a corpus will perform destructive modifications to the database.}
               The specified database should be dedicated completely to use by
               the constructed @tech{corpus} object: it should not be relied upon for
               other purposes, and multiple @tech{corpus} objects should not
               use the same database at the same time.}]
 }
 @defmethod[#:mode public-final (•term-search [term term/c]
                                              [#:exact? exact? any/c #f]
                                              [#:ricoeur-only? ricoeur-only any/c #t])
            (listof (is-a?/c document-search-results<%>))]{
  Used to implement @racket[term-search].
 }
 @defmethod[#:mode public-final (•list-TEI-info) (listof (is-a?/c TEI-info<%>))]{
  Used to implement @racket[list-TEI-info]
 }
 @defmethod[#:mode public-final (•get-checksum-table) (hash/c string?
                                                              string?
                                                              #:immutable #t)]{
  Used to implement @racket[get-checksum-table]
 }
 @defmethod[#:mode pubment (on-initialize [docs (listof (is-a?/c TEI<%>))]) any]{
  This method is called exactly once, at the end of @racket[corpus%]'s
  portion of @(this-obj)'s initialization.
  Attempting to call @(method corpus% on-initialize) directly will
  raise an exception.
  
  The argument given to @(method corpus% on-initialize) is the list of
  @racket[TEI<%>] objects encapsulated by @(this-obj).
  The implementation of @racket[corpus%] guarantees that each item
  in @racket[docs] will have a unique result for @(method TEI-info<%> get-title).

  The purpose of @(method corpus% on-initialize) is to provide a hook for subclasses
  of @racket[corpus%] to access the full list of @racket[TEI<%>] objects,
  without having @racket[corpus%] itself cause them remain reachable after
  initialization.

  @italic{Default implementation:} Does nothing.
 }
}

@defthing[postgresql-data-source/c contract?]{
 A contract recognizing values created using @racket[postgresql-data-source]
 with sufficient arguments
 (i.e. at least @racket[#:database] and @racket[#:user])
 to be able to be used with @racket[dsn-connect]
 without needing to supply any additional arguments.
}

@defthing[empty-corpus (is-a?/c corpus%)]{
 A corpus containing no documents
}

@defclass[directory-corpus% corpus% ()]{
 Extends @racket[corpus%] for the common case when you want to
 use documents from some directory in the filesystem.

 @defconstructor[([path (and/c path-string? directory-exists?)]
                  [search-backend (or/c #f 'noop postgresql-data-source/c) #f])]{
  Constructs a corpus from every file in @racket[path],
  including recursive subdirectories,
  that is recognized by @racket[xml-path?].
  If any such file is not a valid and well-formed TEI XML file
  satisfying Digital Ricœur's specification, it will be silently ignored.

  If @racket[path] is a relative path, it is resolved relative to
  the current directory.

  The @racket[search-backend] argument controls the @tech{search backend}
  as with @racket[corpus%].
 }
 @defmethod[#:mode public-final (get-path)
            (and/c path-string? absolute-path?)]{
  Returns the path used to instantiate @(this-obj).

  The result may not be @racket[equal?] to the @racket[path] argument
  to the @racket[directory-corpus%] constructor, as the
  given path will have been resolved to an absolute path
  and simplified in the sense of @racket[simplify-path].
 }
}

@;{
 ;                                                  
 ;                                                  
 ;                                                  
 ;                                                  
 ;                                           ;;     
 ;                                           ;;     
 ;     ;;      ;;;     ;;    ;; ;;;     ;;;  ;; ;   
 ;   ;;  ;   ;;   ;   ;  ;   ;;;      ;;   ; ;;; ;  
 ;    ;      ;    ;      ;;  ;;       ;      ;;  ;; 
 ;     ;;   ;;;;;;;;   ;;;;  ;;      ;;      ;;  ;; 
 ;       ;;  ;        ;  ;;  ;;       ;      ;;  ;; 
 ;   ;   ;   ;;   ;  ;;  ;;  ;;       ;;   ; ;;  ;; 
 ;    ;;;      ;;;    ;;; ;  ;;         ;;;  ;;  ;; 
 ;                                                  
 ;                                                  
 ;                                                  
 ;                                                  
}

@section{Search}

@deftogether[
 (@defproc[(term-search [term term/c]
                        [#:ricoeur-only? ricoeur-only? any/c #t]
                        [#:book/article book/article (or/c 'any 'book 'article) 'any]
                        [#:exact? exact? any/c #f])
           (listof (is-a?/c document-search-results<%>))]
   @defthing[term/c flat-contract?
             #:value (and/c non-empty-string? (not/c #px"^\\s*$"))])]{
 Searches the @tech{corpus} object determined by @racket[current-corpus]
 for @racket[term].

 When @racket[ricoeur-only?] is non-false, @racket[term-search]
 omits results from passages not by Paul Ricœur.
 
 When @racket[exact?] is non-false, only matches containing @racket[term]
 exactly (but in a case-insensitive manner that normalizes whitespace)
 are returned. Otherwise (and by default), the @tech{search backend}
 attempts to match lexical variants of the given @racket[term],
 though the details of this behavior are backend-specific and unspecified.

 When @racket[book/article] is @racket['book] or @racket['article],
 only searches documents for which
 @racket[(eq? book/article (send doc #,(method TEI-info<%> get-book/article)))]
 would return @racket[#t].
 When @racket[book/article] is @racket['any] (the default),
 all documents in the @racket[current-corpus] are searched.
}

@definterface[document-search-results<%> (TEI-info<%>)]{
 The results of @racket[term-search] for each document in the @tech{corpus}
 are encapsulated in a @deftech{document search results} object.

 A @tech{document search results} object may only be created for documents
 which return at least one @tech{search result} value.
 @margin-note{The @racket[document-search-results<%>]
  interface contains additional,
  private methods which prevent it from being implemented by clients of
  this library.}
 @defmethod[(get-results) (non-empty-listof search-result?)]{
  Returns the @tech{search result} values contained by @(this-obj).
 }
 @defmethod[(count-results) exact-positive-integer?]{
  Returns the number of @tech{search result} values contained by @(this-obj)
  in a way that is cached across repeated calls
 }
}
@deftogether[
 (@defproc[(document-search-results-title [dsr (is-a?/c document-search-results<%>)])
           string?]
   @defproc[(document-search-results-results [dsr (is-a?/c document-search-results<%>)])
            (non-empty-listof search-result?)]
   @defform[#:kind "match expander"
            (document-search-results title-pat results-pat
                                     optional-pat ...)
            #:grammar ([optional-pat
                        (code:line #:date date-pat)
                        (code:line #:count count-pat)
                        (code:line #:citation cite-pat)])])]{
 Alternative means of extracting information from
 @tech{document search results} objects.
}

@deftogether[
 (@defproc[(search-result? [v any/c]) any/c]
   @defform[#:kind "match expander"
            (search-result kw-pat ...)
            #:grammar ([kw-pat
                        (code:line #:excerpt excerpt-pat)
                        (code:line #:location-stack loc-pat)
                        (code:line #:author author-pat)
                        (code:line #:page page-pat)])]
   @defproc[(search-result-excerpt [v search-result?])
            (maybe/c non-empty-string?)]
   @defproc[(search-result-page [v search-result?])
            (or/c (maybe/c string?)
                  (list/c (maybe/c string?) (maybe/c string?)))]
   @defproc[(search-result-location-stack [v search-result?])
            location-stack-entry?]
   @defproc[(search-result-author-string [v search-result?])
            string?]
   @defproc[(search-result<? [a search-result?] [b search-result?])
            any/c]
   @defproc[(search-result>? [a search-result?] [b search-result?])
            any/c])]{
 A @deftech{search result} value represents an individual result found
 by @racket[term-search]. It will have an excerpt unless copyright
 restrictions apply and a page number string unless the corresponding
 page was not numbered.

 The comparison functions @racket[search-result<?] and
 @racket[search-result>?] report the ordering of the results in the
 document from which they come. The results are only meaningful when
 both @tech{search result} values come from the same
 @tech{document search results} object.
}


@defproc[(location-stack-entry? [v any/c]) any/c]{
 A predicate recodnizing values that can represent details
 about the location of a @tech{search-result} in a document.
}

@defproc[(location-stack-entry->strings [loc location-stack-entry?])
         (listof string?)]{
 Converts the location information encapsulated in @racket[loc]
 to a (possibly empty) list of strings suitable for display
 to end users.
 The strings in the list begin with the broadest location
 description (e.g. front-matter) and end with the narrowest
 (e.g. some footnote).
 Each string represents some logical level of location hierarchy:
 returning them in a list allows clients of this library
 to add separators or otherwise lay them out for display.
}



