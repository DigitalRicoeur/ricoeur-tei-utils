#lang scribble/manual

@title{High-level Corpus Functionality}
@(declare-exporting ricoeur/tei)

@(require "for-manual.rkt")

The bindings documented in this section are provided
by @racketmodname[ricoeur/tei], but @bold{not}
by @racketmodname[ricoeur/tei/base].


@defparam[current-corpus corpus
          (is-a?/c corpus%)
          #:value empty-corpus]

@defproc[(get-instance-info-set)
         (instance-set/c)]
@defproc[(get-checksum-table)
         (hash/c symbol?
                 symbol?
                 #:immutable #t)]
@deftogether[
 (@defproc[(term-search [term term/c]
                        [#:ricoeur-only? ricoeur-only? any/c #t]
                        [#:book/article book/article (or/c 'any 'book 'article) 'any]
                        [#:exact? exact? any/c #f])
           (instance-set/c document-search-results?)]
   @defthing[term/c flat-contract?
             #:value (and/c string-immutable/c #px"\\S")])]


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
 @defconstructor[([docs (instance-set/c tei-document?) (instance-set)]
                  [search-backend search-backend/c '(eager noop)])]{

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

@defthing[empty-corpus (is-a?/c corpus%)]

@defthing[search-backend/c contract?
          #:value (let ([base/c (or/c 'noop 'regexp (postgresql-data-source/c))])
                    (or/c base/c
                          (list/c 'eager base/c)))]

@defproc[(postgresql-data-source/c) contract?]

@defclass[directory-corpus% corpus% ()]{
 @defconstructor[([path (and/c path-string?
                               (if/c string? immutable? any/c)
                               directory-exists?)]
                  [search-backend search-backend/c '(eager noop)])]{

 }
}

@subsection{Deriving New Corpus Classes}
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

}




@section{Search Results}
@defpredicate[document-search-results?]
@defpredicate[search-result?]
@deftogether[
 (@defproc[(search-result-<? [a search-result?] [b search-result?])
           boolean?]
   @defproc[(search-result->? [a search-result?] [b search-result?])
            boolean?])]
@deftogether[
 (@defproc[(search-result-excerpt [search-result search-result?])
           (maybe/c (and/c string-immutable/c
                           trimmed-string-px))]
   @defthing[trimmed-string-px pregexp?
             #:value #px"^\\S$|^\\S.*\\S$"])]


@section{Searching Without a Corpus Object}
@deftogether[
 (@defproc[(initialize-search-backend [backend search-backend/c]
                                      [docs (instance-set/c tei-document?)])
           searchable-document-set?]
   @defpredicate[searchable-document-set?])]{

}

@defthing[noop-searchable-document-set searchable-document-set?]{

}

@defproc[(searchable-document-set-do-term-search
          [searchable-document-set searchable-document-set?]
          [term term/c]
          [#:ricoeur-only? ricoeur-only? any/c #t]
          [#:book/article book/article (or/c 'any 'book 'article) 'any]
          [#:exact? exact? any/c #f])
         (instance-set/c document-search-results?)]{

}





