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
The @tech{corpus object} system is also the
primary hook for tools to integrate with
the larger Digital Ricœur application architecture.

@section{Working with Corpus Objects}
@defparam[current-corpus corpus
          (is-a?/c corpus%)
          #:value empty-corpus]{
 Contains a @tech{corpus object} for use by high-level functions
 like @racket[get-instance-info-set] and @racket[get-checksum-table].

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
 (@defproc[(corpus-get-instance-info-set [corpus (is-a?/c corpus%)])
           (instance-set/c)]
   @defproc[(corpus-get-checksum-table [corpus (is-a?/c corpus%)])
            checksum-table/c])]{
 Like @racket[get-instance-info-set] and @racket[get-checksum-table],
 respectively,
 but using @racket[corpus] instead of @racket[(current-corpus)].
}


@section{Creating Corpus Objects}
@defclass[corpus% object% ()]{
 A @tech{corpus object} is an instance of @racket[corpus%]
 or of a subclass of @racket[corpus%].

 For many purposes, @racket[directory-corpus%] offers more
 convienient initialization than @racket[corpus%].

 Note that creating a new instance of @racket[corpus%]
 often involves a fair amount of overhead,
 so creating redundant values should be avoided.
 Reusing @tech{corpus objects} may also improve 
 search performance through caching, for example.
 
 @defconstructor[([docs (instance-set/c tei-document?) (instance-set)])]{
  Constructs a @tech{corpus object} encapsulating @racket[docs].
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
}

@defthing[empty-corpus (is-a?/c corpus%)]{
 An empty @tech{corpus object} used as the default value of
 the @racket[current-corpus] parameter.

 With @racket[empty-corpus], @method[corpus% get-instance-info-set]
 always returns @racket[(instance-set)] and
 @method[corpus% get-checksum-table] always returns @racket[#hasheq()].
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
Clients of this library will want to extend the @tech{corpus object}
system to support additional features by implementing
new classes derived from @racket[corpus%].
There are two main points where derived classes will want to interpose
on @racket[corpus%]'s initialization:
@itemlist[
 #:style 'ordered
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
  for these kinds of extensions through three syntactic forms:
  @racket[corpus-mixin], @racket[corpus-mixin+interface],
  and @racket[define-corpus-mixin+interface].
  Most clients should use @racket[define-corpus-mixin+interface],
  but it is best understood as an extension of the simpler forms.
  }]


@defform[(corpus-mixin [from<%> ...] [to<%> ...]
           mixin-clause ...+)
         #:contracts ([from<%> interface?]
                      [to<%> interface?])]{
 Like @racket[mixin], but cooperates with @racket[corpus%]
 and the @racket[super-docs] and @racket[super-docs-evt] forms
 to provide access to the encapsulated @tech{instance set} of
 @tech{TEI documents} as a ``virtual'' initialization variable.
 The @racket[corpus<%>] interface is implicitly added to
 @racket[corpus-mixin]'s @racket[from<%>] interfaces.
 
 Most clients should use the higher-level @racket[corpus-mixin+interface]
 or @racket[define-corpus-mixin+interface], rather than using @racket[corpus-mixin]
 directly.
                                           
 A key design consideration is that a @racket[corpus%] instance does
 not keep its @tech{TEI documents} reachable after its initialization,
 as @tech{TEI document} values can be rather large.
 Derived classes are urged to follow this practice:
 they should initialize whatever state they need for their extended functionality,
 but they should allow the @tech{TEI documents} to be garbage-collected
 as soon as possible.

 Concretely, this means that @racket[corpus%] does not store
 the @tech{instance set} of @tech{TEI documents} in a
 @seclink["clfields" #:doc '(lib "scribblings/reference/reference.scrbl")]{field}
 (neither public nor private), as objects' fields are reachable after initialization. 

 Instead, derived classes can access the @tech{instance set} of @tech{TEI documents}
 during initialization using @racket[super-docs] or @racket[super-docs-evt]:
 
 @defsubform[(super-docs)]{
  Within @racket[corpus-mixin] and related forms,
  evaluates to the full @tech{instance set}
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
  Within @racket[corpus-mixin] and related forms, similar to @racket[super-docs],
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

@defform[#:literals {interface interface*}
         (corpus-mixin+interface [from<%> ...] [to<%> ...]
           interface-decl
           mixin-clause ...+)
         #:grammar
         [(interface-decl (interface (super<%> ...)
                            interface-method-clause ...)
                          (interface* (super<%> ...)
                                      ([prop-expr val-expr] ...)
                            interface-method-clause ...))
          (interface-method-clause method-id
                                   [method-id contract-expr])]
         #:contracts ([from<%> interface?]
                      [to<%> interface?]
                      [super<%> interface?]
                      [prop-expr struct-type-property?]
                      [contract-expr contract?])]{
 Like @racket[corpus-mixin], but evaluates to two values,
 a mixin and an assosciated interface.

 ...

 Most clients should use the higher-level @racket[define-corpus-mixin+interface],
 rather than using @racket[corpus-mixin+interface] directly.
}

@defform[#:literals (interface interface*
                      define/public define/pubment define/public-final)
         (define-corpus-mixin+interface name-spec
           [from<%> ...] [to<%> ...]
           interface-decl*
           mixin-clause ...+)
         #:grammar
         [(name-spec base-id [id-mixin id<%>])
          (interface-decl* (interface (super<%> ...)
                             interface-method-clause* ...)
                           (interface* (super<%> ...)
                                       ([prop-expr val-expr] ...)
                             interface-method-clause* ...))
          (interface-method-clause* interface-method-clause
                                    ext-method-clause)
          (interface-method-clause method-id
                                   [method-id contract-expr])
          (ext-method-clause [ext-clause-part ...])
          (ext-clause-part (code:line method-definition-form (code:comment "required"))
                           (code:line #:contract contract-expr)
                           (code:line #:proc proc-id)
                           with-current-decl)
          (method-definition-form (define/method (method-id kw-formal ...)
                                    body ...+))
          (define/method define/public define/pubment define/public-final)
          (with-current-decl (code:line #:with-current with-current-id
                                        #:else [else-body ...+])
                             (code:line #:with-current/infer
                                        #:else [else-body ...+]))]
         #:contracts ([from<%> interface?]
                      [to<%> interface?]
                      [super<%> interface?]
                      [prop-expr struct-type-property?]
                      [contract-expr contract?])]{
 If no @racket[ext-method-clause] appears,
 equivalent to:
 @racketblock[
 (define-values [id-mixin id<%>]
   (corpus-mixin+interface [from<%> ...] [to<%> ...]
     interface-decl*
     mixin-clause ...+))]
 except that @racket[define-corpus-mixin+interface] can often
 produce better @seclink["infernames" #:doc '(lib "scribblings/reference/reference.scrbl")]{
  inferred value names}.
 If @racket[name-spec] is given as a single @racket[base-id],
 identifiers are synthesized with the suffixes @racketid[-mixin] and @racketid[<%>]
 using the lexical context of @racket[base-id].
       
 The @racket[ext-method-clause] variant extends the grammar of @racket[interface]
 and @racket[interface*] to support defining functions related to
 one of the interface's methods:
 @itemlist[
 @item{...}
 ]}

@definterface[corpus<%> (corpus%)]{
 Equivalent to @racket[(class->interface corpus%)].
 Note that @racket[corpus%] implements lexically-protected
 methods (see @racket[define-local-member-name]),
 so @racket[corpus<%>] can only be implemented by
 inheriting from @racket[corpus%]:
 @racket[corpus<%>] is provided primarily as a convienience for
 writing derived interfaces, mixins, and contracts.
}
