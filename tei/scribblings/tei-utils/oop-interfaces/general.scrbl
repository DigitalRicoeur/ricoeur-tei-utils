#lang scribble/manual

@title{General Interfaces}

@(require (only-in "../for-manual.rkt"
                   TODO
                   TODO/void
                   TODO/scrbl)
          (for-label (except-in racket
                                date
                                date?)
                     racket/unit
                     xml
                     data/maybe
                     ricoeur/tei/oop
                     (submod ricoeur/tei/oop/old-high-level/search/common private)
                     gregor
                     ))

The interfaces represented in this section contain methods
implemented by broad categories of TEI elements.


@;{                                                       
 ;           ;;;;                                     ;;    
 ;             ;;                                     ;;    
 ;     ;;;     ;;      ;;;  ; ;; ;;    ;;;   ;; ;   ;;;;;;; 
 ;   ;;   ;    ;;    ;;   ; ;; ;; ;  ;;   ;  ;;; ;    ;;    
 ;   ;    ;    ;;    ;    ; ;; ;; ;; ;    ;  ;;  ;;   ;;    
 ;  ;;;;;;;;   ;;   ;;;;;;;;;; ;; ;;;;;;;;;; ;;  ;;   ;;    
 ;   ;         ;;    ;      ;; ;; ;; ;       ;;  ;;   ;;    
 ;   ;;   ;     ;    ;;   ; ;; ;; ;; ;;   ;  ;;  ;;    ;    
 ;     ;;;       ;;    ;;;  ;; ;; ;;   ;;;   ;;  ;;     ;;; 
 ;                                                          
}
@section{@tt{element<%>}}

@definterface[element<%> ()]{
 TEI elements are represented by Racket objects implementing
 the @racket[element<%>] interface.

 @margin-note{The @racket[element<%>] interface and derived
  interfaces contain additional private methods to preserve
  certain invarialts, and thus may only be implemented by
  the objects provided from @racketmodname[ricoeur/tei].}

 @defmethod[(to-xexpr) any-tei-xexpr/c]{
  Returns the x-expression representation of @(this-obj).
 }

 @defmethod[(to-plain-text) string?]{
  Converts @(this-obj) to a plain-text string. The resulting
  string is @italic{not} the XML representation of
  @(this-obj): it is formated for uses that expect unstructured
  plain text. 
 }

 @defmethod[(get-name) symbol?]{
  Returns the name of the @(this-obj).
 }

 @defmethod[(get-attributes) (listof (list/c symbol? string?))]{
  Returns the attributes of @(this-obj) using the same
  representation used by x-expressions.
 }

 @defmethod[(get-body) (listof element-or-xexpr/c)]{
  Returns the contents of @(this-obj), with child elements
  represented by objects implementing @racket[element<%>].
 }

}

@;{
 ;           ;;;;                                     ;;                             ;;;;           
 ;             ;;                                     ;;                               ;;           
 ;     ;;;     ;;      ;;;  ; ;; ;;    ;;;   ;; ;   ;;;;;;;           ;;;    ;; ;      ;;   ;     ; 
 ;   ;;   ;    ;;    ;;   ; ;; ;; ;  ;;   ;  ;;; ;    ;;             ;   ;   ;;; ;     ;;    ;   ;  
 ;   ;    ;    ;;    ;    ; ;; ;; ;; ;    ;  ;;  ;;   ;;     ;;;;;;  ;   ;   ;;  ;;    ;;    ;   ;  
 ;  ;;;;;;;;   ;;   ;;;;;;;;;; ;; ;;;;;;;;;; ;;  ;;   ;;            ;;   ;;  ;;  ;;    ;;     ;  ;  
 ;   ;         ;;    ;      ;; ;; ;; ;       ;;  ;;   ;;             ;   ;   ;;  ;;    ;;     ; ;   
 ;   ;;   ;     ;    ;;   ; ;; ;; ;; ;;   ;  ;;  ;;    ;             ;   ;   ;;  ;;     ;     ; ;   
 ;     ;;;       ;;    ;;;  ;; ;; ;;   ;;;   ;;  ;;     ;;;           ;;;    ;;  ;;      ;;    ;    
 ;                                                                                             ;    
                                                                                                 
}
@section{@tt{elements-only<%>}}
@definterface[elements-only<%> (element<%>)]{
 Elements implementing this interface may not contain textual
 data directly.

 @defmethod[#:mode extend
            (get-body) (listof (or/c (is-a?/c element<%>)
                                     comment?
                                     p-i?))]{
  Like @(xmethod element<%> get-body), but the returned list may
  not contain strings, symbols, characters, or @racket[cdata].
 }

 @defmethod[(get-body/elements-only) (listof (is-a?/c element<%>))]{
  Like @(method elements-only<%> get-body), but the resulting list
  contains only the child elements: that is, it will not contain
  comments or processing instructions.
 }
}

@;{                                                        
 ;                    ;;                             ;;     
 ;                    ;;                             ;;     
 ;     ;;;;;   ;;;  ;;;;;;;          ; ;;            ;;;;   
 ;    ;  ;   ;;   ;   ;;             ;;  ;           ;;  ;  
 ;   ;;  ;;  ;    ;   ;;     ;;;;;;  ;;  ;   ;;;;;;  ;;  ;  
 ;    ;  ;  ;;;;;;;;  ;;             ;;  ;;          ;;  ;; 
 ;     ;;    ;        ;;             ;;  ;           ;;  ;  
 ;   ;;      ;;   ;    ;             ;;  ;           ;;  ;  
 ;    ;;;;;    ;;;      ;;;          ;;;;            ; ;;   
 ;   ;    ;;                         ;;                     
 ;  ;;    ;                          ;;                     
 ;    ;;;;                           ;;                     
 ;
}
@section{@tt{get-page-breaks<%>}}
@definterface[get-page-breaks<%> (element<%>)]{
 An interface implemented by objects from which page-breaks can
 be extracted. Note that @racket[pb<%>] is a sub-interface of
 @racket[get-page-breaks<%>].

 @defmethod[(get-page-breaks) (listof (is-a?/c pb<%>))]{
  Returns a list of objects representing the page-break elements
  recursively contained by @(this-obj)
 }
}



@;{                                                                        
 ;                                                                          
 ;                                                                          
 ;     ;;;;; ;;  ;;    ;;;     ;;      ;;            ; ;;      ;;    ;; ;;; 
 ;    ;  ;   ;;  ;;  ;;   ;  ;;  ;   ;;  ;           ;;  ;    ;  ;   ;;;    
 ;   ;;  ;;  ;;  ;;  ;    ;   ;       ;      ;;;;;;  ;;  ;       ;;  ;;     
 ;    ;  ;   ;;  ;; ;;;;;;;;   ;;      ;;            ;;  ;;    ;;;;  ;;     
 ;     ;;    ;;  ;;  ;           ;;      ;;          ;;  ;    ;  ;;  ;;     
 ;   ;;       ; ;;;  ;;   ;  ;   ;   ;   ;           ;;  ;   ;;  ;;  ;;     
 ;    ;;;;;    ; ;;    ;;;    ;;;     ;;;            ;;;;     ;;; ;  ;;     
 ;   ;    ;;                                         ;;                     
 ;  ;;    ;                                          ;;                     
 ;    ;;;;                                           ;;                     
 ;                                                                          
}
@section{@tt{guess-paragraphs<%>}}
@definterface[guess-paragraphs<%> (get-page-breaks<%>)]{
 Extends @racket[get-page-breaks<%>] with a method for automatically
 converting child "anonymous block" elements (see @racket[ab<%>])
 to paragraphs. In most cases, this method should be invoked
 on the top-level object implementing @racket[TEI<%>].

 @(TODO/void guess-paragraphs<%>
             #: Should this be private?)
 
 @defmethod[(guess-paragraphs [#:mode mode (or/c 'blank-lines 'line-breaks) 'blank-lines])
            (is-a?/c (object-interface #,(this-obj)))]{
  Returns an object like @(this-obj), but with any recursively conained
  "anonymous block" elements (see @racket[ab<%>]) replaced by
  paragraph and page-break elements (see @racket[p<%>] and @racket[pb<%>]).
  Children of the "anonymous block" elements are preserved.

  When mode is @racket['blank-lines] (the default),
  @(method guess-paragraphs<%> guess-paragraphs) assumes a blank line between
  blocks of text is used to indicate a paragraph; otherwise, it assumes
  every line-break begins a new paragraph. In both cases, entirely empty
  paragraphs are ommited, and freestanding page-breaks are allowed.

  Using @(method guess-paragraphs<%> guess-paragraphs) does not eliminate
  the need for manual review: for example, it will identify as "paragraphs"
  segments of text that should be represented as headings, list items, or
  footnotes. However, it saves a great deal of time for the common cases.
 }
}

@defproc[(guess-paragraphs? [v any/c]) any/c]{
 A predicate recognizing Racket objects that implement the
 @racket[guess-paragraphs<%>] interface
}


@;{                                                                 
 ;                                                                  
 ;  ;;;;;;;; ;;;;;;  ;;;;;;             ;                ;;;        
 ;     ;;    ;;        ;;               ;;             ;;           
 ;     ;;    ;;        ;;            ;;;;;   ;; ;    ;;;;;;;  ;;;   
 ;     ;;    ;;        ;;               ;;   ;;; ;     ;;    ;   ;  
 ;     ;;    ;;;;;     ;;    ;;;;;;     ;;   ;;  ;;    ;;    ;   ;  
 ;     ;;    ;;        ;;               ;;   ;;  ;;    ;;   ;;   ;; 
 ;     ;;    ;;        ;;               ;;   ;;  ;;    ;;    ;   ;  
 ;     ;;    ;;        ;;               ;;   ;;  ;;    ;;    ;   ;  
 ;     ;;    ;;;;;;  ;;;;;;             ;;   ;;  ;;    ;;     ;;;   
 ;                                                                  
}                                                                 

@section{@tt{TEI-info<%>}}
@definterface[TEI-info<%> ()]{
 An interface implemented by objects encapsulating information about a
 TEI XML document, notably objects implementing @racket[TEI<%>] or
 @racket[teiHeader<%>].

 @margin-note{Unlike other interfaces in this section, @racket[TEI-info<%>]
  is not derived from @racket[element<%>], so it may be freely implemented
  by client modules. However, additional required methods may be added
  to @racket[TEI-info<%>] without warning.
  Therefore, the recommended way for client modules to implement this interface
  is via @racket[define/TEI-info].
 }
 
 @defmethod[(get-title) string?]{
  Returns the title of the document, including subtitles.
 }
 @defmethod[(get-filename) (or/c #f string?)]{
  Returns the file name (without directory) used to instantiate
  @(this-obj), or @racket[#f] if none is known.
  See alse @racket[read-TEI] and @racket[file->TEI].

  @(TODO/void get-filename in TEI-info<%>
              #: What should happen on copy? "\n\n"
              Maybe these should really be in TEI<%>)
 }
 @defmethod[(get-full-path) (or/c #f (and/c path-string? absolute-path?))]{
  Like @(method TEI-info<%> get-filename), but returns the full path
  (if available).

  @(TODO/void get-full-path in TEI-info<%>
              #: What should happen on copy?)
 }
 @defmethod[(get-modify-seconds) (or/c #f exact-integer?)]{
  @(TODO get-modify-seconds in TEI-info<%>
              #: Document this!
              #:expr @bold{TODO: describe this!})
 }
 @defmethod[(get-publication-date) date?]{
  Returns the publication date of the specific version of the work
  on which the TEI document is based.
 }
 @defmethod[(get-original-publication-date) date?]{
  Returns the date when the work (as a whole) was first published,
  which may or may not be the same as the result of
  @(method TEI-info<%> get-publication-date).
 }
 @defmethod[(get-citation) string?]{
  Returns the human-readable citation for the work
 }
 @defmethod[(get-book/article) (or/c 'book 'article)]{
  Indicates whether @(this-obj) represents a book or an article.
 }
 @defmethod[(get-resp-string [resp symbol?])
            string?]{
  Returns a string naming the author or editor whose @tt{id}
  attribute is the string form of @racket[resp], raising
  an exception if none exists.
 }
}

@defform*[((define/TEI-info name TEI-info-expr)
           (define/TEI-info TEI-info-expr)
           (define/TEI-info #:each-time TEI-info-expr))
          #:contracts ([TEI-info-expr (is-a?/c TEI-info<%>)])]{
 @(TODO/void rethink TEI-info
             #: should it be forced to always be the same?)

 Inside a @racket[class*]-like form, binds all of the methods of
 @racket[TEI-info<%>] to implementations that invoke those methods
 on some other object implementing @racket[TEI-info<%>], as
 determined by the @racket[TEI-info-expr].
 (A @racket[teiHeader<%>] object would be a good choice.)

 If the @racket[#:each-time] keyword is present, the @racket[TEI-info-expr]
 is evaluated each time one of the methods is called.
 Otherwise, the @racket[TEI-info-expr] evaluated
 when an object of the class in which
 the @racket[define/TEI-info] form appears is initialized.
 
 If @racket[name] is given, it is bound to a private field containing
 the value of @racket[TEI-info-expr], except that @racket[set!] is
 protected by a contract insisting that the new value also
 be an object implementing @racket[TEI-info<%>].
}








