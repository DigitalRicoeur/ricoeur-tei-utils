#lang scribble/manual

@title{Defining TEI Elements}
@(declare-exporting ricoeur/tei/kernel/lang/doc-lang)
@(require "for-lang-kernel.rkt"
          scribble/bnf
          )


@defform[
 #:literals (1+ 0-1 0+)
 (define-element name-id
   maybe-inset
   contract-option ...
   type+prose)
 #:grammar
 [(maybe-inset code:blank
               (code:line #:inset? inset?-expr))
  (contract-option #:contains-text
                   (code:line #:extra-check extra-check-expr)
                   (code:line #:attr-contracts attr-contracts-spec)
                   (code:line #:required-attrs (attr-name-id ...))
                   (code:line #:children children-spec)
                   (code:line #:required-order (child-name-id ...)))
  (attr-contracts-spec ([attr-name-id attr-contract-expr] ...))
  (children-spec ([repeat-constraint child-name-id] ...))
  (repeat-constraint @#,(BNF-alt @racket[1]
                                 @racket[1+]
                                 @racket[0-1]
                                 @racket[0+]))
  (type+prose (code:line struct-type-def #:prose [prose-body ...+])
              (code:line #:prose [prose-body ...+] struct-type-def))
  (struct-type-def @#,elem{see @secref["struct-type-def"]})
  ]
 #:contracts ([extra-check-expr
               (or/c #f (-> raw-xexpr-element/c
                            (or/c blame? #f)
                            any/c
                            any/c))]
              [attr-contract-expr flat-contract?]
              )]{

 Declares the specification
 for the TEI XML element @racket[name-id], including documentation,
 a contract, and a @tech{TEI element struct type} implementation.
 See also @racket[define-elements-together].

 In the @tech{documentation-time} phase, the @racket[name-id] and
 @racket[contract-option] clauses are used to make a ``blue box.''
 This is combined with the @racket[prose-body] forms
 (which may include nested @racket[define-element] or
 @racket[define-elements-together] forms)
 to generate Scribble documentation of
 Digital @|Ricoeur|'s required structure for the specified element.
 If an @racket[inset?-expr] is given and evaluates (at documentation-time)
 to a non-false value, the elements are indented
 (somewhat like @racket[defsubform]).
 The @tech{documentation-time} phase generates the
 @guidelines-secref["Formal_Specification"] document.

 At @tech{runtime}, any nested @racket[define-element] or
 @racket[define-elements-together] forms are flattened.
 Parts of the form are evaluated (as discussed further below)
 to obtain values needed for the contract and
 @tech{tei element struct type} definitions.
 The order of this evaluation is unspecified except where
 otherwise noted.
 The @(hash-lang-kernel) language gathers the relevant
 values and encapsulates them in the module's
 @tech{elements specification transformer} binding
 for later linking and invokation with
 @racket[define-values/elements-specifications].
 The @racket[define-element] form may also bind module-level names
 to other values (again, as discussed further below);
 these may be exported with @racket[provide] as desired.

 In preparation for creating the contract,
 portions of the @racket[contract-option] clauses are
 evaluated to obtain intermediate values, which are
 encapsulated in the @tech{elements specification transformer}.
 Compile-time or runtime errors may occur during evaluation:
 for example, if an @racket[extra-check-expr] is given that
 fails to satisfy its contract, an error will be raised as
 soon as the violation is detected.
 However, while these subforms will be evaluated when
 the module containing the @racket[define-element] form is run,
 the contract will not actually be created until
 the resulting @tech{elements specification transformer}
 is invoked using @racket[define-values/elements-specifications].
 This delay allows the contract to refer to (and be referred to by)
 other element definitions without creating cyclic dependency
 problems, as long as the corresponding
 @tech{elements specification transformers} are linked prior to invokation.



 @defform[(define-elements-together
            maybe-inset
            ([name-id
              contract-option ...
              struct-type-def]
             ...+)
            prose-body ...)]{
  Like @racket[define-element], but define several TEI XML
  elements at once, somewhat like @racket[deftogether].
 }








 @section[#:tag "struct-type-def"]{Struct Type Definition Overview}
 @(declare-exporting ricoeur/tei/kernel/lang/specification-lang)

 @defform[#:link-target? #f #:id define-element
 #:literals [field 
             lift-begin 
             lift-property 
             lift-methods]
 (code:line)
 #:grammar
 [(struct-type-def (code:line struct-type-def-part ...))
  (struct-type-def-part (code:line #:predicate predicate-name-id)
                        (code:line #:constructor ctor-spec)
                        (code:line #:property prop-expr prop-val-expr)
                        (code:line #:methods generic-id [methods-body ...])
                        (code:line #:begin [begin-body ...]))
  (ctor-spec [ctor-arg-binding ...
              ctor-body ...])
  (ctor-arg-binding (code:line #:name name-arg-id)
                    (code:line #:attributes attributes-arg-id)
                    (code:line #:body body-arg-id)
                    (code:line #:body/elements-only body/elements-only-arg-id)
                    (code:line #:this/thunk this/thunk-id))
  (ctor-body plain-ctor-body
             (code:line special-ctor-form (code:comment "after partial expansion")))
  (special-ctor-form (field . _)
                     (lift-begin . _)
                     (lift-property . _)
                     (lift-methods . _))
  ]
 #:contracts ([prop-expr struct-type-property?]
              )]{
 
  The @tech{runtime} semantics of the @racket[struct-type-def]
  subform, which defines the @tech{tei element struct type},
  are somewhat more complicated.

  If a @racket[predicate-name-id] is given, it is bound
  at the module level to a predicate function recognizing
  instances of the @tech{tei element struct type} being defined.

  A @racket[#:property] or @racket[#:methods] clause works like
  the corresponding clause for @racket[struct], except that,
  in the @racket[prop-val-expr] or @racket[methods-body] forms,
  the @racket[get-field] form may be used to access
  @tech{tei element struct fields} (discussed below) if
  any are defined.
  A @racket[#:begin] clause causes its @racket[begin-body] forms
  to be spliced into the surrounding context,
  but wraped to cooperate with @racket[get-field] like
  @racket[prop-val-expr] and @racket[methods-body] forms.

  The @racket[ctor-spec] (when given) defines an initialization
  function for the @tech{tei element struct type}.
  It also provides a number of hooks to support macro-based
  extensions over the core functionality.
  Note that the actual function @racket[define-element]
  creates from the @racket[ctor-spec] is never directly
  accessible: it is encapsulated in the
  module's @tech{elements specification transformer}.

  The constructor function is called with arguments for the
  structural components of the element (name, attributes, and body).
  They correspond to the functions @racket[tei-element-get-name],
  @racket[tei-element-get-attributes],
  @racket[tei-element-get-body], and (when applicable)
  @racket[tei-get-body/elements-only].
  The optional @racket[ctor-arg-binding] clauses bind the given
  identifier to the corresponding argument in the scope of the
  @racket[ctor-body] forms.
  Note that it is a syntax error for the keywords 
  @racket[#:body/elements-only] and @racket[#:contains-text]
  to appear together.
 
  The @racket[#:this/thunk] @racket[ctor-arg-binding] clause
  is slightly different: its presence causes @racket[this/thunk-id]
  to be bound to a thunk that returns the @tech{tei element struct}
  currently being constructed (i.e. ``this'').
  Internally, calling the thunk forces a thread-safe
  @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{promise}
  (see @racket[delay/sync]) that returns the instance.
  Such a thunk cannot be used in a reentrant way,
  which would be the moral equivalent of use-before-definition,
  but it can immediately be used in background threads,
  such as to create fields that contain @racket[delay/thread] promises.

 }


                            
 
 
 The @racket[ctor-body] forms are partially expanded
 (including flattening @racket[begin]) to distinguish
 @racket[plain-ctor-body] and @racket[special-ctor-declaration]
 forms, allowing extensions by macros that expand to
 the primitive @racket[special-ctor-declaration].
 The @racket[plain-ctor-body] forms are an arbitrary sequence of
 definitions and expressions in an internal-definition context.
 It is the @racket[special-ctor-declaration] forms which give them
 special meaning.
 
 @deftogether/indent[              
 (@defform[(field field-name-id field-option ...)
           #:grammar
           [(field-option accessor-opt print-opt check-opt)
            (accessor-opt code:blank
                          infer-opt
                          (code:line #:accessor maybe-accessor)
                          [#:accessor maybe-accessor])
            (infer-opt #:infer [#:infer])
            (maybe-accessor accessor-id #f)
            (print-opt code:blank
                       (code:line #:print? bool-literal)
                       [#:print? bool-literal]
                       #:hide
                       [#:hide])
            (check-opt code:blank
                       (code:line #:check check-expr)
                       [#:check check-expr]
                       [#:check])]
           #:contracts ([check-expr contract?])])]{
  The most fundamental kind of @racket[special-ctor-declaration] is
  the @racket[field] form, which declares a
  @deftech{tei element struct field}.
  
  A @tech{tei element struct type} has one (immutable) field
  for each @racket[field] declaration among its expanded
  @racket[ctor-body] forms (in addition to fields inherited from
  its abstract base type).
  The @racket[field] declaration requires that
  @racket[field-name-id] must be defined by one of the
  @racket[plain-ctor-body] forms: not only must it have a binding,
  but a binding from the surrounding context is not sufficient.
  After all of the @racket[plain-ctor-body] forms have been evaluated,
  the @tech{tei element struct} instance is created using the
  values bound to the @racket[field-name-id]s for the
  @tech{tei element struct fields}.

  The @racket[field-name-id]s are recognized with consideration to
  lexical context, rather than symbolically, so macros may freely
  introduce @racket[field] declarations without fear of name collisions.

  The @racket[field-option] clauses, when given, provide some additional
  convieniences. A @racket[check-expr] specifies a contract on the value
  of the field. The expression is lifted to the module level and evaluated
  only once, and the contract is applied just before the
  @tech{tei element struct} instance is created.
  
  If an @racket[accessor-id] or @racket[infer-opt] is given,
  a function that accesses the value of the field is bound 
  in the context of the enclosing @racket[define-element] form.
  If an @racket[accessor-id] is given, it is used as the name of
  the defined function (including its lexical context).
  Otherwise, if @racket[infer-opt] is given, an identifier is
  synthesized with the lexical context of @racket[field-name-id]
  in the form @racket[name-id]@racketidfont{-}@racket[field-name-id],
  where @racket[name-id] is the name of the element currently
  being defined.
  
  A field with no @racket[accessor-id] or @racket[infer-opt] specified 
  can still be accessed via the @racket[get-field] form inside the text of a
  @racket[begin-body], @racket[prop-val-expr], or @racket[methods-body]
  subform of @racket[define-element]. Read on for details.

  A @racket[print-opt] clause, when given, controls whether the field
  is used for printing (as with @racket[print], @racket[write], and
  @racket[display]).
  The field is not printed if @racket[#:hide] appears or if
  @racket[#:print?] is used with @racket[#f].
 }                   

 @deftogether/indent[              
 (@defform*[[(get-field field-name-id)
             (get-field field-name-id target-expr)]] 
   )]{
  Inside the text of a @racket[begin-body], @racket[prop-val-expr],
  or @racket[methods-body] subform
  of @racket[define-element], @racket[(get-field field-name-id)]
  expands to a procedure that accesses the field @racket[field-name-id]
  (which must have been declared with @racket[field])
  from an instance @tech{tei element struct type} being defined.
  The @racket[(get-field field-name-id target-expr)] variant
  is short for @racket[((get-field field-name-id) target-expr)].

  Like @racket[field], @racket[get-field] is sensitive to the
  lexical context of @racket[field-name-id]: @racket[get-field]
  cooperates with @racket[define-element] to locally bind
  @racket[field-name-id] to a compile-time value in the contexts
  where @racket[get-field] is permitted, but shaddowing the binding
  of @racket[field-name-id] will render it inaccessable.
 }
                                                                          
 Macros can take advantage of @racket[field] and @racket[get-field]'s
 respect for scope to add fields
 to a @tech{tei element struct type} ``hygenically''.
 The @racket[lift-begin], @racket[lift-property], and @racket[lift-methods]
 @racket[special-ctor-declaration] forms are designed to make it
 convienient for a macro to expand to both a @racket[field] declaration
 and the use of the defined field.

 @deftogether/indent[              
 (@defform[(lift-property prop-expr prop-val-expr)
           #:contracts ([prop-expr struct-type-property?])])]{
  A @racket[lift-property] declaration is lifted out of
  the constructor definition to attach a structure type property
  to the @tech{tei element struct type} being defined, roughly
  as though it were @racket[#:property prop-expr prop-val-expr].
  Because it is lifted, bindings from the @racket[plain-ctor-body] forms
  are not in scope in the @racket[prop-expr] or @racket[prop-val-expr];
  however, in the @racket[prop-val-expr], @racket[get-field] may be used
  to access @tech{tei element struct fields}.
 }

 @deftogether/indent[              
 (@defform[(lift-methods generic-id
                         [methods-body ...])])]{
  Like @racket[lift-property], but for a @racket[#:methods] clause.
 }          

 @deftogether/indent[             
 (@defform[(lift-begin begin-body ...)])]{
  Like @racket[lift-property], but for a @racket[#:begin] clause.
 }
}


@section{Derived Field Definition Forms}
@(declare-exporting ricoeur/tei/kernel/lang/specification-lang)

Several derived forms are included as alternatives to the
primative @racket[field] declaration.

@defform[(declare-resp-field option ...)
         #:grammar [(option (code:line attributes-expr (code:comment "required"))
                            (code:line #:key key-id))]
         #:contracts ([attributes-expr (listof (list/c symbol? string-immutable/c))])]{
 Creates a field (protected by lexical scope) and attaches assosciated
 properties to make the element being defined work with
 @racket[tei-element-resp].
 The @racket[attributes-expr] should be the element's attribute list,
 probably obtained via an @racket[#:attributes] @racket[_ctor-arg-binding]
 clause in @racket[define-element].

 If a @racket[key-id] is given, it is used (in quoted form)
 instead of @racket['resp] for the attribute name.
 If the specified attribute is present, its value must match
 the regular expression @racket[#rx"^#.+$"].
}

@defform[(declare-paragraphs-status-field value-expr)
         #:contracts ([value-expr guess-paragraphs-status/c])]{
 Declares a field to contain the value that should ultimately
 be returned by @racket[tei-document-paragraphs-status]
 and attaches an assosciated structure type property.
 The name of the field is protected by lexical scope.
 The details of this protocol are subject to change.
}

@defproc[(local-element-name)
         (or/c #f identifier?)]{
 This function is provided @racket[for-syntax].
                                
 @TODO/void[local-element-name #: Where is this legal and/or useful?]
 @bold{TODO!} Where is this legal and/or useful?
}

@defthing[prop:element->plain-text
          (struct-type-property/c
           (or/c (-> tei-element? string?)
                 (-> tei-element? boolean? string?)))]{
 The definition of a @tech{tei element struct type} can use
 @racket[prop:element->plain-text] to override the default
 behavior of @racket[element-or-xexpr->plain-text].
 Note that attaching @racket[prop:element->plain-text] to
 unrelated struct types has no effect: it is only used
 for @tech{tei element structs}.

 The first argument to the function given as the property value
 is always the @tech{tei element struct} instance to be converted
 to plain text.
 If the function given as the property value accepts a second argument,
 it will be a boolean corresponding to the @racket[#:include-header?]
 argument to @racket[element-or-xexpr->plain-text].
}

@defform[(define/field maybe-infer field/opts
           rhs ...)
         #:grammar [(maybe-infer code:blank #:infer)
                    (field/opts field-name-id
                                [field-name-id field-option ...])]]{
 Combines @racket[define] with a @racket[field] declaration,
 where @racket[field-option], if present, is the same as
 for @racket[field].
 
 When the @racket[#:infer] keyword is given for @racket[maybe-infer],
 @racket[#:infer] is implicitly added before the first
 @racket[field-option].
 
 Multiple @racket[rhs] forms are implicitly wrapped with
 @racket[let] if needed.

 Note that @racket[define/field] does not support
 the form of @racket[define] with a function header on the
 left-hand side.
}

@defform[(define-fields maybe-infer [field/opts rhs ...] ...)]{
 Like @racket[def], but based on @racket[define/field]
 instead of @racket[define].
 
 When the @racket[#:infer] keyword is given for @racket[maybe-infer],
 it applies to all of the @racket[field/opts] subforms 
 as with @racket[define/field].
}

@defform[(define-values/fields maybe-infer (field/opts ...)
           rhs ...)]{
 Like @racket[define/field], but based on @racket[define-values].
 The last @racket[rhs] form must be an expression that
 produces as many values as there are @racket[field/opts] forms.

 When the @racket[#:infer] keyword is given for @racket[maybe-infer],
 it applies to all of the @racket[field/opts] subforms 
 as with @racket[define/field].
}






@section{Implementing Additional Forms}
@(declare-exporting ricoeur/tei/kernel/lang/specification-lang)
@deftogether[
 (@defform[(field/derived orig-datum field-name-id field-option ...)]
   @defform*[[(get-field/derived orig-datum field-name-id)
              (get-field/derived orig-datum field-name-id target-expr)]]
   @defform[(lift-property/derived orig-datum prop-expr prop-val-expr)
            #:contracts ([prop-expr struct-type-property?])]
   @defform[(lift-methods/derived orig-datum generic-id
                                  [methods-body ...])]
   @defform[(lift-begin/derived orig-datum begin-body ...)]
   @defform[(define/field/derived orig-datum maybe-infer field/opts
              rhs ...)]
   @defform[(define-values/fields/derived orig-datum maybe-infer (field/opts ...)
              rhs ...)])]{
 Like the corresponding form without the @racket[/derived]
 suffix, but with error reporting in terms of @racket[orig-datum].
}


@defproc[#:kind "syntax class"
         (field-name/maybe-opts [orig-datum syntax?] [infer? #f])
         @#,tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{syntax class}]{
 A syntax class recognizing the @racket[_field/opts] subform
 of @racket[define/field].
 The syntax class binds two attributes:
 @racket[name] is bound to the field name identifier,
 and @racket[declaration] is bound to a synthesized @racket[field/derived]
 declaration for the field, using @racket[orig-datum] for error reporting.

 When @racket[infer?] is non-false, the @racket[#:infer] option is
 implicitly added as with @racket[define/field].
}

