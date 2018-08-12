#lang scribble/manual

@title{Defining TEI Elements}
@(declare-exporting ricoeur/tei/kernel/lang/doc-lang)
@(require "for-lang-kernel.rkt"
          scribble/bnf
          )

@(define-syntax-rule (racket-BNF-alt datum ...)
   (BNF-alt (racket datum) ...))
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
  (repeat-constraint #,(racket-BNF-alt 1 1+ 0-1 0+))
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
 A @racket[define-element] form may appear only at the module level
 or in an @tech{element definition prose} context.

 The @racket[contract-option] clauses specify validity
 requirements for the element's attributes and contents.
 At the @tech{documentation-time} phase, they generate the
 code that produces the element's ``blue box''.
 (See @guidelines-secref["Formal_Specification"] for examples.)
 The @racket[attr-contract-expr] sub-forms are typeset as with
 @racket[racketblock], so bindings at the label phase level
 relative to the @tech{documentation-time} phase are used
 for cross-references.
 At @tech{runtime}, @racket[contract-option] clauses are used
 to generate the contract produced by @racket[(tei-xexpr/c name-id)].
 The @racket[attr-contract-expr] and @racket[extra-check-expr]
 expressions are evaluated at the @tech{runtime} of the module
 containing the @racket[define-element] form,
 and references to their values are part of the static
 information encapsulated by the module's
 @tech{elements specification transformer} to be used when it is
 invoked (see @racket[define-values/elements-specifications]).

 The @racket[struct-type-def] non-terminal produces the implementation
 of the @tech{TEI element struct type} for the element.
 Its grammar and semantics are documented under @secref["struct-type-def"].
 These forms have meaning only at @tech{runtime}.

 The @racket[prose-body] forms are expanded in a
 @deftech{element definition prose} context, a special type of
 expression context that allows nested uses of
 @racket[begin-for-runtime], @racket[define-element], and
 related forms.
 Their values at the @tech{documentation-time} phase are used
 for the prose description of the element, while
 @tech{runtime} code embedded via @racket[begin-for-runtime]
 or similar is lifted to the module level of the
 @tech{runtime} phase.

 If an @racket[inset?-expr] is given and evaluates 
 (at the @tech{documentation-time} phase)
 to a non-false value, the @racket[prose-body] forms are indented
 (somewhat like @racket[defsubform]).

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
                        (code:line #:property prop-expr prop-val-expr)
                        (code:line #:methods generic-id [methods-body ...])
                        (code:line #:begin [begin-body ...])
                        (code:line #:constructor ctor-spec))
  (ctor-spec [ctor-arg-binding ...
              ctor-body ...])
  (ctor-arg-binding (code:line #:name name-arg-id)
                    (code:line #:attributes attributes-arg-id)
                    (code:line #:body body-arg-id)
                    (code:line #:body/elements-only body/elements-only-arg-id)
                    (code:line #:this/thunk this/thunk-id))
  (ctor-body (code:line plain-ctor-body (code:comment "definitions and expressions"))
             (code:line special-ctor-form (code:comment "after partial expansion")))
  (special-ctor-form (code:line (field . _) (code:comment "and related forms"))
                     (lift-begin . _)
                     (lift-property . _)
                     (lift-methods . _))
  ]
 #:contracts ([prop-expr struct-type-property?]
              )]{

  The grammar above describes the @racket[struct-type-def]
  non-terminal from the grammars for @racket[define-element]
  and @racket[define-elements-together], which produces the
  implementation of the @tech{TEI element struct type} for the element.
  All of these sub-forms have meaning only at the @tech{runtime} phase.

  If a @racket[predicate-name-id] is given, it is bound
  at the module level to a predicate function recognizing
  instances of the @tech{TEI element struct type} being defined.

  A @racket[#:property] or @racket[#:methods] clause works like
  the corresponding clause for @racket[struct], except that,
  in the @racket[prop-val-expr] or @racket[methods-body] forms,
  the @racket[get-field] form may be used to access
  @tech{TEI element struct fields} (discussed below) if
  any are defined.
  
  A @racket[#:begin] clause causes its @racket[begin-body] forms
  to be spliced into the surrounding module-level context,
  but wraped to cooperate with @racket[get-field] like
  @racket[prop-val-expr] and @racket[methods-body] forms.

  The @racket[ctor-spec] (when given) declares
  @tech{TEI element struct fields} that are present on instances
  of the @tech{TEI element struct type} and defines an initialization
  function to compute their values.
  
  The @racket[ctor-spec] may begin with @racket[ctor-arg-binding]
  clauses, which bind names to the arguments needed to compute the
  values of the @tech{TEI element struct fields}.
  The @racket[#:name], @racket[#:attributes], @racket[#:body],
  and @racket[#:body/elements-only] keywords correspond to the
  functions @racket[tei-element-get-name], @racket[tei-element-get-attributes],
  @racket[tei-element-get-body], and (when applicable)
  @racket[tei-get-body/elements-only].
  Note that it is a syntax error for @racket[#:body/elements-only]
  to appear if the @racket[#:contains-text] @racket[_contract-option]
  was provided for the @tech{TEI element struct type}.
  (See @racket[define-element].)

  The @racket[#:this/thunk] @racket[ctor-arg-binding] clause
  is slightly different. When it is present, @racket[this/thunk-id]
  is bound to a thunk that returns the @tech{TEI element struct}
  instance being constructed (i.e. ``this'').
  Internally, calling the thunk forces a thread-safe promise
  (see @racket[delay/sync]) that returns the instance.
  Such a thunk cannot be used in a reentrant way,
  which would be the moral equivalent of use-before-definition,
  but it can immediately be used in background threads,
  such as to create @tech{TEI element struct fields}
  that contain @racket[delay/thread] promises.
  
  After the @racket[ctor-arg-binding] clauses, the @racket[ctor-body]	
  forms are a mixture of definitions, expressions, and 
  @racket[special-ctor-form] declarations, which are recognized after
  partial expansion.
  The @racket[ctor-body] need not end with an expression.
  Definitions create local bindings as usual, but definitions of
  identifiers declared as @tech{TEI element struct fields}
  with @racket[field] or a related @racket[special-ctor-form] declaration 
  also specify the value for that field on the resulting
  @tech{TEI element struct} instance.
  Expressions are evaluated for their side-effects:
  there is no ``return value''.
  Other @racket[special-ctor-form] declarations (such as @racket[lift-begin].
  @racket[lift-property], and @racket[lift-methods]) provide hooks
  for implementing higher-level macros to be used in the @racket[ctor-body].

  @defsubform[(field field-name-id field-option ...)
              #:grammar
              [(field-option #,(racket-BNF-alt accessor-opt
                                               print-opt
                                               check-opt))
               (accessor-opt code:blank
                             infer-opt
                             (code:line #:accessor accessor-id))
               (infer-opt #:infer)
               (print-opt code:blank
                          (code:line #:print? bool-literal)
                          #:hide)
               (check-opt code:blank
                          (code:line #:check check-expr))]
              #:contracts ([check-expr contract?])]{
   The primitive @racket[special-ctor-form] for declaring a
   @deftech{TEI element struct field}.
   Higher-level forms such as @racket[define/field] and
   @racket[declare-resp-field] are built on top of @racket[field].

   A @tech{TEI element struct type} has one immutable field
   for each @racket[field] declaration among its expanded
   @racket[ctor-body] forms (in addition to fields inherited from
   its abstract base type).
   The @racket[field] declaration requires that
   @racket[field-name-id] must be defined as a value (not syntax)
   by one of the @racket[plain-ctor-body] forms:
   not only must it have a binding,
   but a binding from the surrounding context is not sufficient.
   After all of the @racket[plain-ctor-body] forms have been evaluated,
   the @tech{TEI element struct} instance is created using the
   values bound to the @racket[field-name-id]s for the
   @tech{TEI element struct fields}.

   The @racket[field-name-id]s are recognized with consideration to
   lexical context, rather than symbolically, so macros may freely
   introduce @racket[field] declarations without fear of name collisions.

   The @racket[field-option] clauses, when given, provide some additional
   convieniences:
   @(itemlist
     @item{A @racket[check-expr] specifies a contract on the value
    of the field. The expression is lifted to the module level and evaluated
    only once, and the contract is applied just before the
    @tech{TEI element struct} instance is created.
   }
     @item{
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
   }
     @item{
    A @racket[print-opt] clause, when given, controls whether the field
    is used for printing (as with @racket[print], @racket[write], and
    @racket[display]).
    The field is not printed if @racket[#:hide] appears or if
    @racket[#:print?] is used with @racket[#f].
   }
     )}

  @defsubform*[[(get-field field-name-id)
                (get-field field-name-id target-expr)]]{
   Inside the text of a @racket[begin-body], @racket[prop-val-expr],
   or @racket[methods-body] subform
   of @racket[define-element], @racket[(get-field field-name-id)]
   expands to a procedure that accesses the field @racket[field-name-id]
   (which must have been declared with @racket[field])
   from an instance @tech{TEI element struct type} being defined.
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
  to a @tech{TEI element struct type} ``hygenically''.
  @margin-note*{See @racket[declare-resp-field] for an example
   of a macro that uses these features.}
  The @racket[lift-begin], @racket[lift-property], and @racket[lift-methods]
  @racket[special-ctor-form] declarations are designed to make it
  convienient for a macro to expand to both a @racket[field] declaration
  and the use of the defined field.

  @defsubform[(lift-property prop-expr prop-val-expr)
              #:contracts ([prop-expr struct-type-property?])]{
   A @racket[lift-property] declaration is lifted out of
   the constructor definition to attach a structure type property
   to the @tech{TEI element struct type} being defined, roughly
   as though it were @racket[#:property prop-expr prop-val-expr].
   Because it is lifted, bindings from the @racket[plain-ctor-body] forms
   are not in scope in the @racket[prop-expr] or @racket[prop-val-expr];
   however, in the @racket[prop-val-expr], @racket[get-field] may be used
   to access @tech{TEI element struct fields}.
  }

  @defsubform[(lift-methods generic-id
                            [methods-body ...])]{
   Like @racket[lift-property], but for a @racket[#:methods] clause.
  }          

  @defsubform[(lift-begin begin-body ...)]{
   Like @racket[lift-property], but for a @racket[#:begin] clause.
  }
 }
}

@section{Field Definition Forms}
@(declare-exporting ricoeur/tei/kernel/lang/specification-lang)

Several higher-level forms are included as alternatives to the
primitive @racket[field] declaration.

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





@section{Supporting Standard Interfaces}
@(declare-exporting ricoeur/tei/kernel/lang/specification-lang)
@defform[(declare-resp-field option ...)
         #:grammar [(option (code:line attributes-expr (code:comment "required"))
                            (code:line #:key key-id))]
         #:contracts ([attributes-expr (listof (list/c symbol? string-immutable/c))])]{
 Declares and defines a @tech{TEI element struct field}
 (protected by lexical scope) and attaches assosciated
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

@defthing[prop:element->plain-text
          (struct-type-property/c
           (-> tei-element? string?))]{
 The definition of a @tech{TEI element struct type} can use
 @racket[prop:element->plain-text] to override the default
 behavior of @racket[element-or-xexpr->plain-text].
 Note that attaching @racket[prop:element->plain-text] to
 unrelated struct types has no effect: it is only used
 for @tech{TEI element structs}.

 The argument to the function given as the property value
 is always the @tech{TEI element struct} instance to be converted
 to plain text.
}

@defform[(declare-paragraphs-status-field value-expr)
         #:contracts ([value-expr guess-paragraphs-status/c])]{
 Declares a field to contain the value that should ultimately
 be returned by @racket[tei-document-paragraphs-status]
 and attaches an assosciated structure type property.
 The name of the field is protected by lexical scope.
 The details of this protocol are subject to change.
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

@defproc[(local-element-name)
         (or/c #f identifier?)]{
 This function is provided @racket[for-syntax].

 During the expansion of any sub-form of the @racket[_struct-type-def]
 non-terminal (see @secref["struct-type-def"]), returns the name
 of the @tech{TEI element struct type} being defined.
 In any other context, returns @racket[#false].

 The @racket[#:infer] option for @racket[field] is implemented using
 @racket[local-element-name].
}

