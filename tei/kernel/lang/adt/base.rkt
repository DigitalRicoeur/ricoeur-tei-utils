#lang racket/base

(require ricoeur/tei/kernel/sans-lang
         ricoeur/tei/kernel/base-structs
         (submod ricoeur/tei/kernel/base-structs
                 private)
         "../stxparam.rkt"
         (submod "../stxparam.rkt" private)
         racket/contract
         racket/stxparam
         racket/splicing
         syntax/location
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     (for-syntax racket/base
                                 syntax/parse)
                     racket/syntax
                     syntax/flatten-begin
                     syntax/transformer
                     syntax/struct
                     syntax/contract
                     racket/match
                     racket/sequence
                     ))

(provide field
         get-field
         lift-property
         lift-methods
         lift-begin 
         field/derived
         get-field/derived
         lift-property/derived
         lift-methods/derived
         lift-begin/derived
         )

(module+ private
  (provide define-element-struct/derived
           ))

(module+ test
  (provide define-element-struct
           ))

;; field and other constructor sub-forms

(define-syntax-rule (define-derived [name name/derived] ...)
  (begin (define-syntax-parser name
           [(_ sub (... ...))
            #`(name/derived #,this-syntax sub (... ...))])
         ...))

(define-derived
  [field field/derived]
  [lift-begin lift-begin/derived]
  [lift-property lift-property/derived]
  [lift-methods lift-methods/derived])


(define-for-syntax (infer-accessor-id field-id)
  ;; Following the Check Syntax documentation.
  (let* ([first-part (symbol->string (syntax-e (local-element-name)))]
         [second-part (symbol->string (syntax-e field-id))]
         [first-len (string-length first-part)]
         [second-len (string-length second-part)]
         [hyphenated-id
          (format-id field-id
                     "~a-~a"
                     (local-element-name)
                     field-id
                     #:source field-id)])
    (syntax-property
     hyphenated-id
     'sub-range-binders
     (list
      (vector (syntax-local-introduce hyphenated-id)
              0 first-len 0.5 0.5
              (syntax-local-introduce (local-element-name))
              0 first-len 0.5 0.5)
      (vector (syntax-local-introduce hyphenated-id)
              (+ first-len 1) second-len 0.5 0
              (syntax-local-introduce field-id)
              0 second-len 0.5 1)))))
                     

(define-syntax-parser field/derived
  #:context (syntax-parse this-syntax
              [(_ original-datum _ ...)
               #'original-datum])
  [(_ original-datum
      name:id
      (~alt (~optional (~or* (~seq (~and accessor-kw #:accessor)
                                   (~or* raw-accessor:id #f))
                             (~seq [(~and accessor-kw #:accessor)
                                    (~or* raw-accessor:id #f)])
                             (~seq (~and infer? #:infer))
                             (~seq [(~and infer? #:infer)]))
                       #:name "#:accessor or#:infer  clause")
            (~optional (~or* (~seq #:check
                                   (~var check
                                         (expr/c #'contract?
                                                 #:name (fmt-check-name #'name))))
                             (~seq [#:check
                                    (~var check
                                          (expr/c #'contract?
                                                  #:name (fmt-check-name #'name)))])
                             (~seq [#:check]))
                       #:name "#:check clause"))
      ...)
   #:fail-when (and (not (local-element-name))
                    (attribute infer?))
   "#:infer specified, but local-element-name not initialized in context"
   #:fail-when (and (attribute infer?)
                    (attribute accessor-kw))
   "#:infer not compatible with explicit #:accessor option"
   #:with accessor (if (attribute infer?)
                       (infer-accessor-id #'name)
                       (or (attribute raw-accessor)
                           #'#f))
   #`(parsed-field original-datum
                   name
                   [#:check #,(if (attribute check.c)
                                  (syntax-local-lift-expression
                                   ;; would be nice to give better name to
                                   ;; anonymous functions
                                   #'check.c)
                                  #'#f)]
                   [#:accessor accessor])])
     
(begin-for-syntax
  (define (fmt-check-name name-stx)
    (format "#:check argument for ~v" (syntax->datum name-stx)))
  (struct field-record (name maybe-accessor maybe-check)
    #:transparent)
  (define-syntax-class parsed-field-stx
    #:literals {parsed-field}
    #:attributes {name record orig-datum}
    (pattern (parsed-field orig-datum
                           name:id
                           (~alt (~once (~seq [#:accessor (~or* accessor:id #f)]))
                                 (~once (~seq [#:check (~or* check:id #f)])))
                           ...)
             #:attr record (field-record #'name
                                         (attribute accessor)
                                         (attribute check)))))

(define-syntax-parser lift-begin/derived
  #:context (syntax-parse this-syntax
              [(_ orig-datum _ ...)
               #'orig-datum])
  [(_ orig-datum body:expr ...)
   #`(parsed-lift-begin orig-datum body ...)])

(begin-for-syntax
  (define-syntax-class parsed-lift-begin-stx
    #:literals {parsed-lift-begin}
    #:attributes {[body 1] orig-datum}
    (pattern (parsed-lift-begin orig-datum body:expr ...))))

(define-syntax-parser lift-property/derived
  #:context (syntax-parse this-syntax
              [(_ orig-datum _ ...)
               #'orig-datum])
  [(_ orig-datum prop:expr val:expr)
   #`(parsed-lift-property orig-datum prop val)])

(begin-for-syntax
  (struct lifted-property (prop val)
    #:transparent)
  (define-syntax-class parsed-lift-property-stx
    #:literals {parsed-lift-property}
    #:attributes {parsed orig-datum}
    (pattern (parsed-lift-property orig-datum prop:expr val:expr)
             #:attr parsed (lifted-property #'prop #'val))))


(define-syntax-parser lift-methods/derived
  #:context (syntax-parse this-syntax
              [(_ orig-datum _ ...)
               #'orig-datum])
  [(_ orig-datum gen:id [body:expr ...])
   #`(parsed-lift-methods orig-datum gen [body ...])])

(begin-for-syntax
  (struct lifted-methods (gen body)
    #:transparent)
  (define-syntax-class parsed-lift-methods-stx
    #:literals {parsed-lift-methods}
    #:attributes {parsed orig-datum}
    (pattern (parsed-lift-methods orig-datum gen:id [body:expr ...])
             #:attr parsed (lifted-methods #'gen
                                           (syntax->list
                                            #'(body ...)))))
  (define-splicing-syntax-class methods-clause
    #:description "#:methods clause"
    #:attributes {parsed}
    (pattern (~seq #:methods gen:id [body:expr ...])
             #:attr parsed (lifted-methods #'gen
                                           (syntax->list
                                            #'(body ...))))))


(define-syntax-rule (define-constructor-subforms [name class] ...)
  (begin
    (define-syntax (name stx)
      (syntax-parse stx
        [(~var it class)
         (raise-syntax-error
          #f "only allowed inside an element definition constructor spec"
          #'it.orig-datum)]
        [_
         (raise-syntax-error #f "bad syntax" stx)]))
    ...))
      
(define-constructor-subforms
  [parsed-field parsed-field-stx]
  [parsed-lift-begin parsed-lift-begin-stx]
  [parsed-lift-property parsed-lift-property-stx]
  [parsed-lift-methods parsed-lift-methods-stx])


;                                                                          
;                                                                          
;                                                                          
;                                                                          
;                    ;;                 ;;;    ;            ;;;;        ;; 
;                    ;;               ;;       ;;             ;;        ;; 
;     ;;;;;   ;;;  ;;;;;;;          ;;;;;;; ;;;;;     ;;;     ;;     ;;;;; 
;    ;  ;   ;;   ;   ;;               ;;       ;;   ;;   ;    ;;    ;   ;; 
;   ;;  ;;  ;    ;   ;;     ;;;;;;    ;;       ;;   ;    ;    ;;    ;   ;; 
;    ;  ;  ;;;;;;;;  ;;               ;;       ;;  ;;;;;;;;   ;;   ;;   ;; 
;     ;;    ;        ;;               ;;       ;;   ;         ;;    ;   ;; 
;   ;;      ;;   ;    ;               ;;       ;;   ;;   ;     ;    ;   ;; 
;    ;;;;;    ;;;      ;;;            ;;       ;;     ;;;       ;;   ;;; ; 
;   ;    ;;                                                                
;  ;;    ;                                                                 
;    ;;;;                                                                  
;                                                                          

(define-syntax-parser get-field
  [(_ f:id)
   #`(get-field/derived #,this-syntax f)]
  [(_ f:id target:expr)
   #`(get-field/derived #,this-syntax f target)])

(define-syntax-parser get-field/derived
  #:context (syntax-parse this-syntax
              [(_ orig-datum . _)
               #'orig-datum])
  [(_ orig-datum f:id)
   #'(core-get-field orig-datum f)]
  [(_ orig-datum f:id target:expr)
   #'((core-get-field orig-datum f) target)])

(define-syntax-parameter core-get-field
  (syntax-parser 
    [(_ orig-datum _)
     (raise-syntax-error
      #f "element struct definition keyword used out of context" #'orig-datum)]))

(define-syntax-parser with-get-field-context
  [(_ (~optional (~seq (~and splicing? #:splicing)))
      []
      body:expr ...)
   #:with stx-parameterize (if (attribute splicing?)
                               #'splicing-syntax-parameterize
                               #'syntax-parameterize)
   #'(stx-parameterize
      ([core-get-field core-get-field:none-in-context])
      body ...)]
  [(_ (~optional (~seq (~and splicing? #:splicing)))
      [(field:id accessor-id:id) ...+]
      body:expr ...)
   #:with (stx-parameterize let-stx)
   (if (attribute splicing?)
       #'(splicing-syntax-parameterize splicing-let-syntax)
       #'(syntax-parameterize let-syntax))
   #'(let-stx
      ([field (field-to-get-transformer #'accessor-id)] ...)
      (stx-parameterize
       ([core-get-field core-get-field:lookup])
       body ...))])

(begin-for-syntax
  (struct field-to-get-transformer (accessor-id)
    #:property prop:procedure
    (λ (this stx) 
      (raise-syntax-error
       #f "field name used out of context" stx)))
  (define core-get-field:none-in-context
    (syntax-parser
      [(_ orig-datum _)
       (raise-syntax-error
        #f "no fields declared in this context" #'orig-datum)]))
  (define (core-get-field:lookup stx)
    (syntax-parse stx
      #:context (syntax-parse stx
                  [(_ orig-datum _)
                   #'orig-datum])
      [(_ orig-datum f:id)
       (with-disappeared-uses
        (let ([v (syntax-local-value/record #'f field-to-get-transformer?)])
          (if v
              (field-to-get-transformer-accessor-id v)
              (raise-syntax-error
               #f "not defined as a field" #'orig-datum #'f))))])))


;                                          
;                                          
;                                          
;                                          
;                                          
;                                          
;     ;;    ;;  ;;    ;;;;;   ;;    ;; ;;; 
;   ;;  ;   ;;  ;;   ;  ;    ;  ;   ;;;    
;    ;      ;;  ;;  ;;  ;;      ;;  ;;     
;     ;;    ;;  ;;   ;  ;     ;;;;  ;;     
;       ;;  ;;  ;;    ;;     ;  ;;  ;;     
;   ;   ;    ; ;;;  ;;      ;;  ;;  ;;     
;    ;;;      ; ;;   ;;;;;   ;;; ;  ;;     
;                   ;    ;;                
;                  ;;    ;                 
;                    ;;;;                  
;                                          

(begin-for-syntax
  (define-syntax-class outer-declarations
    #:description "outer declarations"
    #:attributes {element-name wrapped-constructor-name contains-text?}
    (pattern [(~alt (~once (~seq #:element-name element-name:id))
                    (~once (~seq #:wrapped-constructor-name wrapped-constructor-name:id))
                    (~once (~seq (~or* (~and text-kw #:contains-text)
                                       #:elements-only))))
              ...]
             #:attr contains-text? (not (not (attribute text-kw))))))
                        
(define-syntax-parser define-element-struct
  [(_ outer:outer-declarations body ...)
   #`(splicing-syntax-parameterize-local-element-name
      outer.element-name
      (define-element-struct/derived* #,this-syntax outer body ...))])

(define-syntax-parser define-element-struct/derived
  ;; Like define-element-struct/derived*, but blames
  ;; this-syntax (not original-datum) for bad outer-declarations.
  [(_ original-datum outer:outer-declarations
      body ...)
   #`(splicing-syntax-parameterize-local-element-name
      outer.element-name
      (define-element-struct/derived* original-datum outer body ...))])


;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;                                    ;;                              ;;                    
;                                    ;;                              ;;                    
;      ;;;   ;;;    ;; ;      ;;   ;;;;;;;  ;; ;;;  ;;  ;;     ;;; ;;;;;;;   ;;;    ;; ;;; 
;    ;;   ; ;   ;   ;;; ;   ;;  ;    ;;     ;;;     ;;  ;;   ;;   ;  ;;     ;   ;   ;;;    
;    ;      ;   ;   ;;  ;;   ;       ;;     ;;      ;;  ;;   ;       ;;     ;   ;   ;;     
;   ;;     ;;   ;;  ;;  ;;    ;;     ;;     ;;      ;;  ;;  ;;       ;;    ;;   ;;  ;;     
;    ;      ;   ;   ;;  ;;      ;;   ;;     ;;      ;;  ;;   ;       ;;     ;   ;   ;;     
;    ;;   ; ;   ;   ;;  ;;  ;   ;     ;     ;;       ; ;;;   ;;   ;   ;     ;   ;   ;;     
;      ;;;   ;;;    ;;  ;;   ;;;       ;;;  ;;        ; ;;     ;;;     ;;;   ;;;    ;;     
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          

(define (filter-whitespace lst)
  (for/list ([v (in-list lst)]
             #:unless (and (string? v)
                           (regexp-match? #px"^\\s*$" v)))
    v))

(define (filter-elements-only lst)
  (define new
    (filter tei-element? lst))
  (if (equal? lst new)
      lst
      new))

(define-syntax-parameter local-this/thunk
  (λ (stx)
    (raise-syntax-error #f "used out of context" stx)))


(begin-for-syntax
  (struct context-value ()
    #:property prop:liberal-define-context #t)
  (define (expand-constructor-body to-go)
    (define local-expand-context
      (list (context-value)))
    (let loop ([to-go to-go]
               [fields-so-far null]
               [props-so-far null]
               [methods-so-far null]
               [begin-bodies-so-far null]
               [value-ids-so-far null]
               [bodies-so-far null])
      (match to-go
        ['()
         (values (reverse fields-so-far)
                 (reverse props-so-far)
                 (reverse methods-so-far)
                 begin-bodies-so-far ;; in order
                 (reverse value-ids-so-far)
                 (reverse bodies-so-far))]
        [(cons this to-go)
         (syntax-parse (local-expand this
                                     local-expand-context
                                     (list #'parsed-field
                                           #'parsed-lift-property
                                           #'parsed-lift-methods
                                           #'parsed-lift-begin
                                           #'define-values
                                           #'define-syntaxes
                                           #'begin ;; it's implicitly added, but let's be clear
                                           ))
           #:literals {begin define-values}
           [(begin nested:expr ...)
            (loop (append (flatten-all-begins
                           #'(begin nested ...))
                          to-go)
                  fields-so-far
                  props-so-far
                  methods-so-far
                  begin-bodies-so-far
                  value-ids-so-far
                  bodies-so-far)]
           [f:parsed-field-stx
            (loop to-go
                  (cons (attribute f.record)
                        fields-so-far)
                  props-so-far
                  methods-so-far
                  begin-bodies-so-far
                  value-ids-so-far
                  bodies-so-far)]
           [p:parsed-lift-property-stx
            (loop to-go
                  fields-so-far
                  (cons (attribute p.parsed)
                        props-so-far)
                  methods-so-far
                  begin-bodies-so-far
                  value-ids-so-far
                  bodies-so-far)]
           [m:parsed-lift-methods-stx
            (loop to-go
                  fields-so-far
                  props-so-far
                  (cons (attribute m.parsed)
                        methods-so-far)
                  begin-bodies-so-far
                  value-ids-so-far
                  bodies-so-far)]
           [b:parsed-lift-begin-stx
            (loop to-go
                  fields-so-far
                  props-so-far
                  methods-so-far
                  (append begin-bodies-so-far
                          (syntax->list #'(b.body ...)))
                  value-ids-so-far
                  bodies-so-far)]
           [(define-values (id ...) _)
            (loop to-go
                  fields-so-far
                  props-so-far
                  methods-so-far
                  begin-bodies-so-far
                  (append (syntax->list #'(id ...))
                          value-ids-so-far)
                  (cons this-syntax
                        bodies-so-far))]
           [_
            (loop to-go
                  fields-so-far
                  props-so-far
                  methods-so-far
                  begin-bodies-so-far
                  value-ids-so-far
                  (cons this-syntax
                        bodies-so-far))])])))
  (define (make:field->ctor-arg element-name l-value-ids)
    (define (dat v)
      (datum->syntax #'contract v))
    (define (q v)
      (dat (format "~v" (syntax->datum v))))
    (match-lambda
      [(field-record name _ maybe-check)
       (unless (member name l-value-ids bound-identifier=?)
         (raise-syntax-error
          #f "unbound field name in element definition constructor spec"
          name))
       (if maybe-check
           #`(contract #,maybe-check
                       #,name
                       ;; Ideally want to include the name of the module
                       ;; that defined the field/derived syntax here as well.
                       '(element-constructor #,(q element-name)
                                             #:field #,(q name))
                       (quote-module-name)
                       '(field #,(q name))
                       #'#,name)
           name)]))
  (define (make-wrapped-constructor-expr
           #:contains-text? contains-text?
           #:raw-constructor raw-constructor-id
           #:field-uses [field-uses null]
           #:body-forms-list [body-forms-list null]
           #:name-arg [name-arg-id (generate-temporary "name-arg")]
           #:attributes-arg [attributes-arg-id (generate-temporary "attributes-arg")]
           #:body-arg [body-arg-id (generate-temporary "body-arg")]
           #:this/thunk [this/thunk-id #f]
           #:body/elements-only [body/elements-only-id (generate-temporary "body/elements-only")])
    #`(λ (raw-name-arg raw-attributes-arg raw-body-arg)
        ;; prevent set!
        (define-immutable #,name-arg-id raw-name-arg)
        (define-immutable #,attributes-arg-id raw-attributes-arg)
        (define-immutable #,body-arg-id
          #,(if contains-text?
                #'raw-body-arg
                #'(filter-whitespace raw-body-arg)))
        #,(if contains-text?
              #'(begin)
              #`(define-immutable #,body/elements-only-id
                  (filter-elements-only #,body-arg-id)))
        #,(if this/thunk-id
              #`(define-immutable #,this/thunk-id
                  local-this/thunk)
              #'(begin))
        #,@body-forms-list
        (#,raw-constructor-id raw-name-arg raw-attributes-arg #,body-arg-id
                              #,@(if contains-text?
                                     null
                                     (list body/elements-only-id))
                              #,@field-uses)))
  (define-syntax-class (constructor-spec #:contains-text? contains-text?
                                         #:raw-constructor raw-constructor-id
                                         #:element-name element-name)
    #:description "constructor spec"
    #:attributes {fields properties methods [lift-begin-body 1]
                         wrapped-constructor-expr raw-constructor}
    (pattern [(~alt (~optional (~seq #:name name-arg:id)
                               #:name "name arg binding"
                               #:defaults ([name-arg
                                            (generate-temporary "name-arg")]))
                    (~optional (~seq #:attributes attributes-arg:id)
                               #:name "attributes arg binding"
                               #:defaults ([attributes-arg
                                            (generate-temporary "attributes-arg")]))
                    (~optional (~seq #:body body-arg:id)
                               #:name "body arg binding"
                               #:defaults ([body-arg
                                            (generate-temporary "body-arg")]))
                    (~optional (~and (~seq #:body/elements-only body/elements-only:id)
                                     (~seq body/elements-only-clause ...))
                               #:name "body/elements-only binding"
                               #:defaults ([body/elements-only
                                            (generate-temporary "body/elements-only")]))
                    (~optional (~seq #:this/thunk this/thunk:id)
                               #:name "this/thunk binding")
                    )
              ...
              raw-body:expr ...]
             #:fail-when (and contains-text?
                              (attribute body/elements-only-clause))
             "not allowed for a text-containing element"
             #:do [(define-values {l-fields l-props l-methods l-begin-bodies l-value-ids l-bodies}
                     (expand-constructor-body
                      (syntax->list #`(raw-body ...))))]
             #:fail-when (check-duplicate-identifier
                          (map field-record-name l-fields))
             "duplicate field name"
             #:attr fields l-fields
             #:attr properties l-props
             #:attr methods l-methods
             #:with (lift-begin-body ...) l-begin-bodies
             #:with raw-constructor raw-constructor-id
             #:with basic-wraped-ctor
             (make-wrapped-constructor-expr
              #:contains-text? contains-text?
              #:raw-constructor raw-constructor-id
              #:body-forms-list l-bodies
              #:field-uses (map (make:field->ctor-arg element-name
                                                      l-value-ids)
                                l-fields)
              #:name-arg #'name-arg
              #:attributes-arg #'attributes-arg
              #:body-arg #'body-arg
              #:this/thunk (attribute this/thunk)
              #:body/elements-only #'body/elements-only)
             #:with wrapped-constructor-expr
             (if (attribute this/thunk)
                 #`(λ (name attrs body)
                     (define rslt
                       (syntax-parameterize
                           ([local-this/thunk (λ (stx)
                                                #'(λ () rslt))])
                         (basic-wraped-ctor name attrs body)))
                     rslt)
                 #'basic-wraped-ctor)
             #|END define-syntax-class constructor-spec|#)))

                                       

;                                                  
;                                                  
;                                                  
;                                                  
;       ;;              ;;;    ;                   
;       ;;            ;;       ;;                  
;    ;;;;;    ;;;   ;;;;;;; ;;;;;   ;; ;      ;;;  
;   ;   ;;  ;;   ;    ;;       ;;   ;;; ;   ;;   ; 
;   ;   ;;  ;    ;    ;;       ;;   ;;  ;;  ;    ; 
;  ;;   ;; ;;;;;;;;   ;;       ;;   ;;  ;; ;;;;;;;;
;   ;   ;;  ;         ;;       ;;   ;;  ;;  ;      
;   ;   ;;  ;;   ;    ;;       ;;   ;;  ;;  ;;   ; 
;    ;;; ;    ;;;     ;;       ;;   ;;  ;;    ;;;  
;                                                  
;                                                  
;                                                  
;                                                  

(define-syntax-parser define-element-struct/derived*
  ;; The "sugar" macro that expands to me is responsible
  ;; for splicing-syntax-parameterize-local-element-name
  #:context (syntax-parse this-syntax
              [(_ original-datum _ ...)
               #'original-datum])
  [(_ original-datum outer:outer-declarations
      (~do (define raw-constructor-id
             (generate-temporary
              (format-symbol "~a:raw-constructor"
                             #'outer.element-name))))
      (~alt (~optional (~seq #:predicate predicate-external-name:id)
                       #:name "#:predicate clause")
            (~optional (~seq #:constructor
                             (~var constructor (constructor-spec
                                                #:element-name #'outer.element-name
                                                #:raw-constructor raw-constructor-id
                                                #:contains-text?
                                                (attribute outer.contains-text?))))
                       #:name "#:constructor clause"
                       #:defaults
                       ([constructor.fields null]
                        [constructor.properties null]
                        [constructor.methods null]
                        [[constructor.lift-begin-body 1] null]
                        [constructor.raw-constructor raw-constructor-id]
                        [constructor.wrapped-constructor-expr
                         (make-wrapped-constructor-expr
                          #:contains-text? (attribute outer.contains-text?)
                          #:raw-constructor raw-constructor-id)]))
            (~optional (~seq #:begin
                             (~describe "parenthesized sequence of definitions and expressions"
                                        [kw-begin-body:expr ...]))
                       #:name "#:begin clause"
                       #:defaults
                       ([[kw-begin-body 1] null]))
            (~seq #:property prop-expr:expr prop-val-expr:expr)
            outer-methods:methods-clause
            )
      ...)
   #:do [(define fields
           (attribute constructor.fields))
         (define lifted-properties
           (attribute constructor.properties))
         (define l-methods
           (append (attribute constructor.methods)
                   (attribute outer-methods.parsed)))
         (define l-field-names
           (map field-record-name fields))]
   #:with (field-name ...) l-field-names
   #:with struct-name (format-id #f "tei-~a-struct" #'outer.element-name)
   #:with (plain-struct-field-name ...)
   (for/list ([name (in-list l-field-names)]
              [i (in-naturals)])
     (format-id name "~a:~a" i name))
   #:with (_ _ predicate-name struct-name-field-name ...)
   (build-struct-names #'struct-name
                       (syntax->list #'(plain-struct-field-name ...))
                       #f
                       #t)
   #:with (lifted-prop-expr ...) (map lifted-property-prop lifted-properties)
   #:with (lifted-prop-val-expr ...) (map lifted-property-val lifted-properties)
   #`(begin
       (define-struct/derived original-datum
         (struct-name #,(if (attribute outer.contains-text?)
                            #'content-containing-element
                            #'elements-only-element))
         (plain-struct-field-name ...)
         #:transparent
         #:constructor-name constructor.raw-constructor
         #,@(apply
             append
             (for/list ([prop (in-syntax #'(lifted-prop-expr ... prop-expr ...))]
                        [val (in-syntax #'(lifted-prop-val-expr ... prop-val-expr ...))])
               (list #'#:property
                     prop
                     #`(with-get-field-context
                        ([field-name struct-name-field-name] ...)
                        #,val))))
         #,@(apply
             append
             (for/list ([m (in-list l-methods)])
               (match-define (lifted-methods gen body)
                 m)
               (list #'#:methods
                     gen
                     #`[(with-get-field-context
                         #:splicing
                         ([field-name struct-name-field-name] ...)
                         #,@body)])))
         #|END define-struct/derived|#)
       
       #,@(if (attribute predicate-external-name)
              (list #`(define-immutable predicate-external-name predicate-name))
              null)
       
       (define-accessors
         #,@(for/list ([accessor (in-list (map field-record-maybe-accessor fields))]
                       [raw-accessor (in-syntax #'(struct-name-field-name ...))]
                       #:when accessor)
              #`[#,accessor #,raw-accessor]))

       (with-get-field-context
        #:splicing
        ([field-name struct-name-field-name] ...)
        kw-begin-body ... 
        constructor.lift-begin-body ...)
       
       (define-immutable outer.wrapped-constructor-name
         constructor.wrapped-constructor-expr)
       
       #|END define-element-struct/derived|#)])


;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                          
;   ;;              ;;;;                                                                                   
;   ;;                ;;                                                                                   
;   ;; ;      ;;;     ;;    ; ;;      ;;;   ;; ;;;         ; ;; ;;    ;;       ;;;  ;; ;;;   ;;;      ;;   
;   ;;; ;   ;;   ;    ;;    ;;  ;   ;;   ;  ;;;            ;; ;; ;   ;  ;    ;;   ; ;;;     ;   ;   ;;  ;  
;   ;;  ;;  ;    ;    ;;    ;;  ;   ;    ;  ;;             ;; ;; ;;     ;;   ;      ;;      ;   ;    ;     
;   ;;  ;; ;;;;;;;;   ;;    ;;  ;; ;;;;;;;; ;;             ;; ;; ;;   ;;;;  ;;      ;;     ;;   ;;    ;;   
;   ;;  ;;  ;         ;;    ;;  ;   ;       ;;             ;; ;; ;;  ;  ;;   ;      ;;      ;   ;       ;; 
;   ;;  ;;  ;;   ;     ;    ;;  ;   ;;   ;  ;;             ;; ;; ;; ;;  ;;   ;;   ; ;;      ;   ;   ;   ;  
;   ;;  ;;    ;;;       ;;  ;;;;      ;;;   ;;             ;; ;; ;;  ;;; ;     ;;;  ;;       ;;;     ;;;   
;                           ;;                                                                             
;                           ;;                                                                             
;                           ;;                                                                             
;                                                                                                          

(define-syntax-parser define-accessors
  [(_ [external-name:id raw-accessor:id] ...)
   #`(begin
       (define-immutable external-name raw-accessor)
       ...)])

(define-syntax-parser define-immutable
  [(_ new:id old:id)
   #'(define-syntax new
       (make-variable-like-transformer #'old))]
  [(_ new:id raw:expr)
   #:with tmp (generate-temporary #'new)
   #'(begin (define tmp raw)
            (define-immutable new tmp))])


