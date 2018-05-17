#lang racket/base

(require ricoeur/tei/kernel/sans-lang
         ricoeur/tei/kernel/base-structs
         (submod ricoeur/tei/kernel/base-structs
                 private)
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
         field/derived
         lift-property/derived
         lift-methods/derived
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
  [lift-property lift-property/derived]
  [lift-methods lift-methods/derived])


(define-syntax-parser field/derived
  #:context (syntax-parse this-syntax
              [(_ original-datum _ ...)
               #'original-datum])
  [(_ original-datum
      name:id
      (~alt (~optional (~or* (~seq #:accessor (~or* accessor:id #f))
                             (~seq [#:accessor (~or* accessor:id #f)]))
                       #:name "#:accessor clause"
                       #:defaults ([accessor #'#f]))
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

(define-syntax-parameter get-field
  (λ (stx)
    (raise-syntax-error #f "element definition keyword used out of context" stx)))

(begin-for-syntax
  (define-syntax make-get-field-transformer
    (syntax-parser
      [(_)
       #'(λ (stx)
           (raise-syntax-error #f "no fields declared in this context" stx))]
      [(_ [plain-field:id accessor-id:id] ...+)
       #`(λ (stx)
           (define-syntax-class field-name
             #:description "field-name"
             #:attributes {accessor}
             (pattern (~datum plain-field)
                      #:with accessor #'accessor-id)
             ...)
           (syntax-parse stx
             [(_ field:field-name)
              #'field.accessor]
             [(_ field:field-name target:expr)
              #'(field.accessor target)]))])))


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
   #`(define-element-struct/derived #,this-syntax outer body ...)])

(define-syntax-parser define-element-struct/derived
  ;; Like define-element-struct/derived*, but blames
  ;; this-syntax (not original-datum) for bad outer-declarations.
  [(_ original-datum outer:outer-declarations
      body ...)
   #`(define-element-struct/derived* original-datum outer body ...)])


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
               [value-ids-so-far null]
               [bodies-so-far null])
      (match to-go
        ['()
         (values (reverse fields-so-far)
                 (reverse props-so-far)
                 (reverse methods-so-far)
                 (reverse value-ids-so-far)
                 (reverse bodies-so-far))]
        [(cons this to-go)
         (syntax-parse (local-expand this
                                     local-expand-context
                                     (list #'parsed-field
                                           #'parsed-lift-property
                                           #'parsed-lift-methods
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
                  value-ids-so-far
                  bodies-so-far)]
           [f:parsed-field-stx
            (loop to-go
                  (cons (attribute f.record)
                        fields-so-far)
                  props-so-far
                  methods-so-far
                  value-ids-so-far
                  bodies-so-far)]
           [p:parsed-lift-property-stx
            (loop to-go
                  fields-so-far
                  (cons (attribute p.parsed)
                        props-so-far)
                  methods-so-far
                  value-ids-so-far
                  bodies-so-far)]
           [m:parsed-lift-methods-stx
            (loop to-go
                  fields-so-far
                  props-so-far
                  (cons (attribute m.parsed)
                        methods-so-far)
                  value-ids-so-far
                  bodies-so-far)]
           [(define-values (id ...) _)
            (loop to-go
                  fields-so-far
                  props-so-far
                  methods-so-far
                  (append (syntax->list #'(id ...))
                          value-ids-so-far)
                  (cons this-syntax
                        bodies-so-far))]
           [_
            (loop to-go
                  fields-so-far
                  props-so-far
                  methods-so-far
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
           #:body/elements-only [body/elements-only-id (generate-temporary "body/elements-only")])
    #`(λ (raw-name-arg raw-attributes-arg raw-body-arg)
        ;; prevent set!
        (define-immutable #,name-arg-id raw-name-arg)
        (define-immutable #,attributes-arg-id raw-attributes-arg)
        (define-immutable #,body-arg-id raw-body-arg)
        #,@(if contains-text?
               null
               (list #`(define-immutable #,body/elements-only-id
                         (filter tei-element? raw-body-arg))))
        #,@body-forms-list
        (#,raw-constructor-id raw-name-arg raw-attributes-arg raw-body-arg
                              #,@(if contains-text?
                                     null
                                     (list body/elements-only-id))
                              #,@field-uses)))
  (define-syntax-class (constructor-spec #:contains-text? contains-text?
                                         #:raw-constructor raw-constructor-id
                                         #:element-name element-name)
    #:description "constructor spec"
    #:attributes {fields properties methods
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
                    )
              ...
              raw-body:expr ...]
             #:fail-when (and contains-text?
                              (attribute body/elements-only-clause))
             "not allowed for a text-containing element"
             #:do [(define-values {l-fields l-props l-methods l-value-ids l-bodies}
                     (expand-constructor-body
                      (syntax->list #'(raw-body ...))))]
             #:fail-when (check-duplicate-identifier
                          (map field-record-name l-fields))
             "duplicate field name"
             #:attr fields l-fields
             #:attr properties l-props
             #:attr methods l-methods
             #:with raw-constructor raw-constructor-id
             #:with wrapped-constructor-expr
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
              #:body/elements-only #'body/elements-only)
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
                        [constructor.raw-constructor raw-constructor-id]
                        [constructor.wrapped-constructor-expr
                         (make-wrapped-constructor-expr
                          #:contains-text? (attribute outer.contains-text?)
                          #:raw-constructor raw-constructor-id)]))
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
   #:with struct-name (format-id #f "~a-struct" #'outer.element-name)
   #:with (plain-field-name ...) l-field-names
   #:with (_ _ predicate-name struct-name-field-name ...)
   (build-struct-names #'struct-name l-field-names #f #t)
   #:with (lifted-prop-expr ...) (map lifted-property-prop lifted-properties)
   #:with (lifted-prop-val-expr ...) (map lifted-property-val lifted-properties)
   #`(begin
       (define-for-syntax get-field-transformer
         (make-get-field-transformer
          #,@(for/list ([field (in-list l-field-names)]
                        [accessor (in-syntax #'(struct-name-field-name ...))])
               #`[#,field #,accessor])))
       (define-struct/derived original-datum
         (struct-name #,(if (attribute outer.contains-text?)
                            #'content-containing-element
                            #'elements-only-element))
         (plain-field-name ...)
         #:transparent
         #:constructor-name constructor.raw-constructor
         #,@(apply
             append
             (for/list ([prop (in-syntax #'(lifted-prop-expr ... prop-expr ...))]
                        [val (in-syntax #'(lifted-prop-val-expr ... prop-val-expr ...))])
               (list #'#:property
                     prop
                     #`(syntax-parameterize ([get-field get-field-transformer])
                         #,val))))
         #,@(apply
             append
             (for/list ([m (in-list l-methods)])
               (match-define (lifted-methods gen body)
                 m)
               (list #'#:methods
                     gen
                     #`[(splicing-syntax-parameterize
                            ([get-field get-field-transformer])
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
   #'(begin (define tmp raw)
            (define-immutable new tmp))])


