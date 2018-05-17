#lang racket/base

(require ricoeur/tei/kernel/sans-lang
         ricoeur/tei/kernel/base-structs
         (submod ricoeur/tei/kernel/base-structs
                 private)
         racket/contract
         racket/stxparam
         racket/splicing
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     (for-syntax racket/base
                                 syntax/parse)
                     racket/syntax
                     syntax/flatten-begin
                     syntax/transformer
                     syntax/struct
                     racket/match
                     racket/sequence
                     ))

(provide field
         field/derived
         get-field
         lift-property
         lift-methods
         )

(module+ private
  (provide define-element-struct/derived
           ))

(module+ test
  (provide define-element-struct
           ))

;; field and other constructor sub-forms

(define-syntax-parser field
  [(_ sub ...)
   #`(field/derived #,this-syntax sub ...)])


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
                                  (syntax-local-lift-expression #'check.c)
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

(define-syntax (parsed-field stx)
  (raise-syntax-error
   #f "only allowed inside an element definition constructor spec"
   (syntax-parse stx
     [f:parsed-field-stx
      #'f.orig-datum]
     [_ stx])))

(define-syntax-rule (define-constructor-subforms name ...)
  (begin
    (define-for-syntax (constructor-subform-error stx)
      (raise-syntax-error
       #f "only allowed inside an element definition constructor spec"
       stx))
    (define-syntax name constructor-subform-error) ...))

(define-constructor-subforms lift-property lift-methods)


(begin-for-syntax
  (struct lifted-property (prop val)
    #:transparent)
  (define-syntax-class lift-property-stx
    #:literals {lift-property}
    #:attributes {parsed}
    (pattern (lift-property prop:expr val:expr)
             #:attr parsed (lifted-property #'prop #'val)))
  (struct lifted-methods (gen body)
    #:transparent)
  (define-syntax-class lift-methods-stx
    #:literals {lift-methods}
    #:attributes {parsed}
    (pattern (lift-methods gen:id [body:expr ...])
             #:attr parsed (lifted-methods #'gen
                                           (syntax->list
                                            #'(body ...)))))
  (define-splicing-syntax-class methods-clause
    #:description "#:methods clause"
    #:attributes {parsed}
    (pattern (~seq #:methods gen:id [body:expr ...])
             #:attr parsed (lifted-methods #'gen
                                           (syntax->list
                                            #'(body ...)))))
  #|END begin-for-syntax|#)


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
      '(my-local-expand-context)
      #;(let ([existing
             (syntax-local-context)])
        (println existing)
        (if (list? existing)
            (cons (context-value) existing)
            (list (context-value)))))
    (let loop ([to-go to-go]
               [fields-so-far null]
               [props-so-far null]
               [methods-so-far null]
               [bodies-so-far null])
      (match to-go
        ['()
         (values (reverse fields-so-far)
                 (reverse props-so-far)
                 (reverse methods-so-far)
                 (reverse bodies-so-far))]
        [(cons this to-go)
         (syntax-parse (local-expand this
                                     local-expand-context
                                     (list #'parsed-field
                                           #'lift-property
                                           #'lift-methods
                                           #'begin ;; it's implicitly added, but let's be clear
                                           ))
           #:literals {begin}
           [(begin nested:expr ...)
            (loop (append (flatten-all-begins
                           #'(begin nested ...))
                          to-go)
                  fields-so-far
                  props-so-far
                  methods-so-far
                  bodies-so-far)]
           [f:parsed-field-stx
            (loop to-go
                  (cons (attribute f.record)
                        fields-so-far)
                  props-so-far
                  methods-so-far
                  bodies-so-far)]
           [p:lift-property-stx
            (loop to-go
                  fields-so-far
                  (cons (attribute p.parsed)
                        props-so-far)
                  methods-so-far
                  bodies-so-far)]
           [m:lift-methods-stx
            (loop to-go
                  fields-so-far
                  props-so-far
                  (cons (attribute m.parsed)
                        methods-so-far)
                  bodies-so-far)]
           [other
            (loop to-go
                  fields-so-far
                  props-so-far
                  methods-so-far
                  (cons #'other
                        bodies-so-far))])])))
  (define-syntax-class (constructor-spec #:contains-text? contains-text?
                                         #:element-name element-name)
    #:description "constructor spec"
    #:attributes {fields raw-constructor wrapped-constructor-expr
                         properties methods}
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
             #:do [(define-values {l-fields l-props l-methods l-bodies}
                     (expand-constructor-body
                      (syntax->list #'(raw-body ...))))]
             #:fail-when (check-duplicate-identifier
                          (map field-record-name l-fields))
             "duplicate field name"
             #:attr fields l-fields
             #:attr properties l-props
             #:attr methods l-methods
             #:with (body ...) l-bodies
             #:with raw-constructor (generate-temporary
                                     (format-symbol "~a:raw-constructor"
                                                    element-name))
             #:with wrapped-constructor-expr
             #`(λ (name-arg attributes-arg body-arg)
                 #,@(if contains-text?
                        null
                        (list #'(define body/elements-only
                                  (filter tei-element? body-arg))))
                 body ...
                 (raw-constructor name-arg attributes-arg body-arg
                                  #,@(if contains-text?
                                         null
                                         (list #'body/elements-only))
                                  ;; TODO: use the check contract when present.
                                  ;; TODO: want to be able to catch
                                  ;; undefined fields.
                                  ;; This might mean I need to add a scope
                                  ;; with make-syntax-introducer or something
                                  ;; to make sure they not only are defined,
                                  ;; but are defined in the bodies.
                                  #,@(map field-record-name l-fields)))
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
      (~alt (~optional (~seq #:predicate predicate-external-name:id)
                       #:name "#:predicate clause")
            (~once (~seq #:constructor
                         (~var constructor (constructor-spec
                                            #:element-name #'outer.element-name
                                            #:contains-text?
                                            (attribute outer.contains-text?))))
                   #:name "#:constructor clause")
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
   #|#:with predicate-name (format-id #f "~a?" #'struct-name)
   #:with (struct-name-field-name ...)
   (map (λ (f) (format-id #f "~a-~a" #'struct-name (field-record-name f)))
        fields)|#
   #`(begin
       (define-syntax get-field-transformer
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
              (list #`(define-syntax predicate-external-name
                        (make-variable-like-transformer #'predicate-name)))
              null)
       (define-accessors
         #,@(for/list ([accessor (in-list (map field-record-maybe-accessor fields))]
                       [raw-accessor (in-syntax #'(struct-name-field-name ...))]
                       #:when accessor)
              #`[#,accessor #,raw-accessor]))
       (define outer.wrapped-constructor-name
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
       (define-syntax external-name
         (make-variable-like-transformer #'raw-accessor))
       ...)])




