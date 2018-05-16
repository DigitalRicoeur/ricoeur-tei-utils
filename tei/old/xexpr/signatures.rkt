#lang racket/base

(require xml
         adjutor
         racket/contract
         racket/dict
         racket/pretty
         racket/class
         racket/match
         racket/unit
         (for-syntax racket/base
                     ))

(provide tei-xexpr-contracts^
         element-contracts^
         tei-xexpr-contracts@
         )

(define-signature tei-xexpr-contracts^
  ((contracted
    [tei-element-name/c flat-contract?]
    [tei-xexpr/c (-> tei-element-name/c flat-contract?)]
    [any-tei-xexpr/c flat-contract?]
    [get-body (-> (and/c list? xexpr/c)
                  (listof xexpr/c))]
    [get-attributes (-> (and/c list? xexpr/c)
                        (listof (list/c symbol? string?)))]
    [make-element-contract
     (->* {tei-element-name/c}
          {#:children (listof (list/c (or/c 1 '1+ '0-1 '0+)
                                      tei-element-name/c))
           #:text? any/c
           #:required-order (listof tei-element-name/c)
           #:attr-contracts (listof (list/c symbol? flat-contract?))
           #:required-attrs (listof symbol?)
           #:extra-check (or/c #f (-> (and/c list? xexpr/c)
                                      (or/c blame? #f)
                                      any/c
                                      any/c))}
          flat-contract?)]
    )))

(define-signature element-contracts^ 
  (TEI/c
   teiHeader/c fileDesc/c
   titleStmt/c title/c author/c editor/c
   publicationStmt/c authority/c availability/c
   sourceDesc/c bibl/c date/c
   profileDesc/c textClass/c catRef/c keywords/c term/c
   text/c body/c front/c back/c
   div/c pb/c tei:list/c sp/c
   ab/c p/c head/c note/c item/c
   ))


(define (body-in-order? l-child-element-names
                        order)
  (let body-in-order? ([l-child-element-names
                        l-child-element-names]
                       [order
                        (filter (λ (n) (memq n l-child-element-names))
                                order)])
    (or (null? order)
        (match l-child-element-names
          ['() #f]
          [(cons (? (λ (name) (eq? name (car order))))
                 more)
           (body-in-order? more (cdr order))]
          [(cons _ more)
           (body-in-order? more order)]))))


(define-unit tei-xexpr-contracts@
  (import element-contracts^)
  (export tei-xexpr-contracts^)
  (define tei-element-name/c
    (or/c 'TEI 'teiHeader 'fileDesc 'titleStmt 'title 'author 'editor
          'publicationStmt 'authority 'availability
          'sourceDesc 'bibl 'date
          'profileDesc 'textClass 'catRef 'keywords 'term
          'text 'body 'front 'back
          'div 'pb 'list 'sp 'ab 'p 'head 'note 'item))

  (define (tei-xexpr/c name)
    (case name
      [(TEI) TEI/c]
      [(teiHeader) teiHeader/c]
      [(fileDesc) fileDesc/c]
      [(titleStmt) titleStmt/c]
      [(title) title/c]
      [(author) author/c]
      [(editor) editor/c]
      [(publicationStmt) publicationStmt/c]
      [(authority) authority/c]
      [(availability) availability/c]
      [(sourceDesc) sourceDesc/c]
      [(bibl) bibl/c]
      [(date) date/c]
      [(profileDesc) profileDesc/c]
      [(textClass) textClass/c]
      [(catRef) catRef/c]
      [(keywords) keywords/c]
      [(term) term/c]
      [(text) text/c]
      [(body) body/c]
      [(front) front/c]
      [(back) back/c]
      [(div) div/c]
      [(pb) pb/c]
      [(list) tei:list/c]
      [(sp) sp/c]
      [(ab) ab/c]
      [(p) p/c]
      [(head) head/c]
      [(note) note/c]
      [(item) item/c]
      ))



  (define (make-element-contract name
                                 #:children [children '()]
                                 #:text? [text? #f]
                                 #:required-order [required-order '()]
                                 #:extra-check [extra-check #f]
                                 #:attr-contracts [attr-contracts '()]
                                 #:required-attrs [required-attrs '()])
    (new tei-element-contract%
         [name name]
         [children children]
         [text? text?]
         [required-order required-order]
         [extra-check extra-check]
         [attr-contracts attr-contracts]
         [required-attrs required-attrs]))



  (define (get-body tag)
    (match tag
      [(list-rest _ (or '() (cons (? list?) _)) body)
       body]
      [(cons _ body)
       body]))

  (define (get-attributes tag)
    (match tag
      [(list-rest _ (and attrs (or (cons (? list?) _)
                                   '())) body)
       attrs]
      [_
       '()]))


  (define-syntax-rule (define-lexical-member-names name ...)
    (begin (define-member-name name (generate-member-key)) ...))
  (define-lexical-member-names
    get-name
    first-order
    late-neg-projection
    stronger-than?
    )


  (define list-xexpr-late-neg-projection
    (get/build-late-neg-projection
     (and/c list? xexpr/c)))


  ;                                          
  ;                                          
  ;                                          
  ;                                          
  ;           ;;;;                           
  ;             ;;                           
  ;      ;;;    ;;      ;;      ;;      ;;   
  ;    ;;   ;   ;;     ;  ;   ;;  ;   ;;  ;  
  ;    ;        ;;        ;;   ;       ;     
  ;   ;;        ;;      ;;;;    ;;      ;;   
  ;    ;        ;;     ;  ;;      ;;      ;; 
  ;    ;;   ;    ;    ;;  ;;  ;   ;   ;   ;  
  ;      ;;;      ;;   ;;; ;   ;;;     ;;;   
  ;                                          
  ;                                          
  ;                                          
  ;                                          

  (define tei-element-contract%
    (class* object%
      [(interface* ()
                   ([prop:custom-write
                     contract-custom-write-property-proc]
                    [prop:flat-contract
                     (build-flat-contract-property
                      #:name (λ (this) (send this get-name))
                      #:first-order (λ (this) (send this first-order))
                      #:late-neg-projection
                      (λ (this) (send this late-neg-projection))
                      #:stronger (λ (this other)
                                   (send this stronger-than? other)))]))]
      (super-new)
      (init [(init:name name)]
            [(init:children children)]
            [(init:text? text?)]
            [(init:required-order required-order)]
            [(init:maybe-extra-check extra-check)]
            [(init:attr-contracts attr-contracts)]
            [(init:required-attrs required-attrs)])
      (def
        [name init:name]
        [text? init:text?]
        [required-order init:required-order]
        [maybe-extra-check init:maybe-extra-check]
        [required-attrs init:required-attrs])
      (define allowed-children
        (map cadr init:children))
      (define children-assocs
        (for/list ([raw (in-list init:children)])
          (match raw
            [(list rep name)
             (cons rep name)])))
      (define-values {dict:attr->contract
                      dict:attr->late-neg-projection}
        (for/lists {dict:attr->contract
                    dict:attr->late-neg-projection}
                   ([raw (in-list init:attr-contracts)])
          (match-define (list attr raw-c)
            raw)
          (define c
            (coerce-flat-contract 'tei-element-contract% raw-c))
          (values (cons attr c)
                  (cons attr (get/build-late-neg-projection c)))))
      (define name-late-neg-projection
        (get/build-late-neg-projection
         (coerce-flat-contract 'tei-element-contract% name)))
      (define name-contract-name
        (contract-name
         (coerce-flat-contract 'tei-element-contract% name)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define/public-final (get-name)
        `(tei-xexpr/c ,name-contract-name))
      (define/public-final (stronger-than? other)
        (or (equal? this other)
            (contract-stronger? any-tei-xexpr/c other))) ;<-------                                                                                     
      ;                                                                                          
      ;       ;;;    ;                     ;;                                 ;;                 
      ;     ;;       ;;                    ;;                                 ;;                 
      ;   ;;;;;;; ;;;;;   ;; ;;;    ;;   ;;;;;;;           ;;;    ;; ;;;   ;;;;;    ;;;   ;; ;;; 
      ;     ;;       ;;   ;;;     ;;  ;    ;;             ;   ;   ;;;     ;   ;;  ;;   ;  ;;;    
      ;     ;;       ;;   ;;       ;       ;;     ;;;;;;  ;   ;   ;;      ;   ;;  ;    ;  ;;     
      ;     ;;       ;;   ;;        ;;     ;;            ;;   ;;  ;;     ;;   ;; ;;;;;;;; ;;     
      ;     ;;       ;;   ;;          ;;   ;;             ;   ;   ;;      ;   ;;  ;       ;;     
      ;     ;;       ;;   ;;      ;   ;     ;             ;   ;   ;;      ;   ;;  ;;   ;  ;;     
      ;     ;;       ;;   ;;       ;;;       ;;;           ;;;    ;;       ;;; ;    ;;;   ;;     
      (define/public-final (first-order)
        (λ (val)
          (and (list? val)
               (xexpr? val)
               (first-order:check-attributes val)
               (let ([body (get-body val)])
                 (and (first-order:check-every-child val body)
                      (first-order:check-repetition-constraints val body)
                      (first-order:check-order val body)
                      (or (not maybe-extra-check)
                          (maybe-extra-check val #f #f)))))))
      ;   ;;;;             ;;                                            
      ;     ;;             ;;                                            
      ;     ;;      ;;   ;;;;;;;    ;;;           ;; ;      ;;;     ;;;;;
      ;     ;;     ;  ;    ;;     ;;   ;          ;;; ;   ;;   ;   ;  ;  
      ;     ;;        ;;   ;;     ;    ;  ;;;;;;  ;;  ;;  ;    ;  ;;  ;; 
      ;     ;;      ;;;;   ;;    ;;;;;;;;         ;;  ;; ;;;;;;;;  ;  ;  
      ;     ;;     ;  ;;   ;;     ;               ;;  ;;  ;         ;;   
      ;      ;    ;;  ;;    ;     ;;   ;          ;;  ;;  ;;   ;  ;;     
      ;       ;;   ;;; ;     ;;;    ;;;           ;;  ;;    ;;;    ;;;;; 
      ;                                                           ;    ;;
      ;                                                          ;;    ; 
      ;                                                            ;;;;  
      (define/public-final (late-neg-projection)
        (λ (blame)
          (define list-xexpr-late-neg
            (list-xexpr-late-neg-projection (blame-add-context blame #f)))
          (define name-late-neg
            (name-late-neg-projection
             (blame-add-context blame "the name of")))
          (define dict:attr->late-neg
            (for/list ([pr (in-list dict:attr->late-neg-projection)])
              (match-define (cons attr proj)
                pr)
              (cons attr (proj (blame-add-context
                                blame
                                (format "the value of the ~e attribute of"
                                        attr))))))
          ;;;;
          (λ (val neg-party)
            ;check list + xexpr
            (list-xexpr-late-neg val neg-party)
            ;check name
            (name-late-neg (car val) neg-party)
            ;check attributes
            (late-neg:check-attributes blame
                                       val
                                       neg-party
                                       dict:attr->late-neg)
            (define body
              (get-body val))
            ;check every child
            (late-neg:check-every-child blame
                                        val
                                        neg-party
                                        body)
            ;check repetition constraints
            (late-neg:check-repetition-constraints blame
                                                   val
                                                   neg-party
                                                   body)
            ;check order
            (late-neg:check-order blame
                                  val
                                  neg-party
                                  body)
            ;extra check
            (when maybe-extra-check
              (maybe-extra-check val blame neg-party))
            ;success!
            val)))                                                                              
      ;           ;;                      ;;                       ;;      ;;                    
      ;           ;;                      ;;                       ;;      ;;                    
      ;      ;;;  ;; ;      ;;;      ;;;  ;;   ;;           ;;   ;;;;;;; ;;;;;;;  ;; ;;;    ;;   
      ;    ;;   ; ;;; ;   ;;   ;   ;;   ; ;;  ;            ;  ;    ;;      ;;     ;;;     ;;  ;  
      ;    ;      ;;  ;;  ;    ;   ;      ;; ;                ;;   ;;      ;;     ;;       ;     
      ;   ;;      ;;  ;; ;;;;;;;; ;;      ;;;;              ;;;;   ;;      ;;     ;;        ;;   
      ;    ;      ;;  ;;  ;        ;      ;;  ;            ;  ;;   ;;      ;;     ;;          ;; 
      ;    ;;   ; ;;  ;;  ;;   ;   ;;   ; ;;   ;          ;;  ;;    ;       ;     ;;      ;   ;  
      ;      ;;;  ;;  ;;    ;;;      ;;;  ;;   ;;          ;;; ;     ;;;     ;;;  ;;       ;;;   
      ;
      (define/private (combined:check-attributes val
                                                 maybe-blame
                                                 neg-party
                                                 dict:attr->late-neg)
        (let/ec return
          (define attrs
            (get-attributes val))
          ;check required attributes
          (for ([required (in-list required-attrs)]
                #:unless (assq required attrs))
            (if maybe-blame
                (raise-blame-error 
                 maybe-blame #:missing-party neg-party
                 val
                 '(expected:
                   "attributes including required attribute ~e"
                   given: "~e")
                 required
                 val)
                (return #f)))
          ;check attribute values satisfy contracts
          (for* ([spec (in-list attrs)]
                 [attr (in-value (car spec))]
                 [maybe-cntct/late-neg
                  (in-value (dict-ref (if maybe-blame
                                          dict:attr->late-neg
                                          dict:attr->contract)
                                      attr
                                      #f))]
                 #:when maybe-cntct/late-neg)
            (if maybe-blame
                (maybe-cntct/late-neg (cadr spec) neg-party)
                (unless ((flat-contract-predicate maybe-cntct/late-neg)
                         (cadr spec))
                  (return #f))))))
      (define/private (first-order:check-attributes val)
        (combined:check-attributes val #f #f #f))
      (define/private (late-neg:check-attributes blame
                                                 val
                                                 neg-party
                                                 dict:attr->late-neg)
        (combined:check-attributes val
                                   blame
                                   neg-party
                                   dict:attr->late-neg))                                                                                  
      ;                                                           ;;         ;    ;;;;        ;; 
      ;                                                           ;;         ;;     ;;        ;; 
      ;     ;;;  ;     ;    ;;;   ;; ;;; ;     ;             ;;;  ;; ;    ;;;;;     ;;     ;;;;; 
      ;   ;;   ;  ;   ;   ;;   ;  ;;;     ;   ;            ;;   ; ;;; ;      ;;     ;;    ;   ;; 
      ;   ;    ;  ;   ;   ;    ;  ;;      ;   ;            ;      ;;  ;;     ;;     ;;    ;   ;; 
      ;  ;;;;;;;;  ;  ;  ;;;;;;;; ;;       ;  ;           ;;      ;;  ;;     ;;     ;;   ;;   ;; 
      ;   ;        ; ;    ;       ;;       ; ;             ;      ;;  ;;     ;;     ;;    ;   ;; 
      ;   ;;   ;   ; ;    ;;   ;  ;;       ; ;             ;;   ; ;;  ;;     ;;      ;    ;   ;; 
      ;     ;;;     ;       ;;;   ;;        ;                ;;;  ;;  ;;     ;;       ;;   ;;; ; 
      ;                                     ;                                                    
      ;                                    ;                                                     
      ;                                  ;;
      (define/private (combined:check-every-child val body maybe-blame neg-party)
        (let/ec return
          (for ([child (in-list body)])
            (match child
              [(cons child-name _)
               ;check child is allowed
               (unless (memq child-name allowed-children)
                 (if maybe-blame
                     (raise-blame-error
                      maybe-blame #:missing-party neg-party
                      val
                      `(expected:
                        "only child elements in this list: ~a"
                        given: "~e"
                        "\n  context: ~e")
                      (pretty-format allowed-children)
                      child-name
                      val)
                     (return #f)))
               ;check child is valid
               (if maybe-blame
                   (((get/build-late-neg-projection
                      (tei-xexpr/c child-name))
                     (blame-add-context maybe-blame
                                        (format "a child ~a element of"
                                                child-name)))
                    child neg-party)
                   (unless ((tei-xexpr/c child-name) child)
                     (return #f)))]
              [(pregexp #px"^\\s*$")
               (void)]
              [bad
               ;check text?
               (unless text?
                 (if maybe-blame
                     (raise-blame-error
                      maybe-blame #:missing-party neg-party
                      val
                      '(expected:
                        "a body without textual content"
                        given:
                        "~e"
                        "\n  offending part: ~e")
                      val
                      bad)
                     (return #f)))]))))
      (define/private (first-order:check-every-child val body)
        (combined:check-every-child val body #f #f))
      (define/private (late-neg:check-every-child blame
                                                  val
                                                  neg-party
                                                  body)
        (combined:check-every-child val body blame neg-party))                           
      ;   ;; ;;;    ;;;   ; ;;      ;;   
      ;   ;;;     ;;   ;  ;;  ;   ;;  ;  
      ;   ;;      ;    ;  ;;  ;    ;     
      ;   ;;     ;;;;;;;; ;;  ;;    ;;   
      ;   ;;      ;       ;;  ;       ;; 
      ;   ;;      ;;   ;  ;;  ;   ;   ;  
      ;   ;;        ;;;   ;;;;     ;;;   
      ;                   ;;             
      ;                   ;;             
      ;                   ;;
      (define/private (combined:check-repetition-constraints val
                                                             body
                                                             maybe-blame
                                                             neg-party)
        (let/ec return
          (for ([pr (in-list children-assocs)])
            (match-define (cons rep child-name)
              pr)
            (unless (eq? rep '0+)
              (define count
                (length (for/list ([child (in-list body)]
                                   #:when (and (list? child)
                                               (eq? child-name (car child))))
                          child)))
              (case rep
                [(1 1+)
                 (when (= 0 count)
                   (if maybe-blame
                       (raise-blame-error
                        maybe-blame #:missing-party neg-party
                        val
                        '(expected:
                          "~a child element ~e"
                          given: "~e")
                        (case rep
                          [(1) "a"]
                          [(1+) "at least one"])
                        child-name
                        val)
                       (return #f)))])
              (case rep
                [(1 0-1)
                 (when (< 1 count)
                   (if maybe-blame
                       (raise-blame-error
                        maybe-blame #:missing-party neg-party
                        val
                        '(expected:
                          "~a one child element ~e"
                          given: "~e")
                        (case rep
                          [(1) "only"]
                          [(0-1) "at most"])
                        child-name
                        val)
                       (return #f)))])))))
      (define/private (first-order:check-repetition-constraints val body)
        (combined:check-repetition-constraints val
                                               body
                                               #f
                                               #f))
      (define/private (late-neg:check-repetition-constraints blame
                                                             val
                                                             neg-party
                                                             body)
        (combined:check-repetition-constraints val
                                               body
                                               blame
                                               neg-party))                                                                               
      ;                                                                                  
      ;   ;;                  ;;                                      ;;                 
      ;   ;;                  ;;                                      ;;                 
      ;   ;;;;     ;;;     ;;;;; ;     ;           ;;;    ;; ;;;   ;;;;;    ;;;   ;; ;;; 
      ;   ;;  ;   ;   ;   ;   ;;  ;   ;           ;   ;   ;;;     ;   ;;  ;;   ;  ;;;    
      ;   ;;  ;   ;   ;   ;   ;;  ;   ;           ;   ;   ;;      ;   ;;  ;    ;  ;;     
      ;   ;;  ;; ;;   ;; ;;   ;;   ;  ;          ;;   ;;  ;;     ;;   ;; ;;;;;;;; ;;     
      ;   ;;  ;   ;   ;   ;   ;;   ; ;            ;   ;   ;;      ;   ;;  ;       ;;     
      ;   ;;  ;   ;   ;   ;   ;;   ; ;            ;   ;   ;;      ;   ;;  ;;   ;  ;;     
      ;   ; ;;     ;;;     ;;; ;    ;              ;;;    ;;       ;;; ;    ;;;   ;;     
      ;                             ;                                                    
      ;                            ;                                                     
      ;                          ;;
      (define/private (first-order:check-order val body)
        (or (null? required-order)
            (body-in-order? (for/list ([ch (in-list body)]
                                       #:when (list? ch))
                              (car ch))
                            required-order)))
      (define/private (late-neg:check-order blame
                                            val
                                            neg-party
                                            body)
      
        (unless (first-order:check-order val body)
          (raise-blame-error
           blame #:missing-party neg-party
           val
           '(expected:
             "the following order for child elements (if present): ~a"
             given:
             "~e")
           (pretty-format required-order)
           val)))
      #|END class tei-element-contract%|#))


  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;     ;;    ;; ;   ;     ; 
  ;    ;  ;   ;;; ;   ;   ;  
  ;       ;;  ;;  ;;  ;   ;  
  ;     ;;;;  ;;  ;;   ;  ;  
  ;    ;  ;;  ;;  ;;   ; ;   
  ;   ;;  ;;  ;;  ;;   ; ;   
  ;    ;;; ;  ;;  ;;    ;    
  ;                     ;    
  ;                    ;     
  ;                  ;;      
  ;                          

  (define any-tei-xexpr/c
    (make-flat-contract
     #:name 'any-tei-xexpr/c
     #:list-contract? #t
     #:first-order
     (λ (val)
       (and (list? val)
            (xexpr? val)
            (let ([name (car val)])
              (tei-element-name/c name)
              ((flat-contract-predicate (tei-xexpr/c name)) val))))
     #:late-neg-projection
     (λ (blame)
       (define list-xexpr-late-neg
         (list-xexpr-late-neg-projection blame))
       (define name-late-neg
         ((get/build-late-neg-projection tei-element-name/c)
          (blame-add-context blame "the name of")))
       (λ (val neg-party)
         (list-xexpr-late-neg val neg-party)
         (let ([name (car val)])
           (name-late-neg name neg-party)
           (unless (tei-element-name/c name)
             (raise-blame-error
              blame #:missing-party neg-party
              val
              '(expected:
                "a TEI element"
                given: "~e")
              val))
           (((get/build-late-neg-projection (tei-xexpr/c name))
             (blame-add-context blame
                                (format "the ~a element case of"
                                        name)))
            val neg-party))))
     #:stronger
     (let ([listof-any/c (listof any/c)])
       (λ (this other)
         (or (equal? this other)
             (contract-stronger? xexpr/c other)
             (contract-stronger? xexpr? other)
             (contract-stronger? listof-any/c other))))))

  )
