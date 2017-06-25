#lang racket/base

(require racket/contract
         racket/match
         racket/string
         xml/xexpr
         )

(provide tei-xexpr/c
         any-tei-xexpr/c
         )

(define/contract (tei-xexpr/c name) ;case-lambda
  (-> symbol? flat-contract?)
  (case name
    [(TEI.2) TEI.2/c]
    [(teiHeader) teiHeader/c]
    [(fileDesc) fileDesc/c]
    [(titleStmt) titleStmt/c]
    [(publicationStmt) publicationStmt/c]
    [(sourceDesc) sourceDesc/c]
    [(availability) availability/c]
    [(pb) pb/c]
    [(front back body div text list)
     (get-tag-contract/other/elements-only name)]
    [else
     (get-tag-contract/else name)]))

(define (get-body tag)
  (match tag
    [(list-rest _ (cons (? list?) _) body)
     body]
    [(cons _ body)
     body]))

(define (body-in-order? l-child-element-names
                        order)
  (let body-in-order?
    ([l-child-element-names
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

(define any-tei-xexpr/c
  (make-flat-contract
   #:name 'any-tei-xexpr/c
   #:first-order
   (λ (val)
     (and (list? val)
          (xexpr? val)
          ((flat-contract-predicate
            (tei-xexpr/c (car val)))
           val)))
   #:late-neg-projection
   (λ (blame)
     (define list/xexpr-late-neg
       ((get/build-late-neg-projection
         (and/c list? xexpr/c))
        blame))
     (λ (val neg-party)
       (list/xexpr-late-neg val neg-party)
       (((get/build-late-neg-projection
          (tei-xexpr/c (car val)))
         blame)
        val neg-party)))
   #:stronger
   (λ (this other)
     (or (equal? this other)
         (contract-stronger? xexpr/c other)
         (contract-stronger? xexpr? other)
         (contract-stronger? (listof any/c) other)))))
         

(define-struct/contract tei-xexpr-contract
  ([name symbol?]
   [child-tag/c flat-contract?]
   [l-required-children (listof symbol?)]
   [elements-only? boolean?]
   [maybe-required-order (or/c #f (listof symbol?))]
   )
  #:transparent
  #:property prop:custom-write
  contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (this)
     `(tei-xexpr/c ,(contract-name
                     (coerce-flat-contract
                      'tei-xexpr-contract
                      (tei-xexpr-contract-name this)))))
   #:first-order
   (match-lambda
     [(tei-xexpr-contract name
                          child-tag/c
                          l-required-children
                          elements-only?
                          maybe-required-order
                          )
      (define child-tag/c-pred
        (flat-contract-predicate child-tag/c))
      (λ (val)
        (let/ec return
          (unless (and
                   ;check list + xexpr
                   (list? val)
                   (xexpr? val)
                   ;check name
                   (eq? name (car val)))
            (return #f))
          (define body
            (get-body val))
          (for ([child (in-list body)])
            (match child
              [(cons name _)
               ;; check child-tag/c and that child is valid
               (unless (and (child-tag/c-pred name)
                            ((flat-contract-predicate
                              (tei-xexpr/c name))
                             child))
                 (return #f))]
              [(pregexp #"^\\s*$")
               (void)]
              [_
               ; check elements-only?
               (when elements-only?
                 (return #f))]))
          (when (and (null? l-required-children)
                     (not maybe-required-order))
            (return #t))
          (define l-child-element-names
            (for/list ([child (in-list body)]
                       #:when (list? child))
              (car child)))
          ;check l-required-children
          (for ([required (in-list l-required-children)]
                #:unless (memq required
                               l-child-element-names))
            (return #f))
          ;check maybe-required-order
          (if maybe-required-order
              (body-in-order? l-child-element-names
                              maybe-required-order)
              #t)))])
   #:late-neg-projection
   (match-lambda
     [(tei-xexpr-contract name
                          child-tag/c
                          l-required-children
                          elements-only?
                          maybe-required-order
                          )
      (λ (blame) 
        (define list/xexpr-late-neg
          ((get/build-late-neg-projection
            (and/c list? xexpr/c))
           blame))
        (define name-late-neg
          ((get/build-late-neg-projection
            (coerce-flat-contract 'tei-xexpr-contract name))
           (blame-add-context blame "the name of")))
        (define body-blame
          (blame-add-context blame "the body of"))
        (define child-element-blame
          (blame-add-context blame "a child element of"))
        (define child-tag/c-late-neg
          ((get/build-late-neg-projection child-tag/c)
           (blame-add-context child-element-blame
                              "the name of")))
        
        (λ (val neg-party)
          ;check list + xexpr
          (list/xexpr-late-neg val neg-party)
          ;check name
          (name-late-neg (car val) neg-party)
          (define body
            (get-body val))
          (for ([child (in-list body)])
            (match child
              [(cons name _)
               ;; check child-tag/c and that child is valid
               (child-tag/c-late-neg name neg-party)
               (((get/build-late-neg-projection
                  (tei-xexpr/c name))
                 (blame-add-context blame
                                    (format "a child ~a element of" name)))
                child neg-party)]
              [(pregexp #"^\\s*$")
               (void)]
              [_
               ; check elements-only?
               (when elements-only?
                 (raise-blame-error
                  body-blame #:missing-party neg-party
                  val
                  '(expected:
                    "a child element or whitespace"
                    given: "~e")
                  child))]))
          (cond
            [(and (null? l-required-children)
                  (not maybe-required-order))
             val]
            [else
             (define l-child-element-names
               (for/list ([child (in-list body)]
                          #:when (list? child))
                 (car child)))
             ;check l-required-children
             (for ([required (in-list l-required-children)]
                   #:unless (memq required
                                  l-child-element-names))
               (raise-blame-error
                body-blame #:missing-party neg-party
                val
                '(expected:
                  "children including an element named ~a"
                  given: "~e")
                required
                body))
             ;check maybe-required-order
             (unless (or (not maybe-required-order)
                         (body-in-order? l-child-element-names
                                         maybe-required-order))
               (raise-blame-error
                body-blame #:missing-party neg-party
                val
                '(expected:
                  "the following elements (if present) to be in this order: ~a"
                  given:
                  "~e")
                (string-join (map symbol->string
                                  maybe-required-order))
                body))
             val])))])
   #:stronger (λ (this other)
                (or (equal? this other)
                    (contract-stronger? any-tei-xexpr/c other)))
   ))

(define (make-tag-contract
         name
         #:child-tag/c child-tag/c
         #:required-children [required-children null]
         #:elements-only? [elements-only? #f]
         #:required-order [required-order #f])
  (tei-xexpr-contract name
                      (coerce-flat-contract 'make-tag-contract
                                            child-tag/c)
                      required-children
                      elements-only?
                      required-order
                      ))

(define TEI.2-only-children
  (or/c 'teiHeader 'text))

(define teiHeader-only-children
  (or/c 'fileDesc 'encodingDesc
        'profileDesc 'revisionDesc
        'xenoData))

(define fileDesc-only-children
  (or/c 'titleStmt 'editionStmt 'extent 'publicationStmt
        'seriesStmt 'notesStmt 'sourceDesc))

(define restricted-children/c
  (or/c TEI.2-only-children
        teiHeader-only-children
        fileDesc-only-children
        ))

(define TEI.2/c
  (make-tag-contract 'TEI.2
                     #:child-tag/c TEI.2-only-children
                     #:required-children '(teiHeader text)
                     #:elements-only? #t
                     #:required-order '(teiHeader text)))

(define teiHeader/c
  (make-tag-contract 'teiHeader
                     #:child-tag/c teiHeader-only-children
                     #:required-children '(fileDesc)
                     #:elements-only? #t
                     #:required-order '(fileDesc
                                        encodingDesc
                                        profileDesc
                                        revisionDesc)))

(define fileDesc/c
  (make-tag-contract 'fileDesc
                     #:child-tag/c fileDesc-only-children
                     #:elements-only? #t
                     #:required-children
                     '(titleStmt publicationStmt sourceDesc)
                     #:required-order
                     '(titleStmt editionStmt extent publicationStmt
                                 seriesStmt notesStmt sourceDesc)))

(define titleStmt/c
  (make-tag-contract 'titleStmt
                     #:child-tag/c (or/c 'title 'author 'editor 'sponsor 'funder
                                         'principal 'respStmt)
                     #:elements-only? #t
                     #:required-children '(title)))

(define publicationStmt/c
  (make-tag-contract
   'publicationStmt
   #:elements-only? #t
   ;not enforcing requirement to have at least one of
   ;<p> | <publisher> | <distributor> | <authority>
   #:child-tag/c (or/c 'address 'date 'p 'pubPlace 'publisher
                       'authority 'availability 'distributor 'idno 'ab)))

(define availability/c
  (make-tag-contract
   'availability
   #:elements-only? #t
   #:child-tag/c (or/c 'p 'licence 'ab)))

(define sourceDesc/c
  (make-tag-contract
   'sourceDesc
   #:elements-only? #t
   ;not enforcing requirement to have exactly one of
   ;<p> | <bibl> | <biblStruct> | <biblFull> | <listBibl>
   #:child-tag/c (or/c 'bibl 'biblStruct 'list 'listBibl 'p
                       'table 'biblFull 'ab 'msDesc
                       'listEvent 'listNym 'listOrg 'listPerson 'listPlace
                       'recordingStmt 'scriptStmt
                       'listApp 'listWit)))

(define pb/c
  (make-tag-contract 'pb
                     #:child-tag/c none/c
                     #:elements-only? #t))

(define (contract-cache-ref hsh sym make/thunk)
  (let/ec return
    (define v
      (weak-box-value
       (hash-ref hsh sym (λ ()
                           (let ([rslt (make/thunk)])
                             (hash-set! hsh sym (make-weak-box rslt))
                             (return rslt))))))
    (or v
        (let ([rslt (make/thunk)])
          (hash-set! hsh sym (make-weak-box rslt))
          rslt))))
       
(define cache:other/elements-only
  (make-hasheq))

(define cache:else
  (make-hasheq))

(define (get-tag-contract/other/elements-only name)
  (contract-cache-ref cache:other/elements-only
                      name
                      (λ ()
                        (make-tag-contract
                         name
                         #:child-tag/c (not/c restricted-children/c)
                         #:elements-only? #t))))
                         
(define (get-tag-contract/else name)
  (contract-cache-ref cache:other/elements-only
                      name
                      (λ ()
                        (make-tag-contract
                         name
                         #:child-tag/c (not/c restricted-children/c)))))





