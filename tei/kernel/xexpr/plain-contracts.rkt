#lang racket/base

(require racket/contract
         adjutor)

(provide raw-xexpr-element/c
         raw-xexpr-element?
         raw-xexpr-atom/c
         raw-xexpr/c
         raw-xexpr?
         normalized-xexpr-element/c
         normalized-xexpr-element?
         normalized-xexpr-atom/c
         normalized-xexpr/c
         normalized-xexpr?
         ;;;;
         comment?
         p-i?
         valid-char?
         cdata?
         entity-symbol/c
         ;;;;
         normalized-comment?
         normalized-p-i?
         normalized-comment/c
         normalized-p-i/c
         ;;;;
         (contract-out
          [check-raw-xexpr-element
           (-> blame? any/c any/c
               any/c)]
          [check-normalized-xexpr-element
           (-> blame? any/c any/c
               any/c)]
          ))

(module+ private
  (provide normalize-comment
           normalize-p-i
           ))

(TODO/void Fix & restore caches)

(module validate typed/racket/base
  (provide incorrect-raw-xexpr-element?
           raw-xexpr-element?
           incorrect-normalized-xexpr-element?
           normalized-xexpr-element?
           normalize-comment
           normalize-p-i
           (prefix-out typed:
                       (combine-out
                        normalized-comment?
                        normalized-p-i?))
           (structs-out xexpr-error
                        context
                        context-stack-root
                        context-stack-node
                        bad-element-name
                        attribute-problem
                        mutable-attribute-string
                        bad-attribute-name
                        bad-attribute-cdr
                        ))
  (require racket/match
           racket/provide-syntax
           (submod "entity-utils.rkt" typed)
           (for-syntax racket/base
                       syntax/parse
                       ))

  (require/typed
   xml
   [#:opaque Cdata cdata?]
   [#:opaque Valid-Char valid-char?]
   [#:struct source ([start : Any]
                     [stop : Any])]
   [#:struct (p-i source)
    ([target-name : Any]
     [instruction : String])
    #:type-name P-I]
   [#:struct comment ([text : String])
    #:type-name Comment]
   )

  (define-provide-syntax structs-out
    (syntax-parser
      [(_ s:id ...)
       #`(combine-out (struct-out s) ...)]))

  ;; This is heavily based on xml/private/xexpr-core,
  ;; but that isn't quite customizable enough to do what I want.

  (: incorrect-raw-xexpr-element? (-> Any (U False xexpr-error)))
  (define (incorrect-raw-xexpr-element? v)
    (incorrect-xexpr-element-list? v #f incorrect-raw-body-atom? #t))

  (: raw-xexpr-element? (-> Any Boolean))
  (define (raw-xexpr-element? v)
    (not (incorrect-xexpr-element-list? v #f incorrect-raw-body-atom? #f)))

  (: incorrect-normalized-xexpr-element? (-> Any (U False xexpr-error)))
  (define (incorrect-normalized-xexpr-element? v)
    (incorrect-xexpr-element-list? v #t incorrect-normalized-body-atom? #t))

  (: normalized-xexpr-element? (-> Any Boolean))
  (define (normalized-xexpr-element? v)
    (not (incorrect-xexpr-element-list? v #t incorrect-normalized-body-atom? #f)))

  (struct xexpr-error ([problem : (U 'bad-entity
                                     'invalid-char
                                     'bad-datum
                                     'unnormalized-datum
                                     'contains-mutable-string
                                     'non-list
                                     'missing-attr-list
                                     'missing-element-name
                                     bad-element-name
                                     mutable-attribute-string
                                     bad-attribute-name
                                     bad-attribute-cdr)]
                       [thing : Any]
                       [context : (U False context)])
    #:transparent)

  (struct context ([parent : (Listof Any)]
                   [child-index : Natural]
                   [stack : Context-Stack])
    #:transparent)

  (struct context-stack-root ([name : Symbol])
    #:transparent)

  (struct context-stack-node ([name : Symbol]
                              [index : Natural]
                              [stack : Context-Stack])
    #:transparent)

  (define-type Context-Stack (U context-stack-root context-stack-node))

  (struct bad-element-name ([value : Any])
    #:transparent)

  (struct attribute-problem ([pair : (Pairof Any Any)]
                             [index : Natural])
    #:transparent)

  (struct bad-attribute-name attribute-problem
    ([value : Any])
    #:transparent)

  (struct bad-attribute-cdr attribute-problem
    ([right-shape? : Boolean]
     [value : Any])
    #:transparent)

  (struct mutable-attribute-string attribute-problem ()
    #:transparent)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-type Incorrect-Body-Atom?
    (-> Any (U False context) (U False xexpr-error)))

  (: incorrect-raw-body-atom? Incorrect-Body-Atom?)
  (define (incorrect-raw-body-atom? atom maybe-context)
    (cond
      [(symbol? atom)
       (if (entity-symbol? atom)
           #f
           (xexpr-error 'bad-entity
                        atom
                        maybe-context))]
      [(number? atom) ;exact-nonnegative-integer?
       (if (valid-char? atom)
           #f
           (xexpr-error 'invalid-char
                        atom
                        maybe-context))]
      [(or (string? atom)
           (comment? atom)
           (p-i? atom)
           (cdata? atom))
       #f]
      [else
       (xexpr-error 'bad-datum
                    atom
                    maybe-context)]))

  (: incorrect-normalized-body-atom? Incorrect-Body-Atom?)
  (define (incorrect-normalized-body-atom? atom maybe-context)
    (define-syntax-rule (check/str pred atom)
      (if (pred atom) #f 'contains-mutable-string))
    (define problem
      (cond
        [(string? atom)
         (check/str immutable? atom)]
        [(comment? atom)
         (check/str normalized-comment? atom)]
        [(p-i? atom)
         (check/str normalized-p-i? atom)]
        [(or (symbol? atom)
             (valid-char? atom)
             (cdata? atom))
         'unnormalized-datum]
        [else
         'bad-datum]))
    (if problem
        (xexpr-error problem
                     atom
                     maybe-context)
        #f))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #|(: hsh:normalized-comments (Weak-HashTable Comment #t))
  (define hsh:normalized-comments
    (make-weak-hash))|#

  (: normalized-comment? (-> Comment Boolean))
  (define (normalized-comment? it)
    #|(define (known? it)
      (hash-has-key? hsh:normalized-comments it))|#
    (match it
      ;[(? known?) #t]
      [(comment (? immutable?))
       ;(hash-set! hsh:normalized-comments it #t)
       #t]
      [_ #f]))

  (: normalize-comment (-> Comment Comment))
  (define (normalize-comment it)
    (cond
      [(normalized-comment? it)
       it]
      [else
       (define norm
         (struct-copy
          comment it
          [text (string->immutable-string
                 (comment-text it))]))
       ;(hash-set! hsh:normalized-comments norm #t)
       norm]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #|(: hsh:normalized-p-i (Weak-HashTable P-I #t))
  (define hsh:normalized-p-i
    (make-weak-hash))|#

  (: normalized-p-i? (-> P-I Boolean))
  (define (normalized-p-i? it)
    (cond
      #|[(hash-has-key? hsh:normalized-p-i it)
       #t]|#
      [(immutable? (p-i-instruction it))
       ;(hash-set! hsh:normalized-p-i it #t)
       #t]
      [else
       #f]))

  (: normalize-p-i (-> P-I P-I))
  (define (normalize-p-i it)
    (if (normalized-p-i? it)
        it
        (let ([it (struct-copy
                   p-i it
                   [instruction
                    (string->immutable-string
                     (p-i-instruction it))])])
          ;(hash-set! hsh:normalized-p-i it #t)
          it)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #|(: hsh:normalized-element (Weak-HashTable (Listof Any) #t))
  (define hsh:normalized-element
    (make-weak-hash))|#

  
  (: incorrect-xexpr-element-list? (-> Any Boolean Incorrect-Body-Atom? Boolean
                                       (U False xexpr-error)))
  (define (incorrect-xexpr-element-list? maybe-lst
                                         must-be-normalized?
                                         incorrect-body-atom?
                                         accumulate-context?)
    (cond
      #|[(hash-has-key? hsh:normalized-element maybe-lst)
       #f]|#
      [(list? maybe-lst)
       (cond
         [(incorrect-xexpr-element-list?* maybe-lst
                                          must-be-normalized?
                                          incorrect-body-atom?
                                          accumulate-context?)]
         [else
          #;(when must-be-normalized?
              `(hash-set! hsh:normalized-element maybe-lst #t))
          #f])]
      [else
       (xexpr-error 'non-list maybe-lst #f)]))


  (: incorrect-xexpr-element-list?*
     (->* {(Listof Any) Boolean Incorrect-Body-Atom? Boolean}
          {(U False context)}
          (U False xexpr-error)))
  (define (incorrect-xexpr-element-list?* lst
                                          attr-list-required?
                                          incorrect-body-atom?
                                          accumulate-context?
                                          [maybe-context #f])
    (: incorrect-body? (-> Symbol (Listof Any) (U False xexpr-error)))
    (define (incorrect-body? elem-name body)
      (: make-new-context (-> Natural (U False context)))
      (define make-new-context
        (cond
          [accumulate-context?
           (define new-stack
             (match maybe-context
               [#f 
                (context-stack-root elem-name)]
               [(context _ child-index stack)
                (context-stack-node elem-name child-index stack)]))
           (λ (i)
             (context lst i new-stack))]
          [else
           (λ (i)
             #f)]))
      (for/or ([child (in-list body)]
               [i : Natural (in-naturals)])
        (cond
          [(list? child)
           (incorrect-xexpr-element-list?* child
                                           attr-list-required?
                                           incorrect-body-atom?
                                           accumulate-context?
                                           (make-new-context i))]
          [else
           (incorrect-body-atom? child (make-new-context i))])))
    (match lst
      ['()
       (xexpr-error 'missing-element-name
                    lst
                    maybe-context)]
      [(cons bad-elem-name _)
       #:when (not (symbol? bad-elem-name))
       (xexpr-error (bad-element-name bad-elem-name)
                    lst
                    maybe-context)]
      [(list-rest elem-name
                  (and unchecked-attr-list
                       (list (? pair?) ...))
                  body)
       (or (incorrect-attr-list? (assert unchecked-attr-list list?)
                                 #:normalized attr-list-required?
                                 lst
                                 maybe-context)
           (incorrect-body? (assert elem-name symbol?) body))]
      [(cons elem-name body)
       (cond
         [attr-list-required?
          (xexpr-error 'missing-attr-list
                       lst
                       maybe-context)]
         [else
          (incorrect-body? (assert elem-name symbol?) body)])]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  #|(: hsh:normalized-attribute-lists
     (Weak-HashTable (Listof Any) #t))
  (define hsh:normalized-attribute-lists
    (make-weak-hash))|#

  (: incorrect-attr-list? (-> (Listof Any)
                              ;; Really (Listof (Pairof Any Any)),
                              ;; but that's hard to prove to TR
                              (Listof Any) 
                              (U False context)
                              #:normalized Boolean
                              (U False xexpr-error)))
  (define (incorrect-attr-list? lst
                                #:normalized must-be-normalized?
                                elem
                                maybe-context)
    (cond
      #|[(hash-has-key? hsh:normalized-attribute-lists lst)
       #f]|#
      [(for/or : (U False xexpr-error)
         ([attr (in-list lst)]
          [i : Natural (in-naturals)])
         (define-syntax-rule (make-error problem)
           (xexpr-error problem
                        elem
                        maybe-context))
         (match attr
           [(list (? symbol?) (? string? str))
            (if (and must-be-normalized?
                     (not (immutable? str)))
                (make-error
                 (mutable-attribute-string attr i))
                #f)]
           [(cons bad-name _)
            #:when (not (symbol? bad-name))
            (make-error (bad-attribute-name attr
                                            i
                                            bad-name))]
           [(list name non-string)
            (make-error (bad-attribute-cdr attr
                                           i
                                           #t
                                           non-string))]
           [(cons name misshapen)
            (make-error (bad-attribute-cdr attr
                                           i
                                           #f
                                           misshapen))]))]
      [else
       #;(when must-be-normalized?
           (hash-set! hsh:normalized-attribute-lists lst #t))
       #f]))
  #|END module validate|#)

;                                                          
;                                                          
;                    ;;                                 ;; 
;                    ;;                                 ;; 
;   ;;  ;;  ;; ;   ;;;;;;; ;     ;  ; ;;      ;;;    ;;;;; 
;   ;;  ;;  ;;; ;    ;;     ;   ;   ;;  ;   ;;   ;  ;   ;; 
;   ;;  ;;  ;;  ;;   ;;     ;   ;   ;;  ;   ;    ;  ;   ;; 
;   ;;  ;;  ;;  ;;   ;;      ;  ;   ;;  ;; ;;;;;;;;;;   ;; 
;   ;;  ;;  ;;  ;;   ;;      ; ;    ;;  ;   ;       ;   ;; 
;    ; ;;;  ;;  ;;    ;      ; ;    ;;  ;   ;;   ;  ;   ;; 
;     ; ;;  ;;  ;;     ;;;    ;     ;;;;      ;;;    ;;; ; 
;                             ;     ;;                     
;                            ;      ;;                     
;                          ;;       ;;                     
;                                                          

(require 'validate
         racket/match
         racket/pretty
         racket/string
         xml
         "entity-utils.rkt"
         "../pre-kernel-lib.rkt"
         )


(define/final-prop raw-xexpr-element/c
  (make-flat-contract
   #:name 'raw-xexpr-element/c
   #:list-contract? #t
   #:first-order raw-xexpr-element?
   #:stronger (λ (this other)
                (or (equal? this other)
                    (contract-stronger? xexpr/c other)
                    (contract-stronger? listof-any/c other)
                    (contract-stronger? xexpr? other)
                    (contract-stronger? list? other)))
   #:late-neg-projection
   (λ (blame)
     (λ (val neg-party)
       (check-raw-xexpr-element blame val neg-party)))))

(define/final-prop normalized-xexpr-element/c
  (make-flat-contract
   #:name 'normalized-xexpr-element/c
   #:list-contract? #t
   #:first-order normalized-xexpr-element?
   #:stronger (λ (this other)
                (or (equal? this other)
                    (contract-stronger? raw-xexpr-element/c other)))
   #:late-neg-projection
   (λ (blame)
     (λ (val neg-party)
       (check-normalized-xexpr-element blame val neg-party)))))

(define contains-mutable-strings-blame-fmt-lst
  '("x-expression has not been normalized;\n"
    " value contains a mutable string"
    given: "~e"))

(define (make-immutable-string-struct-contract
         #:name name
         #:first-order first-order?
         #:typed:normalized? typed:normalized? 
         #:raw-predicate unnorm?)
  (def
    [pred-as-contract
     (coerce-flat-contract name first-order?)]
    [unnorm/c (coerce-flat-contract name unnorm?)]
    [unnorm-late-neg
     (get/build-late-neg-projection unnorm/c)])
  (make-flat-contract
   #:name name
   #:first-order first-order?
   ;#:stronger 
   ;#:equivalent
   #:late-neg-projection
   (λ (blame)
     (define check-unnorm
       (unnorm-late-neg blame))
     (λ (val neg-party)
       (check-unnorm val neg-party)
       (unless (typed:normalized? val)
         (raise-blame-error
          blame #:missing-party neg-party
          val
          contains-mutable-strings-blame-fmt-lst
          val))
       val))))

(define/final-prop (normalized-comment? v)
  (and (comment? v)
       (typed:normalized-comment? v)))

(define/final-prop normalized-comment/c
  (make-immutable-string-struct-contract
   #:name 'normalized-comment/c
   #:first-order normalized-comment?
   #:typed:normalized? typed:normalized-comment?
   #:raw-predicate comment?))

(define/final-prop (normalized-p-i? v)
  (and (p-i? v)
       (typed:normalized-p-i? v)))

(define/final-prop normalized-p-i/c
  (make-immutable-string-struct-contract
   #:name 'normalized-p-i/c
   #:first-order normalized-p-i?
   #:typed:normalized? typed:normalized-p-i?
   #:raw-predicate p-i?))
     
(define/final-prop (normalized-xexpr? v)
  (or (and (string? v)
           (immutable? v))
      (normalized-comment? v)
      (normalized-p-i? v)
      (normalized-xexpr-element? v)))

(define/final-prop normalized-xexpr-atom/c
  (or/c string-immutable/c
        normalized-comment/c
        normalized-p-i/c))

(define/final-prop normalized-xexpr/c
  (if/c list?
        normalized-xexpr-element/c
        normalized-xexpr-atom/c))

(define/final-prop (raw-xexpr? v)
  (or (string? v)
      (comment? v)
      (p-i? v)
      (entity-symbol? v)
      (valid-char? v)
      (cdata? v)
      (raw-xexpr-element? v)))

(define/final-prop raw-xexpr-atom/c
  (or/c string?
        comment?
        p-i?
        entity-symbol/c
        valid-char?
        cdata?))

(define/final-prop raw-xexpr/c
  (if/c list?
        raw-xexpr-element/c
        raw-xexpr-atom/c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define listof-any/c
  (listof any/c))


(define ((make-xexpr-blame-checker incorrect? formatted-child-contract) blame
                                                                        val
                                                                        neg-party)
  (define error?
    (incorrect? val))
  (if error?
      (raise-xexpr-blame-error blame val neg-party error? formatted-child-contract)
      val))

(define check-raw-xexpr-element
  (make-xexpr-blame-checker
   incorrect-raw-xexpr-element?
   "a string, numeric or symbolic entity, cdata, comment, p-i, or list"))

(define check-normalized-xexpr-element
  (make-xexpr-blame-checker incorrect-normalized-xexpr-element?
                            (contract-name normalized-xexpr/c)))

(define (raise-xexpr-blame-error blame val neg-party e formatted-child-contract)
  (match-define (xexpr-error problem thing the-context)
    e)
  (define (raise-with-context l-format #:convert-blame [post-convert-blame values]
                              . format-args)
    (define (do-raise l-format format-args #:blame [blame blame])
      (apply
       raise-blame-error
       (post-convert-blame blame) #:missing-party neg-party
       val
       l-format
       format-args))
    (match the-context
      [#f
       (do-raise (append l-format
                         '("\n  context...:~a"))
                 (append format-args
                         (list (pretty-error-format val))))]
      [(context parent child-index (context-stack-root _))
       (do-raise (append l-format
                         '("\n  parent element...:~a"
                           "\n  child position: ~a"))
                 (append format-args
                         (list (pretty-error-format parent)
                               (index->ordinal-string child-index))))]
      [(context parent child-index stack)
       (do-raise (append l-format
                         '("\n  parent element...:~a"
                           "\n  child position: ~a"
                           "\n  root...:~a"))
                 (append format-args
                         (list (pretty-error-format parent)
                               (index->ordinal-string child-index)
                               (pretty-error-format val)))
                 #:blame (stack-convert-blame stack blame))]))
  (match problem
    ['non-list
     (raise-blame-error
      blame #:missing-party neg-party
      val
      '(expected: "list?" given: "~e")
      val)]
    ['bad-entity
     (raise-with-context
      '("unknown symbolic entity in x-expression"
        given: "~e"
        "\n  known symbolic entities...:\n~a")
      thing
      (string-join (map (λ (sym)
                          (format "   ~v" sym))
                        entity-symbols-list)
                   "\n"))]
    ['contains-mutable-string
     (raise-with-context
      contains-mutable-strings-blame-fmt-lst
      thing)]
    ['invalid-char
     (raise-with-context
      '("invalid numeric entity in x-expression"
        expected: "~a"
        given: "~e")
      (contract-name valid-char?)
      thing)]
    ['unnormalized-datum
     (raise-with-context
      '("x-expression has not been normalized"
        expected: "~a"
        given: "~e")
      (contract-name (or/c string? comment? p-i?))
      thing)]
    ['missing-attr-list
     (raise-with-context
      '("x-expression has not been normalized"
        expected: "an element with an explicit attribute list"
        given: "~e")
      thing)]
    ['missing-element-name
     (raise-with-context
      '("not an x-expression"
        expected: "a list beginning with a symbol for the element name"
        given: "~e")
      thing)]
    [(bad-element-name non-symbol)
     (raise-with-context
      '("not an x-expression"
        expected: "a symbol as the element name"
        given: "~e"
        "\n  element...:~a")
      non-symbol
      (pretty-error-format thing))]
    [(mutable-attribute-string pair index)
     (raise-with-context
      '("x-expression has not been normalized"
        expected: "an immutable attribute value string"
        given: "~e"
        "\n  attribute pair: ~e"
        "\n  attribute position: ~a"
        "\n  element...:~a")
      (cdr pair)
      pair
      (index->ordinal-string index)
      (pretty-error-format thing))]
    [(bad-attribute-name pair index non-symbol)
     (raise-with-context
      '("not an x-expression"
        expected: "an attribute name symbol"
        given: "~e"
        "\n  attribute pair: ~e"
        "\n  attribute position: ~a"
        "\n  element...:~a")
      non-symbol
      pair
      (index->ordinal-string index)
      (pretty-error-format thing))]
    [(bad-attribute-cdr pair index #t non-string)
     (raise-with-context
      '("not an x-expression"
        expected: "an attribute value string"
        given: "~e"
        "\n  attribute pair: ~e"
        "\n  attribute position: ~a"
        "\n  element...:~a")
      non-string
      pair
      (index->ordinal-string index)
      (pretty-error-format thing))]
    [(bad-attribute-cdr pair index #f _)
     (raise-with-context
      '("not an x-expression; malformed attribute pair"
        expected: "~a"
        given: "~e"
        "\n  attribute position: ~a"
        "\n  element...:~a")
      (contract-name (list/c symbol? string?))
      pair
      (index->ordinal-string index)
      (pretty-error-format thing))]
    ['bad-datum
     (raise-with-context
      '("not an x-expression"
        expected: "~a"
        given: "~e")
      formatted-child-contract
      thing)]))


(define (stack-convert-blame stack blame)
  (for/fold ([blame blame])
            ([str (in-list
                   (let loop ([stack stack]
                              [so-far null])
                     (match stack
                       [(context-stack-root _)
                        so-far]
                       [(context-stack-node name index stack)
                        (loop stack
                              (cons (format "the ~a child (a ~v element) of"
                                            (index->ordinal-string index)
                                            name)
                                    so-far))])))])
    (blame-add-context blame str)))
  

(define (index->ordinal-string index)
  (define str
    (number->string (add1 index)))
  (string-append str
                 (if (regexp-match #rx"1.$" str)
                     "th"
                     (case (string-ref str (sub1 (string-length str)))
                       [(#\1) "st"]
                       [(#\2) "nd"]
                       [(#\3) "rd"]
                       [else "th"]))))
                                       
(define (pretty-error-format v)
  (parameterize ([pretty-print-print-line
                  (λ (line-num out old-line-len col-count)
                    (write-string "\n   " out)
                    3)])
    (pretty-format v (error-print-width))))


#;
(define/contract example
  raw-xexpr-element/c
  #("illegal"))
#;
(define/contract example
  raw-xexpr-element/c
  '(p "foo" 'unknown "bar"))
#;
(define/contract example
  normalized-xexpr-element/c 
  '(p ([id "foo"])
      "Something" amp
      (span "x" (a #("illegal")) "z")
      "foo."))
#;
(define/contract example
  raw-xexpr-element/c
  '(p ([id "foo"])
      "Something" amp
      (span "x" (a #("illegal")) "z")
      "foo."))
#;
(define/contract example
  raw-xexpr-element/c
  '(p ([id "foo"])
      "Something" amp
      (span "x" (a (["illegal" "bar"]) "z")
            "foo.")))

                   


