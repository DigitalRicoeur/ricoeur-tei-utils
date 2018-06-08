#lang racket/base

(require racket/contract
         racket/match
         adjutor
         "base-structs.rkt"
         )

(module+ test
  (require rackunit
           (submod "..")))

(provide tei-element-can-have-resp?
         resp-string/c
         has-tei-document-paragraphs-status?
         guess-paragraphs-status/c
         has-tei-document-paragraphs-status/c
         (contract-out
          [tei-element-resp
           (->* {tei-element-can-have-resp?}
                {(or/c 'ricoeur #f)}
                (or/c symbol? #f))]
          [tei-element-resp/string
           (->* {tei-element-can-have-resp?}
                {(or/c 'ricoeur #f)}
                (or/c resp-string/c #f))]
          [resp-string->symbol
           (-> resp-string/c symbol?)]
          [tei-document-paragraphs-status
           (-> has-tei-document-paragraphs-status?
               guess-paragraphs-status/c)]
          ))

(module+ for-lang
  (provide (contract-out
            [prop:resp
             (struct-type-property/c
              (cons/c (-> any/c (or/c symbol? #f))
                      (-> any/c (or/c resp-string/c #f))))]
            [prop:guess-paragraphs-status
             (struct-type-property/c
              (-> any/c guess-paragraphs-status/c))]
            )))

(TODO/void resp-string/c and tei-element-resp/string:
           better names
           #: These clash with tei-get-resp-string.)

(define/final-prop resp-string/c
  #rx"^#.+$")

(define (resp-string->symbol str)
  (string->symbol (substring str 1)))

(module+ test
  (check-eq? (resp-string->symbol "#ricoeur")
             'ricoeur))

(match-define-values {prop:resp/symbol
                      _
                      get-get-resp}
  (make-struct-type-property 'prop:resp/symbol))

(match-define-values {prop:resp/string
                      _
                      get-get-resp/string}
  (make-struct-type-property 'prop:resp/string))
                      
(match-define-values {prop:resp
                      tei-element-can-have-resp?
                      _}
  (make-struct-type-property 'prop:resp
                             #f
                             (list (cons prop:resp/symbol car)
                                   (cons prop:resp/string cdr))))

(define (tei-element-resp e [default 'ricoeur])
  (or ((get-get-resp e) e)
      default))

(define (tei-element-resp/string e [default 'ricoeur])
  (or ((get-get-resp/string e) e)
      (and default
           "#ricoeur")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values {prop:guess-paragraphs-status
                has-tei-document-paragraphs-status?
                get-get-status}
  (make-struct-type-property 'prop:guess-paragraphs-status))


(define (tei-document-paragraphs-status d)
  ((get-get-status d) d))


(define/final-prop guess-paragraphs-status/c
  (or/c 'todo
        'line-breaks
        'blank-lines
        'done
        'skip))

(define/subexpression-pos-prop (has-tei-document-paragraphs-status/c inner)
  (let ([inner (coerce-flat-contract 'tei-document-paragraphs-status/c inner)])
    (paragraphs-status-contract inner
                                (get/build-late-neg-projection inner)
                                (flat-contract-predicate inner))))


(struct paragraphs-status-contract (inner inner-late-neg inner-predicate)
  #:transparent
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (this)
     (build-compound-type-name 'tei-document-with-paragraphs-status/c
                               (paragraphs-status-contract-inner this)))
   #:stronger
   (λ (this it)
     (or (contract-stronger? has-tei-document-paragraphs-status? it)
         (match* {this it}
           [{(paragraphs-status-contract this _ _)
             (paragraphs-status-contract it _ _)}
            (contract-stronger? this it)]
           [{_ _}
            #f])))
   #:first-order
   (λ (this)
     (define pred
       (paragraphs-status-contract-inner-predicate this))
     (λ (val)
       (check-paragraphs-status-contract val #f #f pred)))
   #:late-neg-projection
   (λ (this)
     (define inner-late-neg
       (paragraphs-status-contract-inner-late-neg this))
     (λ (blame)
       (define inner+blame
         (inner-late-neg (blame-add-context blame
                                            "the tei-document-paragraphs-status of")))
       (λ (val neg-party)
         (check-paragraphs-status-contract val blame neg-party inner+blame))))))


(define (check-paragraphs-status-contract val maybe-blame neg-party inner-check)
  (cond
    [(not (has-tei-document-paragraphs-status? val))
     (and maybe-blame
          (raise-blame-error
           maybe-blame val #:missing-party neg-party
           '(expected: "has-tei-document-paragraphs-status?" given: "~e")
           val))]
    [maybe-blame
     (inner-check (tei-document-paragraphs-status val) neg-party)
     val]
    [else
     (inner-check (tei-document-paragraphs-status val))]))







