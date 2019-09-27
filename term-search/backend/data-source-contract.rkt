#lang racket/base

(require db/base
         adjutor
         racket/contract
         racket/match
         )

(provide postgresql-data-source/c
         )

(define/final-prop (postgresql-data-source/c)
  postgresql-data-source/c:impersonator)

(define* postgresql-data-source/c:impersonator
  #:with [(def
            [optional-hsh
             (hasheq
              '#:server string?	 
              '#:port exact-positive-integer?	 
              '#:socket (or/c path-string? 'guess #f)	 
              '#:password (or/c string? #f)	 
              '#:allow-cleartext-password? boolean?	 
              '#:ssl (or/c 'yes 'optional 'no)	 
              '#:notice-handler (or/c 'output 'error)	 
              '#:notification-handler (or/c 'output 'error))]
            [(hash-map-value proc hsh)
             (for/hasheq ([{k v} (in-immutable-hash hsh)])
               (values k (proc k v)))]
            [optional-hsh:predicate
             (hash-map-value (λ (k v)
                               (flat-contract-predicate v))
                             optional-hsh)]
            [optional-hsh:late-neg
             (hash-map-value (λ (k v)
                               (get/build-late-neg-projection
                                (coerce-flat-contract
                                 'postgresql-data-source/c
                                 v)))
                             optional-hsh)])]
  (make-contract
   #:name '(postgresql-data-source/c)
   #:first-order
   (match-lambda
     [(? checked-postgresql-dsn?) #t]
     [(data-source 'postgresql args _)
      (and (check-required-arg '#:user args)
           (check-required-arg '#:database args)
           (check-allowed-args args optional-hsh:predicate))]
     [_ #f])
   #:late-neg-projection
   (λ (blame)
     (define connector-blame
       (blame-add-context blame "the connector field of"))
     (define optional-hsh:+blame
       (hash-map-value
        (λ (k v)
          (v (blame-add-context
              connector-blame
              (format "the value for the ~e argument keyword in"
                      k))))
        optional-hsh:late-neg))
     (λ (val neg-party)
       (cond
         [(checked-postgresql-dsn? val)
          ;; Adjusts blame, but avoids copying
          (chaperone/copy-data-source blame val neg-party)]
         [else
          (unless (data-source? val)
            (raise-blame-error 
             blame #:missing-party neg-party
             val
             '(expected: "data-source?" given: "~e")
             val))
          (let ([val (chaperone/copy-data-source blame val neg-party)])
            (match-define (data-source connector args _)
              val)
            (unless (eq? 'postgresql connector)
              (raise-blame-error
               connector-blame
               #:missing-party neg-party
               val
               '(expected: "~e" given: "~e")
               'postgresql
               connector))
            (check-required-arg '#:user args connector-blame val neg-party)
            (check-required-arg '#:database args connector-blame val neg-party)
            (check-allowed-args args optional-hsh:+blame connector-blame val neg-party)
            val)])))))

(module+ test
  (define/contract a
    (postgresql-data-source/c)
    (postgresql-data-source #:user "me" #:database "it")))


(define (check-allowed-args args
                            hsh
                            [maybe-blame #f]
                            [val #f]
                            [neg-party #f])
  (let/ec return
    (define (return-false-thunk)
      (return #f))
    (let loop ([to-go args])
      (match to-go
        ['() #t]
        [(list* (or '#:user '#:database) _ to-go)
         (loop to-go)]
        [(list* (? keyword? kw) arg-val to-go)
         (define unknown-kw-thunk
           (if maybe-blame
               (λ ()
                 (raise-blame-error
                  maybe-blame #:missing-party neg-party
                  val
                  '("unknown keyword"
                    expected: "~a"
                    given: "~e"
                    "\n  data source: ~e")
                  (apply build-compound-type-name
                         'or/c '#:user '#:database
                         (hash-keys hsh))
                  kw
                  val))
               return-false-thunk))
         (define check
           (hash-ref hsh kw unknown-kw-thunk))
         (if maybe-blame
             (check arg-val neg-party)
             (unless (check arg-val)
               (return #f)))
         (loop to-go)]
        [_
         (if maybe-blame
             (raise-blame-error
              maybe-blame #:missing-party neg-party
              val
              '("malformed argument list"
                expected: "alternating keywords and argument values"
                given: "~e"
                "\n  data source: ~e")
              args val)
             (return #f))]))))



(define (check-required-arg kw
                            args
                            [maybe-blame #f]
                            [val #f]
                            [neg-party #f])
  (match (memq kw args)
    [#f
     (and maybe-blame
          (raise-blame-error
           maybe-blame #:missing-party neg-party
           val
           '("missing a required keyword"
             expected: "~e" given: "~e"
             "\n  data source: ~e")
           kw args val))]
    [(list* _ (? string?) _)
     #t]
    [(list* _ bad _)
     (and maybe-blame
          (raise-blame-error
           maybe-blame #:missing-party neg-party
           val
           '("bad argument for required keyword ~e"
             expected: "string?" given: "~e"
             "\n  data source: ~e")
           kw bad val))]
    [_
     (and maybe-blame
          (raise-blame-error
           maybe-blame #:missing-party neg-party
           val
           '("malformed argument list"
             expected: "alternating keywords and argument values"
             given: "~e"
             "\n  data source: ~e")
           args val))]))




(match-define-values {i-prop:checked-postgresql-dsn
                      checked-postgresql-dsn?
                      _}
  (make-impersonator-property 'i-prop:checked-postgresql-dsn))

(define (chaperone/copy-data-source blame val neg-party)
  (define ((chaperone-mutator name) this arg)
    (raise-blame-error
     blame #:missing-party neg-party
     this
     '("mutation is not allowed\n"
       "  mutator procedure: ~a\n"
       "  argument: ~e\n"
       "  data-source: ~e\n")
     name arg this))
  (chaperone-struct
   (if (checked-postgresql-dsn? val)
       val
       (struct-copy
        data-source val
        [args (for/list ([arg (in-list (data-source-args val))])
                (if (string? arg)
                    (string->immutable-string arg)
                    arg))]))
   struct:data-source
   set-data-source-connector!
   (chaperone-mutator 'set-data-source-connector!)
   set-data-source-args!
   (chaperone-mutator 'set-data-source-connector!)
   set-data-source-extensions!
   (chaperone-mutator 'set-data-source-extensions!)
   i-prop:checked-postgresql-dsn #t))


