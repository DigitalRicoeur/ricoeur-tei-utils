#lang at-exp racket

(require xml
         "xml-entity-utils.rkt"
         "tei-xexpr-contracts.rkt"
         roman-numeral
         data/maybe
         (rename-in data/functor
                    [map fmap])
         )

(provide tag->element
         element<%>
         TEI.2<%>
         teiHeader<%>
         TEI-info<%>
         pb<%> 
         element-or-xexpr/c
         (contract-out
          [read-TEI
           (->* {} {input-port?} (is-a?/c TEI.2<%>))]
          ))

(module+ private:term-search
  (provide smoosh))

(define/contract (cdata->plain-text it)
  (-> cdata? string?)
  (match (cdata-string it)
    [(regexp #rx"<!\\[CDATA\\[(.*)\\]\\]>" (list _ content))
     content]
    [content content]))

(define/contract (valid-char->plain-text num)
  (-> valid-char? string?)
  (string (integer->char num)))

(define-member-name smoosh (generate-member-key))

(define-values {element% element-or-xexpr/c}
  (let ([element<%> (interface ()
                      [to-xexpr (->m xexpr/c)]
                      [to-plain-text (->m string?)]
                      )])
    (define element-or-xexpr/c
      (or/c (is-a?/c element<%>)
            string?
            symbol?
            valid-char?
            cdata?
            comment?
            p-i?))
    (define/contract element%
      (class/c (init-field [name symbol?]
                           [attributes (listof (list/c symbol? string?))]
                           [body (listof element-or-xexpr/c)]))
      (class* object% ((interface (element<%>)
                         [get-name (->m symbol?)]
                         [get-attributes (->m (listof (list/c symbol? string?)))]
                         [get-body (->m (listof element-or-xexpr/c))]))
        (inspect #f)
        (super-new)
        (init-field name
                    [attributes null]
                    [body null]
                    )
        (define/public (to-xexpr)
          (list* name attributes (for/list ([child (in-list body)])
                                   (if (xexpr? child)
                                       child
                                       (send child to-xexpr)))))
        (define/public (to-plain-text)
          (string-join (map element-or-xexpr->plain-text
                            body)
                       ""))
        (define/public (smoosh)
          (flatten
           (for/list ([child (in-list body)])
             (if (string? child)
                 child
                 (send child smoosh)))))
        (public*
         [get-name (λ () name)]
         [get-attributes (λ () attributes)]
         [get-body (λ () body)])
        #|END element%|#))
    (values element% element-or-xexpr/c)))


(define/contract (element-or-xexpr->plain-text child)
  (-> element-or-xexpr/c string?)
  (cond [(string? child)
         child]
        [((is-a?/c element%) child)
         (send child to-plain-text)]
        [(or (comment? child)
             (p-i? child))
         ""]
        [(cdata? child)
         (cdata->plain-text child)]
        [(valid-char? child)
         (valid-char->plain-text child)]
        [(symbol? child) 
         (string (entity-symbol->char child))]))


(define element<%>
  (class->interface element%))

(define (tag->element tag)
  (-> any-tei-xexpr/c (is-a?/c element<%>))
  (define-values {name attributes raw-body}
    (match tag
      [(list-rest name
                  (? (listof (list/c symbol? string?)) attributes)
                  raw-body)
       (values name attributes raw-body)]
      [(cons name raw-body)
       (values name null raw-body)]))
  (new (case name
         [(TEI.2) TEI.2%]
         [(text) text%]
         [(body) body%]
         [(front) front%]
         [(back) back%]
         [(pb) pb%]
         [(p) p%]
         [(ab) ab%]
         [(head) head%]
         [(div) div%]
         [(teiHeader) teiHeader%] 
         [(fileDesc) fileDesc%]
         [(titleStmt) titleStmt%]
         [(publicationStmt) publicationStmt%]
         [(sourceDesc) sourceDesc%]
         [(title) title%]
         [(author) author%]
         [(editor) editor%]
         [(authority) authority%]
         [(availability) availability%]
         [(bibl) bibl%]
         [else element%])
       [name name]
       [attributes attributes]
       [body (for/list ([child (in-list raw-body)])
               (if (list? child)
                   (tag->element child)
                   child))]))

(define (discard-bom p)
   (void (regexp-try-match #rx"^\uFEFF" p)))

(define (read-TEI [in (current-input-port)])
  ;TODO: enforce TEI.2<%>
  (discard-bom in)
  (tag->element
   (xml->xexpr
    (document-element
     (read-xml in)))))

(define-values {elements-only-mixin elements-only-element%/c}
  (let ([elements-only<%> (interface () )])
    (values (mixin {element<%>} {elements-only<%>}
              (inspect #f)
              (init [body null])
              (super-new [body (filter (is-a?/c element%) body)]))
            (is-a?/c elements-only<%>))))

(define guess-paragraphs<%>
  (interface (element<%>)
    guess-paragraphs))

(define guess-paragraphs-mixin
  (mixin {element<%>} {guess-paragraphs<%>}
    (inspect #f)
    (super-new)
    (inherit-field name attributes body)
    (define/public (guess-paragraphs)
      (new this%
           [name name]
           [attributes attributes]
           [body (flatten
                  (for/list ([child (in-list body)])
                    (if ((is-a?/c guess-paragraphs<%>) child)
                        (send child guess-paragraphs)
                        child)))]))))

(define get-title<%>
  (interface (element<%>)
    [get-title (->m string?)]))

(define TEI-info<%>
  (interface (get-title<%>)))

(define TEI.2%
  (class* (guess-paragraphs-mixin
           (elements-only-mixin element%))
    ((interface (guess-paragraphs<%>)
       [guess-paragraphs (->m (recursive-contract
                               (is-a?/c TEI.2%)))]
       #;[smoosh (->m (listof (or/c string?
                                  (recursive-contract
                                   (is-a?/c pb<%>)))))]
       [write-TEI (->*m {} {output-port?} any)])
     TEI-info<%>)
    (inspect #f)
    (super-new)
    (inherit-field body)
    (inherit to-xexpr)
    (match-define (list teiHeader text)
      body)
    (define/override (to-plain-text)
      (send text to-plain-text))
    (define/override (smoosh)
      (send text smoosh))
    (define/public (get-teiHeader)
      teiHeader)
    (define/public (get-title)
      (send teiHeader get-title))
    (define/public (write-TEI [out (current-output-port)])
      (displayln @string-append{
 <?xml version="1.0" encoding="utf-8"?>
 <!DOCTYPE TEI.2 PUBLIC "-//TEI P4//DTD Main Document Type//EN" "tei2.dtd" [
 @"  "<!ENTITY % TEI.XML   "INCLUDE" >
 @"  "<!ENTITY % TEI.prose "INCLUDE" >
 @"  "<!ENTITY % TEI.linking "INCLUDE" >
 ]>}
                 out)
      (write-xexpr (to-xexpr)))))

(define TEI.2<%>
  (class->interface TEI.2%))

(define text%
  (class (guess-paragraphs-mixin
          (elements-only-mixin element%))
    (inspect #f)
    (super-new)))

(define body%
  (class (guess-paragraphs-mixin
          (elements-only-mixin element%))
    (inspect #f)
    (super-new)))

(define front%
  (class (guess-paragraphs-mixin
          (elements-only-mixin element%))
    (inspect #f)
    (super-new)))

(define back%
  (class (guess-paragraphs-mixin
          (elements-only-mixin element%))
    (inspect #f)
    (super-new)))

(define pb%
  (class (elements-only-mixin element%)
    (inspect #f)
    (super-new)
    (inherit-field attributes)
    (define n
      (fmap car (false->maybe (dict-ref attributes 'n #f))))
    #|(define interp
      (match (from-just #f n)
        [#f #f]
        [(app string->number (? number? it))
         (cons 'number it)]
        [(app (curry exn->maybe exn:fail? roman->number)
              (just it))
         (cons 'roman it)]
        [(? string? it)
         (cons #f it)]))|#
    (define/public (get-page-string)
      n)
    #|(define/public (interpret-number)
      interp)|#
    (define/override (to-plain-text)
      "\f")
    (define/override (smoosh)
      this)))

(define pb<%>
  (class->interface pb%))

(define p%
  (class element%
    (inspect #f)
    (super-new)
    (inherit-field body)
    (define/override (to-plain-text)
      (cond
        [(null? body)
         ""]
        [else
         (string-join
          (for/list ([child
                      (in-list
                       (let ([body (let ([body
                                          (for/list ([child (in-list body)])
                                            (if ((is-a?/c element%) child)
                                                child
                                                (element-or-xexpr->plain-text child)))])
                                     (if (string? (first body))
                                         (cons (string-trim (first body)
                                                            #:right? #f)
                                               (rest body))
                                         body))])
                         (if (string? (last body))
                             (append (drop-right body 1)
                                     (list (string-trim (last body)
                                                        #:left? #f)))
                             body)))])
            (if (string? child)
                (string-normalize-spaces child #:trim? #f)
                (send child to-plain-text)))
          ""
          #:before-first "\n"
          #:after-last "\n")]))))


(define ab%
  (let ()
    (struct parbreak ())
    (class* element% {guess-paragraphs<%>}
      (inspect #f)
      (super-new)
      (inherit-field body)
      (define/private (insert-parbreaks)
        (flatten
         (for/list ([child (in-list body)])
           (cond
             [(string? child)
              (add-between (regexp-split #px"\n\\s*\n" child)
                           (parbreak))]
             [else
              child]))))
      (define/private (group-by-parbreaks)
        (let loop ([this-group null]
                   [to-go (insert-parbreaks)])
          (match to-go
            ['() (list this-group)]
            [(cons (? parbreak?) more)
             (cons this-group
                   (loop null more))]
            [(cons (pregexp #px"^\\s*$") more)
             (loop this-group more)]
            [(cons this-item more)
             (loop (append this-group
                           (list this-item))
                   more)])))
      (define/public (guess-paragraphs)
        (for/list ([pargroup (in-list (group-by-parbreaks))]
                   #:unless (null? pargroup))
          (match pargroup
            [(list (? (is-a?/c element%) elem))
             elem]
            [_
             (new p%
                  [name 'p]
                  [body pargroup])]))))))

(define head%
  (class element%
    (inspect #f)
    (super-new)))

(define div%
  (class (guess-paragraphs-mixin
          (elements-only-mixin element%))
    (inspect #f)
    (super-new)))

(define teiHeader%
  (class* (elements-only-mixin element%) (TEI-info<%>)
    (inspect #f)
    (super-new)
    (inherit-field body)
    (define/public (get-title)
      (send (findf (is-a?/c fileDesc%) body) get-title))))

(define teiHeader<%>
  (class->interface teiHeader%))

(define fileDesc%
  (class* (elements-only-mixin element%) (get-title<%>)
    (inspect #f)
    (super-new)
    (inherit-field body)
    (define/public (get-title)
      (send (findf (is-a?/c titleStmt%) body) get-title))))
              

(define titleStmt%
  (class* (elements-only-mixin element%) (get-title<%>)
    (inspect #f)
    (super-new)
    (inherit-field body)
    (define promise:title
      (delay
        (string-join (for/list ([t (in-list body)]
                                #:when ((is-a?/c title%) t))
                       (string-normalize-spaces
                        (string-trim (send t to-plain-text))))
                     ": ")))
    (define/public (get-title)
      (force promise:title))))

(define publicationStmt%
  (class (elements-only-mixin element%)
    (inspect #f)
    (super-new)))

(define sourceDesc%
  (class (elements-only-mixin element%)
    (inspect #f)
    (super-new)))

(define title%
  (class element%
    (inspect #f)
    (super-new)))

(define author%
  (class element%
    (inspect #f)
    (super-new)))

(define editor%
  (class element%
    (inspect #f)
    (super-new)))

(define authority%
  (class element%
    (inspect #f)
    (super-new)))

(define availability%
  (class element%
    (inspect #f)
    (super-new)))

(define bibl%
  (class element%
    (inspect #f)
    (super-new)))

#|
(with-output-to-file
    "/Users/philip/code/ricoeur/texts/TEI/oneself_as_another-para.xml"
  #:exists 'replace
  (λ () 
    (send (send
            (call-with-input-file
                "/Users/philip/code/ricoeur/texts/TEI/oneself_as_another.xml"
              read-TEI)
            guess-paragraphs)
           write-TEI)))


(displayln (send
            (call-with-input-file
                "/Users/philip/code/ricoeur/texts/TEI/oneself_as_another.xml"
              read-TEI)
            to-plain-text))
|#