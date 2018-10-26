#lang ricoeur/tei/kernel

ƒtitle{Content-Containing Elements}

ƒbegin-for-runtime[
 (provide tei-note?
          (contract-out
           [tei-note-get-place
            (-> tei-note? (or/c 'foot 'end))]
           [tei-note-get-n
            (-> tei-note? string-immutable/c)]
           [tei-note-get-transl?
            (-> tei-note? (or/c #f 'transl))]
           ))
 (module+ private-to-base
   (provide tei-ab?))
 (require (submod ricoeur/tei/kernel private)
          roman-numeral)
 ]

ƒ(require-self-for-label)
ƒ(require (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date)))

ƒ(define-element ab
   #:contains-text
   #:children ([0+ list]
               [0+ note]
               [0+ pb])
   #:predicate tei-ab?
   #:prose ƒ[]{
 The ƒtag{ab} ("anonymous block")
 element may contain a combination of
 free-form text and ƒtag{list}, ƒtag{note},
 and ƒtag{pb} elements.
 It is used for blocks of text
 that have not yet been divided into more semantically
 meaningful elements.
 })

ƒ(define-element p
   #:contains-text
   #:children ([0+ list]
               [0+ note]
               [0+ pb])
   #:prose ƒ[]{
 The ƒtag{p} ("paragraph")
 element may contain a combination of
 free-form text and ƒtag{list}, ƒtag{note},
 and ƒtag{pb} elements.
}
   #:property prop:element->plain-text
   (λ (this)
     (define body
       (tei-element-get-body this))
     (cond
       [(null? body)
        ""]
       [else
        (let* ([body (for/list ([child (in-list body)])
                       (if (tei-element? child)
                           child
                           (element-or-xexpr->plain-text child)))]
               [body (if (string? (first body))
                         (cons (string-trim (first body)
                                            #:right? #f)
                               (rest body))
                         body)]
               [body (if (string? (last body))
                         (append (drop-right body 1)
                                 (list (string-trim (last body)
                                                    #:left? #f)))
                         body)]
               [body (for/list ([child (in-list body)])
                       (if (string? child)
                           (string-normalize-spaces child #:trim? #f)
                           (element-or-xexpr->plain-text child)))])
          (string-join body
                       ""
                       #:before-first "\n"
                       #:after-last "\n"))])))

ƒ(define-element head
   #:contains-text
   #:children ([0+ note])
   #:prose ƒ[]{
 The ƒtag{head} ("heading")
 element may contain a combination of
 free-form text and ƒtag{note} elements.
 })

ƒ(define-element note
   #:contains-text
   #:children ([0+ list]
               [0+ note]
               [0+ p]
               [0+ ab])
   #:attr-contracts ([place (or/c "foot" "end")]
                     [resp #rx"^#.+$"]
                     [type "transl"]
                     [n string?])
   #:required-attrs (place n)
   #:predicate tei-note?
   #:constructor
   [#:attributes attrs
    (declare-resp-field attrs)
    (define-fields
      ([n #:accessor tei-note-get-n]
       (string->immutable-string (attributes-ref attrs 'n)))
      ([place #:accessor tei-note-get-place]
       (string->symbol (attributes-ref attrs 'place)))
      ([transl? #:accessor tei-note-get-transl?]
       (and (attributes-ref attrs 'transl)
            'transl)))]
   #:prose ƒ{
     The ƒtag{note} element
     may contain a combination of free-form text
     and ƒtag{list}, ƒtag{note}, ƒtag{p}, and
     ƒtag{ab} elements.
     It must have a ƒtt{place} attribute with
     a value of either ƒracket["foot"] or
     ƒracket["end"].
     It must also have an ƒattr{n} attribute giving the
     the number or symbol for the footnote or endnote.
     Translation notes should have a ƒattr{type} attribute
     with a value of ƒracket["transl"].
     Any ƒtag{note} element may, and a ƒtag{note} representing a
     note not by Paul Ricœur must, have a ƒattr{resp} attribute
     that is valid per the TEI standard, as discussed above under
     ƒsecref["Sections_Not_by_Ric_ur"].
     })

ƒ(define-element item
   #:contains-text
   #:children ([0+ list]
               [0+ note]
               [0+ p]
               [0+ pb]
               [0+ ab])
   #:prose ƒ[]{
 The ƒtag{item} element may contain
 a combination of free-form text and
 ƒtag{list}, ƒtag{note}, ƒtag{p}, ƒtag{pb}, and
 ƒtag{ab} elements.
 })

