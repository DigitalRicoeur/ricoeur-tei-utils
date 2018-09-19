#lang ricoeur/tei/kernel

ƒ[#:spec text-spec]

ƒtitle{The ƒtt{text} Element}

ƒbegin-for-runtime[
 (provide tei-pb?
          tei-note?
          div?
          div-type/c
          (contract-out
           [pb-get-page-string
            (-> tei-pb? (maybe/c string-immutable/c))]
           [pb-get-kind
            (-> tei-pb? (or/c 'none 'number 'roman 'other))]
           [pb-get-numeric
            (-> tei-pb? (maybe/c natural-number/c))]
           [tei-note-get-place
            (-> tei-note? (or/c 'foot 'end))]
           [tei-note-get-n
            (-> tei-note? string-immutable/c)]
           [tei-note-get-transl?
            (-> tei-note? (or/c #f 'transl))]
           [div-get-type
            (-> div? div-type/c)]
           [div-get-n
            (-> div? (maybe/c string-immutable/c))]
           ))
 (module+ private-to-base
   (provide tei-ab? 
            tei-text-element? 
            text-lang))
 (require (submod ricoeur/tei/kernel private)
          roman-numeral)
 ]

ƒ(require-self-for-label)
ƒ(require (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date)))


ƒ(define-element text
   #:children ([0-1 front]
               [1 body]
               [0-1 back])
   #:required-order (front body back)
   #:attr-contracts
   ([xml:lang (or/c "en" "fr" "de")])
   #:required-attrs (xml:lang)
   #:predicate tei-text-element?
   #:constructor [
 #:attributes attrs
 (define/field #:infer lang
   (string->symbol
    (attributes-ref attrs 'xml:lang)))]
   #:prose ƒ{

  The ƒtag{text} element may contain only (in order) 
  a ƒtag{front} element, a ƒtag{body} element,
  and a ƒtag{back} element,
  but the ƒtag{front} and ƒtag{back} elements are optional.
  It must have a ƒattr{xml:lang} attribute specifying the
  primary language of the document: ƒracket["en"] for English;
  ƒracket["fr"] for French; or ƒracket["de"] for German.

  ƒdefine-elements-together[
 #:inset? #t
 ;; This is where I miss being able to say that
 ;; body+front+back have the same children
 ([body
   #:children ([0+ head]
               [0+ p]
               [0+ pb]
               [0+ ab]
               [0+ div])]
  [front
   #:children ([0+ head]
               [0+ p]
               [0+ pb]
               [0+ ab]
               [0+ div])]
  [back
   #:children ([0+ head]
               [0+ p]
               [0+ pb]
               [0+ ab]
               [0+ div])])]{
   The ƒtag{body}, ƒtag{front}, and ƒtag{back} elements
   may contain ƒtag{head}, 
   ƒtag{p}, ƒtag{pb}, ƒtag{ab}, and ƒtag{div} elements.
  }
  })


ƒsection{Structural Elements}
ƒ(define-element div
   #:children ([0+ head]
               [0+ list]
               [0+ p]
               [0+ pb]
               [0+ ab]
               [0+ sp]
               [0+ div])
   #:attr-contracts
   ([type (or/c "chapter"
                "part"
                "section"
                "dedication"
                "contents"
                "intro"
                "bibl"
                "ack"
                "index")]
    [n string?]
    [resp #rx"^#.+$"])
   #:required-attrs (type)
   #:predicate div?
   #:begin [
 ;; TODO: Extend DSL to integrate this w/
 ;; #:attr-contracts contract
 (define/final-prop div-type/c
   (or/c 'chapter 'part 'section 'dedication
         'contents 'intro 'bibl 'ack 'index))]
   #:constructor [
 #:attributes attrs
 (declare-resp-field attrs)
 (define-fields
   #:infer
   [get-type (string->symbol (attributes-ref attrs 'type))]
   [get-n (fmap string->immutable-string
                (false->maybe (attributes-ref attrs 'n)))])
 #|END #:constructor|#]
   #:prose ƒ[]{
 The ƒtag{div} element may contain
 ƒtag{head}, ƒtag{list}, ƒtag{p},
 ƒtag{pb}, ƒtag{ab}, ƒtag{sp}, or nested ƒtag{div} elements.
 It must have a ƒattr{type} attribute with a value of
 ƒracket["chapter"], ƒracket["part"], ƒracket["section"],
 ƒracket["dedication"], ƒracket["contents"], ƒracket["intro"],
 ƒracket["bibl"] (for a bibliography),
 ƒracket["ack"] (for acknowledgements), or ƒracket["index"].
 If the division is numbered, it should have an
 ƒattr{n} attribute giving the page number
 as given in the source (i.e. possibly as a Roman numeral).
 Any ƒtag{div} element may, and a ƒtag{div} representing a
 section not by Paul Ricœur must, have a ƒattr{resp} attribute
 that is valid per the TEI standard, as discussed above under
 ƒsecref["Sections_Not_by_Ric_ur"].
 ƒmargin-note{If a type of division arises that does not
  fit neatly into these categories, we should decide on a standard
  value for the ƒtt{type} attribute and amend this document.}
 })

ƒ(define-element pb
   #:attr-contracts ([n string?])
   #:prose ƒ[]{
 The ƒtag{pb} element has no contents.
 Unless the corresponding page is not numbered in the source,
 it should have an ƒattr{n} attribute giving the page number
 as given in the source (i.e. possibly as a Roman numeral).
}
   #:predicate tei-pb?
   #:property prop:element->plain-text (λ (this) "\f")
   #:constructor [
 #:attributes attrs
 (define n
   (attributes-ref attrs 'n))
 (define/field #:infer get-page-string
   (fmap string->immutable-string (false->maybe n)))
 (define-values/fields #:infer (get-kind get-numeric)
   (match n
     [#f (values 'none nothing)]
     [(app string->number (? exact-nonnegative-integer? it))
      (values 'number (just it))]
     [(app (λ (n)
             (exn->maybe exn:fail? roman->number n))
           (just it))
      (values 'roman (just it))]
     [_
      (values 'other nothing)]))
 #|END pb|#])

ƒ(define-element list
   #:children ([0+ head]
               [0+ item]
               [0+ pb])
   #:attr-contracts
   ([rend (or/c "numbered" "bulleted")])
   #:prose ƒ[]{
 The ƒtag{list} element may contain
 the ƒtag{head}, ƒtag{item}, and ƒtag{pb} elements.
 It may have a ƒattr{rend} attribute with a value of either
 ƒracket["numbered"] or ƒracket["bulleted"] (the default).
 })

ƒ(define-element sp
   #:children ([0+ list]
               [0+ p]
               [0+ pb]
               [0+ ab])
   #:attr-contracts ([who #rx"^#.+$"])
   #:required-attrs (who)
   #:constructor [#:attributes attrs
                  (declare-resp-field attrs #:key who)]
   #:prose ƒ[]{
 The ƒtag{sp} ("speech") element must have a valid ƒattr{who}
 attribute and may contain ƒtag{list}, ƒtag{p}, ƒtag{pb}, or ƒtag{ab}
 elements.
 })


ƒsection{Content-Containing Elements}
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









