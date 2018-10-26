#lang ricoeur/tei/kernel

ƒtitle{Structural Elements}

ƒbegin-for-runtime[
 (provide tei-pb?
          div?
          div-type/c
          (contract-out
           [pb-get-page-string
            (-> tei-pb? (maybe/c string-immutable/c))]
           [pb-get-kind
            (-> tei-pb? (or/c 'none 'number 'roman 'other))]
           [pb-get-numeric
            (-> tei-pb? (maybe/c natural-number/c))]
           [div-get-type
            (-> div? div-type/c)]
           [div-get-n
            (-> div? (maybe/c string-immutable/c))]
           ))
 (require (submod ricoeur/tei/kernel private)
          roman-numeral)
 ]

ƒ(require-self-for-label)
ƒ(require (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date)))

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



