#lang ricoeur/tei/kernel

ƒ[#:spec text-spec]

ƒtitle{The text Element}

ƒ(require ricoeur/lib
          (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date
                                )
                     ))

ƒ(define-element text
   #:children ([0-1 front]
               [1 body]
               [0-1 back])
   #:required-order (front body back)
   #:prose ƒ{

 The ƒtag{text} element may contain only (in order) 
 a ƒtag{front} element, a ƒtag{body} element,
 and a ƒtag{back} element,
 but the ƒtag{front} and ƒtag{back} elements are optional.

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
                "bibl"
                "ack"
                "intro"
                "index")]
    [resp #rx"#.+"])
   #:required-attrs (type)
   #:prose ƒ{
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
   #:attr-contracts ([n any/c])
   #:prose ƒ[]{
 The ƒtag{pb} element has no contents.
 Unless the corresponding page is not numbered in the source,
 it should have an ƒattr{n} attribute giving the page number
 as given in the source (i.e. possibly as a Roman numeral).
 })

ƒ(define-element list
   #:children ([0+ head]
               [0+ item]
               [0+ pb])
   #:attr-contracts
   ([type (or/c "numbered" "bulleted")])
   #:prose ƒ[]{
 The ƒtag{list} element may contain
 the ƒtag{head}, ƒtag{item}, and ƒtag{pb} elements.
 It may have a ƒattr{rend} attribute with a value of either
 ƒracket["numbered"] or ƒracket["bulleted"] (the default).

 ƒTODO/scrbl[[list element attribute #: Should it be rend or type?]]{
  ƒbold{TODO!!} Is the attribute supposed to be ƒattr{rend}
  or ƒattr{type}?}
 })

ƒ(define-element sp
   #:children ([0+ list]
               [0+ p]
               [0+ pb]
               [0+ ab])
   #:attr-contracts ([who #rx"#.+"])
   #:required-attrs (who)
   #:prose ƒ{
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
 })

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
                     [resp #rx"#.+"]
                     [type "transl"]
                     [n any/c])
   #:required-attrs (place n)
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









