#lang ricoeur/tei/spec-lang

ƒtitle{The Source Description}

ƒbegin-for-runtime[
 (module+ private-to-teiHeader
   (provide sourceDesc-citation
            sourceDesc-orig-publ-date
            sourceDesc-this-publ-date
            sourceDesc-this-is-orig?))
 (require (submod ricoeur/tei/kernel private)
          xml/path)
 ]

ƒ(require (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date)))

ƒ(define (lib-tech . args)
   (TODO/void lib-tech: avoid duplicating in for-guidelines.rkt)
   (TODO/void lang issue)
   ;; Un-commenting the following causes a use-before-definition error
   ;; with a lifted variable (possibly related to the contract on tech)
   ;; for reasons I don't understand. It would seem to be a bug in the #lang.
   ;; (apply tech #:doc '(lib "ricoeur/tei/scribblings/tei-utils/ricoeur-tei-utils.scrbl") args)
   args)

ƒdefine-elements-together[
 ([sourceDesc
   #:children ([1 bibl])
   #:constructor
   [#:body/elements-only body/elements-only
    (match-define (list bibl)
      body/elements-only)
    (define/field [citation #:accessor sourceDesc-citation]
      (string->immutable-string
       (string-normalize-spaces
        (element-or-xexpr->plain-text bibl))))
    (define-values/fields #:infer (orig-publ-date
                                   this-publ-date
                                   this-is-orig?)
      (match (for/list ([c (in-list (tei-element-get-body bibl))]
                        #:when (tei-date-element? c))
               (cons (date-when c) (date-subtype c)))
        [(list (cons d 'thisIsOriginal))
         (values d d 'thisIsOriginal)]
        [(or (list (cons this 'this) (cons original 'original))
             (list (cons original 'original) (cons this 'this)))
         (values original this #f)]))
    #|END sourceDesc|#]]
  [bibl
   #:contains-text
   #:children ([1+ date])
   #:extra-check
   (λ (val maybe-blame neg-party)
     (match (for/list ([c (in-list (get-body val))]
                       #:when (and (list? c)
                                   (eq? 'date (car c))))
              (cadr (assq 'subtype (get-attributes c))))
       [(or (list-no-order "this" "original")
            (list "thisIsOriginal"))
        #t]
       [bad-subtypes
        (and maybe-blame
             (raise-blame-error 
              maybe-blame #:missing-party neg-party
              val
              '("invalid combination of date element subtypes inside bibl element"
                expected: "'(\"this\" \"original\") '(\"original\" \"this\") or '(\"thisIsOriginal\")"
                given: "~e"
                "\n  bibl element...:\n   ~e")
              bad-subtypes
              val))]))]
  [date
   #:contains-text
   #:attr-contracts ([when #px"^(\\d\\d\\d\\d)(-\\d\\d)?(-\\d\\d)?$"]
                     [type "publication"]
                     [subtype (or/c "this" "original" "thisIsOriginal")])
   #:required-attrs (when type subtype)
   #:predicate tei-date-element?
   #:constructor 
   [#:attributes attrs
    (define-fields
      #:infer
      [subtype (string->symbol (attributes-ref attrs 'subtype))]
      [when (iso8601->date (attributes-ref attrs 'when))]
      #|END bibl|#)]])]{
 The ƒtag{sourceDesc} element must contain exactly one
 ƒtag{bibl} element.

 The ƒtag{bibl} element contains free-form text,
 which provides a citation to the source from which
 the digitized document was created, and either
 one or two ƒtag{date} elements mark up the parts of that text
 which refer to the publication date(s):
 ƒitemlist[
 ƒitem{If the digitized document is based on the first
   ƒlib-tech{instance} to be published in any language,
   there must be one ƒtag{date} element with a ƒattr{subtype}
   of ƒracket["thisIsOriginal"] marking that date.
  }
 ƒitem{Otherwise, there must be two ƒtag{date} elements:
   one with a ƒattr{subtype} of ƒracket["this"] marking the
   publication date of the specific ƒlib-tech{instance} from
   which the digitized document was prepared,
   and one with a ƒattr{subtype} of ƒracket["original"] giving
   the first publication date in any language.
   }]

 For compilations of articles, the ƒracket["thisIsOriginal"]
 or ƒracket["original"] ƒtag{date} refers to the first 
 publication of the collection as a whole.
 
 In either case, make sure that the textual content of the
 ƒtag{bibl} element as a whole is understandable if
 displayed without the tags, as it will be shown in that format
 to end-users.

 The ƒtag{date} element contains free-form text
 representing the human-readable publication date.
 It must have:
 ƒitemlist[
 ƒitem{a ƒattr{type} attribute with a value of ƒracket["publication"];}
 ƒitem{a ƒattr{subtype} attribute with a value of ƒracket["this"],
   ƒracket["original"], or ƒracket["thisIsOriginal"]; and}
 ƒitem{a ƒattr{when} attribute giving the
   date in machine-readable format, which must be a subset of
   the format required by TEI: ƒracket["YYYY-MM-DD"], ƒracket["YYYY-MM"],
   or ƒracket["YYYY"], where the month
   and day, if present, must allways be two digits
   (e.g. ƒtt{01} for January).}]

 Examples:
 ƒ(itemlist
   ƒitem{ƒtt{ƒlitchar{<bibl>}Fallible Man. Introduction by C.A. Kelbley.
   Chicago: Henry Regnery,
   ƒlitchar{<date type="publication" subtype="this" when="1965">}1965ƒlitchar{</date>},
   xxix-224 p. (Paper: Gateway Editions).
   Revised edition in 1986.
   First published in French in
   ƒlitchar{<date type="publication" subtype="original" when="1960">}1960@litchar{</date>}.@litchar{</bibl>}}}
   ƒitem{ƒtt{ƒlitchar{<bibl>}Can Fictional Narratives be true? 
   Analecta Hus­serliana Vol. XIV
   (ƒlitchar{<date type="publication" subtype="thisIsOriginal" when="1983">}1983ƒlitchar{</date>})
   3-19.ƒlitchar{</bibl>}}})
}