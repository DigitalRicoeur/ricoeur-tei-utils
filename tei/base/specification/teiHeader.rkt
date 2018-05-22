#lang ricoeur/tei/kernel

ƒ[#:spec teiHeader-spec]

ƒtitle{The teiHeader Element}

ƒ(require (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date
                                )
                     ))
ƒ(begin-for-runtime
   (require (submod ricoeur/tei/kernel private)
            ))

ƒdefine-elements-together[
 ([teiHeader
   #:required-order (fileDesc profileDesc)
   #:children ([1 fileDesc]
               [1 profileDesc])]
  [fileDesc
   #:children ([1 titleStmt]
               [1 publicationStmt]
               [1 sourceDesc])
   #:required-order (titleStmt publicationStmt sourceDesc)]
  [profileDesc
   #:children ([1 textClass])])]{

 The ƒtag{teiHeader} element contains (in order) one
 ƒtag{fileDesc} element followed by one
 ƒtag{profileDesc} element.

 The ƒtag{fileDesc} element contains exactly (in order)
 the ƒtag{titleStmt}, ƒtag{publicationStmt}, and
 ƒtag{sourceDesc} elements.

 The ƒtag{profileDesc} element contains only the
 ƒtag{textClass} element.

}


ƒsection{The Title Statement}
ƒ(define-element titleStmt
   #:children ([1+ title]
               [1+ author]
               [0+ editor])
   #:prose ƒ[]{

 The ƒtag{titleStmt} contains
 one or more ƒtag{title} elements,
 one or more ƒtag{author} elements, and
 zero or more ƒtag{editor} elements.
 These may be intermixed freely any order, though the order of repeated
 elements indicates first author vs. second author,
 main title vs. subtitle, etc.

 ƒ(define-element title
    #:inset? #t
    #:contains-text
    #:extra-check
    (λ (val maybe-blame neg-party)
      (or (regexp-match? #px"\\S"
                         (non-element-body->plain-text (get-body val)))
          (and maybe-blame
               (raise-blame-error
                maybe-blame #:missing-party neg-party
                val
                '("title element may not be empty"
                  given: "~e")
                val))))
    #:prose ƒ{
  The ƒtag{title} element contains free-form text.
  It must not be empty.
  })

 ƒ(define-element author
    #:inset? #t
    #:contains-text
    #:attr-contracts ([xml:id any/c])
    #:prose ƒ[]{
  The ƒtag{author} element contains free-form text and may have
  an optional ƒattr{xml:id} attribute. As a special case,
  the ID ƒracket["ricoeur"] is reserved for use with Paul Ricœur across all
  documents.
  })
                 
 ƒ(define-element editor
    #:inset? #t
    #:contains-text
    #:attr-contracts ([role (or/c "editor"
                                  "translator"
                                  "compiler"
                                  "preface")]
                      [xml:id any/c])
    #:prose ƒ[]{

  The ƒtag{editor} element contains free-form text and
  has optional ƒattr{role} and ƒattr{xml:id} attributes.
  If the ƒattr{role} attribute is present,
  its value must be either ƒracket["editor"], ƒracket["translator"],
  ƒracket["compiler"], or ƒracket["preface"] (to indicate the author of
  a preface). Ommiting the ƒattr{role} attribute is equivalent to a value
  of ƒracket["editor"].

  ƒmargin-note{If a type of editor arises that does not
   fit neatly into these categories, we should decide on a standard
   value for the ƒtt{role} attribute and amend this document.}

  }) 
 })


ƒsection{The Publication Statement}
ƒdefine-elements-together[
 ([publicationStmt
   #:children ([1 authority]
               [1 availability])]
  [authority
   #:contains-text]
  [availability
   #:children ([1 p])
   #:attr-contracts ([status "restricted"])
   #:required-attrs (status)])]{
 The ƒtag{publicationStmt}, which contains the ƒtag{authority}
 and ƒtag{availability} elements, should be exactly as follows:
 ƒ(nested
   #:style 'inset
   (verbatim
    ƒtt{<publicationStmt>}"\n"
    "  "ƒtt{<authority>Digital Ricoeur</authority>}"\n"
    "  "ƒtt{<availability status="restricted">}"\n"
    "    "ƒtt{<p>Not for distribution.</p>}"\n"
    "  "ƒtt{</availability>}"\n"
    ƒtt{</publicationStmt>}))
}


ƒsection{The Source Description}
ƒdefine-elements-together[
 ([sourceDesc
   #:children ([1 bibl])]
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
   #:required-attrs (when type subtype)])]{
 The ƒtag{sourceDesc} element must contain exactly one
 ƒtag{bibl} element.

 The ƒtag{bibl} element contains free-form text and
 one or two ƒtag{date} elements: either one with
 a ƒattr{subtype} attribute of ƒracket["thisIsOriginal"]
 or both one with a ƒattr{subtype} attribute of ƒracket["this"]
 and one with a ƒattr{subtype} attribute of ƒracket["original"].

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
}


ƒsection{The Text Classification}
ƒ(define-element textClass
   #:children ([1 catRef]
               [1 keywords])
   #:prose ƒ[]{
                            
 The ƒtag{textClass} element must contain
 one ƒtag{catRef} element and one ƒtag{keywords} element.

 ƒ(define-element catRef
    #:inset? #t
    #:attr-contracts
    ([scheme "https://schema.digitalricoeur.org/taxonomy/type"]
     [target (or/c "https://schema.digitalricoeur.org/taxonomy/type#article"
                   "https://schema.digitalricoeur.org/taxonomy/type#book")])
    #:required-attrs (scheme target)
    #:prose ƒ{
  The ƒtag{catRef} element contains nothing.
  It has two attributes, ƒattr{scheme} and ƒattr{target},
  both of which are required.
  This element encodes whether
  the document is a book or an article.
  The ƒattr{scheme} attribute must have the value
  ƒracket["https://schema.digitalricoeur.org/taxonomy/type"].
  The value for the ƒattr{target} attribute must be either:
  ƒitemlist[
 ƒitem{ƒracket["https://schema.digitalricoeur.org/taxonomy/type#article"],
    if the document is an article; or
   }
 ƒitem{ƒracket["https://schema.digitalricoeur.org/taxonomy/type#book"],
    if the document is a book.
    }]
  })

 ƒdefine-elements-together[
 #:inset? #t
 ([keywords
   #:attr-contracts
   ([scheme "https://schema.digitalricoeur.org/tools/tei-guess-paragraphs"])
   #:required-attrs (scheme)
   #:children ([1 term])]
  [term
   #:contains-text
   #:extra-check
   (λ (val maybe-blame neg-party)
     (define body-string
       (non-element-body->plain-text
        (get-body val)))
     (case body-string
       [("todo"
         "line-breaks"
         "blank-lines"
         "done"
         "skip")
        #t]
       [else
        (and maybe-blame
             (raise-blame-error
              maybe-blame #:missing-party neg-party
              val
              '("invalid contents of term element"
                expected: "(list/c (or/c \"todo\" \"line-breaks\" \"blank-lines\" \"done\" \"skip\"))"
                given: "~e"
                "\n  term element...:\n   ~e")
              body-string
              val))]))])]{
  The ƒtag{keywords} element is currently used to encode flags
  for the ƒtt{guess-paragraphs} tool.
  It must have a ƒattr{scheme} attribute with a value of
  ƒracket["https://schema.digitalricoeur.org/tools/tei-guess-paragraphs"].
  The ƒtag{keywords} element must contain exactly one ƒtag{term} element.

  The ƒtag{term} element contains immediate text, but its
  contents must conform to the vocabulary prescribed by the ƒattr{scheme}
  attribute of its parent ƒtag{keywords} element.
  Currently, its contents must be exactly one of the following:
  ƒlitchar{todo}; ƒlitchar{line-breaks}; ƒlitchar{blank-lines}; ƒlitchar{done};
  or ƒlitchar{skip}.
  Only the ƒlitchar{todo} value should be entered manually.
 }                                 
 })



