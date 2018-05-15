#lang ricoeur/tei/kernel

ƒtitle[#:version ""]{Formal Specification}
ƒ(require (for-label racket
                     ricoeur/tei/xexpr/normalize
                     ))
ƒ(begin-for-runtime
   (require ricoeur/tei/xexpr/normalize
            (submod ricoeur/tei/kernel private)
            ))

Digital Ricœur imposes requirements for the structure of TEI
documents that are more stringent than merely being well-formed XML
that is valid with respect to the ƒtt{DR-TEI.dtd} Document Type
Definition.
The rest of this manual has introduced the structure of our documents
and these project-specific requirements in an informal tutorial style.
This section specifies these requirements in a precise, succinct
form suitable for reference once you understand the basics.

ƒsection{Document Structure}

The document should begin with an XML declaration and DOCTYPE
declaration, which must be exactly as follows:
ƒ(nested #:style 'inset
         (verbatim
          ƒtt{<?xml version="1.0" encoding="utf-8"?>}"\n"
          ƒtt{<!DOCTYPE TEI SYSTEM "DR-TEI.dtd">}))

ƒdefine-element[
 TEI
 #:children ([1 teiHeader]
             [1 text])
 #:required-order (teiHeader text)
 #:attr-contracts ([version "5.0"]
                   [xmlns "http://www.tei-c.org/ns/1.0"])
 #:required-attrs (version xmlns)]{

 The root element is a ƒtag{TEI} element,
 which contains exactly (in order)
 ƒtag{teiHeader} and ƒtag{text} elements.
 It must have the attributes
 ƒtt{version="5.0"} and
 ƒtt{xmlns="http://www.tei-c.org/ns/1.0"}.

}



ƒsection{The teiHeader Element}
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


ƒsubsection{The Title Statement}
ƒdefine-element[titleStmt
                #:children ([1+ title]
                            [1+ author]
                            [0+ editor])]{

 The ƒtag{titleStmt} contains
 one or more ƒtag{title} elements,
 one or more ƒtag{author} elements, and
 zero or more ƒtag{editor} elements.
 These may be intermixed freely any order, though the order of repeated
 elements indicates first author vs. second author,
 main title vs. subtitle, etc.

 ƒdefine-element[
 title
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
             val))))]{
  The ƒtag{title} element contains free-form text.
  It must not be empty.
 }

 ƒdefine-element[
 author
 #:inset? #t
 #:contains-text
 #:attr-contracts ([xml:id any/c])
 ]{
  The ƒtag{author} element contains free-form text and may have
  an optional ƒattr{xml:id} attribute. As a special case,
  the ID ƒracket["ricoeur"] is reserved for use with Paul Ricœur across all
  documents.
 }
                 
 ƒdefine-element[
 editor
 #:inset? #t
 #:contains-text
 #:attr-contracts ([role (or/c "editor"
                               "translator"
                               "compiler"
                               "preface")]
                   [xml:id any/c])]{

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

 } 
}


ƒsubsection{The Publication Statement}
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


ƒsubsection{The Source Description}
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


ƒsubsection{The Text Classification}
ƒdefine-element[
 textClass
 #:children ([1 catRef]
             [1 keywords])]{
                            
 The ƒtag{textClass} element must contain
 one ƒtag{catRef} element and one ƒtag{keywords} element.

 ƒdefine-element[
 catRef
 #:inset? #t
 #:attr-contracts
 ([scheme "https://schema.digitalricoeur.org/taxonomy/type"]
  [target (or/c "https://schema.digitalricoeur.org/taxonomy/type#article"
                "https://schema.digitalricoeur.org/taxonomy/type#book")])
 #:required-attrs (scheme target)]{
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
 }

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
}


ƒsection{The text Element}
ƒdefine-element[
 text
 #:children ([0-1 front]
             [1 body]
             [0-1 back])
 #:required-order (front body back)]{

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
}


ƒsubsection{Structural Elements}
ƒdefine-element[
 div
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
 #:required-attrs (type)]{
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
}

ƒdefine-element[
 pb
 #:attr-contracts ([n any/c])]{
 The ƒtag{pb} element has no contents.
 Unless the corresponding page is not numbered in the source,
 it should have an ƒattr{n} attribute giving the page number
 as given in the source (i.e. possibly as a Roman numeral).
}

ƒdefine-element[
 list
 #:children ([0+ head]
             [0+ item]
             [0+ pb])
 #:attr-contracts
 ([type (or/c "numbered" "bulleted")])]{
 The ƒtag{list} element may contain
 the ƒtag{head}, ƒtag{item}, and ƒtag{pb} elements.
 It may have a ƒattr{rend} attribute with a value of either
 ƒracket["numbered"] or ƒracket["bulleted"] (the default).

 ƒbold{TODO!!} Is the attribute supposed to be ƒattr{rend}
 or ƒattr{type}?
}

ƒdefine-element[
 sp
 #:children ([0+ list]
             [0+ p]
             [0+ pb]
             [0+ ab])
 #:attr-contracts ([who #rx"#.+"])
 #:required-attrs (who)]{
 The ƒtag{sp} ("speech") element must have a valid ƒattr{who}
 attribute and may contain ƒtag{list}, ƒtag{p}, ƒtag{pb}, or ƒtag{ab}
 elements.
}


ƒsubsection{Content-Containing Elements}
ƒdefine-element[
 ab
 #:contains-text
 #:children ([0+ list]
             [0+ note]
             [0+ pb])]{
 The ƒtag{ab} ("anonymous block")
 element may contain a combination of
 free-form text and ƒtag{list}, ƒtag{note},
 and ƒtag{pb} elements.
 It is used for blocks of text
 that have not yet been divided into more semantically
 meaningful elements.
}

ƒdefine-element[
 p
 #:contains-text
 #:children ([0+ list]
             [0+ note]
             [0+ pb])]{
 The ƒtag{p} ("paragraph")
 element may contain a combination of
 free-form text and ƒtag{list}, ƒtag{note},
 and ƒtag{pb} elements.
}

ƒdefine-element[
 head
 #:contains-text
 #:children ([0+ note])]{
 The ƒtag{head} ("heading")
 element may contain a combination of
 free-form text and ƒtag{note} elements.
}

ƒdefine-element[
 note
 #:contains-text
 #:children ([0+ list]
             [0+ note]
             [0+ p]
             [0+ ab])
 #:attr-contracts ([place (or/c "foot" "end")]
                   [resp #rx"#.+"]
                   [type "transl"]
                   [n any/c])
 #:required-attrs (place n)]{
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
}

ƒdefine-element[
 item
 #:contains-text
 #:children ([0+ list]
             [0+ note]
             [0+ p]
             [0+ pb]
             [0+ ab])]{
 The ƒtag{item} element may contain
 a combination of free-form text and
 ƒtag{list}, ƒtag{note}, ƒtag{p}, ƒtag{pb}, and
 ƒtag{ab} elements.
}









