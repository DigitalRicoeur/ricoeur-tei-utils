#lang ricoeur/tei/spec-lang

ƒ[#:spec teiHeader-spec
  #:extends titleStmt-spec sourceDesc-spec textClass-spec
  #:with-local local-spec]

ƒtitle{The ƒtt{teiHeader} Element}

ƒbegin-for-runtime[
 (module+ private-to-base
   (provide teiHeader? 
            profileDesc? 
            textClass?
            tei-keywords?))
 (module+ private-to-spec
   (provide tH-title
            tH-resp-table
            tH-citation
            tH-orig-publication-date
            tH-publication-date
            tH-publication-original?
            tH-book/article))
 (require (submod ricoeur/tei/kernel private)
          (rename-in "titleStmt.rkt" [spec titleStmt-spec])
          (submod "titleStmt.rkt" private-to-teiHeader)
          (submod "sourceDesc.rkt" private-to-teiHeader)
          (rename-in "sourceDesc.rkt" [spec sourceDesc-spec])
          (submod "textClass.rkt" private-to-teiHeader)
          (rename-in "textClass.rkt" [spec textClass-spec])
          xml/path)
 ]

ƒ(require (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date)))

ƒdefine-elements-together[
 ([teiHeader
   #:required-order (fileDesc profileDesc)
   #:children ([1 fileDesc]
               [1 profileDesc])
   #:predicate teiHeader?
   #:begin
   [(define-syntax-rule (define-teiHeader-accessors
                          [name proc field] ...)
      (begin (define (name it)
               (proc (get-field field it)))
             ...))
    (define-teiHeader-accessors
      [tH-title fileDesc-title fileD]
      [tH-resp-table fileDesc-resp-table fileD]
      [tH-citation fileDesc-citation fileD]
      [tH-orig-publication-date fileDesc-orig-publ-date fileD]
      [tH-publication-date fileDesc-this-publ-date fileD]
      [tH-publication-original? fileDesc-this-is-orig? fileD]
      [tH-book/article textClass-book/article textC]
      )]
   #:constructor
   [#:body/elements-only body/elements-only
    (field fileD #:hide)
    (field textC #:hide)
    (match-define (list fileD (app profileDesc-textClass
                                   textC))
      body/elements-only)
    (declare-paragraphs-status-field
     (textClass-guess-paragraphs-status textC))
    #|END teiHeader|#]]
  [fileDesc
   #:children ([1 titleStmt]
               [1 publicationStmt]
               [1 sourceDesc])
   #:required-order (titleStmt publicationStmt sourceDesc)
   #:constructor [
 #:body/elements-only body/elements-only
 (match-define (list titleS _ sourceD)
   body/elements-only)
 (define-fields
   #:infer
   [title (titleStmt-title titleS)]
   [resp-table (titleStmt-resp-table titleS)]
   [citation (sourceDesc-citation sourceD)]
   [orig-publ-date (sourceDesc-orig-publ-date sourceD)]
   [this-publ-date (sourceDesc-this-publ-date sourceD)]
   [this-is-orig? (sourceDesc-this-is-orig? sourceD)]
   #|END fileDesc|#)]]
  [profileDesc
   #:children ([1 textClass])
   #:predicate profileDesc?
   #:begin
   [(define (profileDesc-textClass this)
      (car (tei-get-body/elements-only this)))]])]{

 The ƒtag{teiHeader} element contains exactly (in order) one
 ƒtag{fileDesc} element followed by one
 ƒtag{profileDesc} element.

 The ƒtag{fileDesc} element contains exactly (in order)
 the ƒtag{titleStmt}, ƒtag{publicationStmt}, and
 ƒtag{sourceDesc} elements.

 The ƒtag{profileDesc} element contains only the
 ƒtag{textClass} element.

}





ƒinclude-section[(submod "titleStmt.rkt" doc)]
                                                                                         

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


ƒinclude-section[(submod "sourceDesc.rkt" doc)]
ƒinclude-section[(submod "textClass.rkt" doc)]

