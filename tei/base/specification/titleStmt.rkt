#lang ricoeur/tei/spec-lang                                                                   

ƒtitle{The Title Statement}

ƒbegin-for-runtime[
 (module+ private-to-teiHeader
   (provide titleStmt-title
            titleStmt-resp-table))
 (require (submod ricoeur/tei/kernel private)
          xml/path)
 ]

ƒ(require (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date)))

ƒ(define-element titleStmt
   #:children ([1 title]
               [1+ author]
               [0+ editor])
   #:extra-check
   (λ (val maybe-blame neg-party)
     (or (member "ricoeur"
                 (se-path*/list `(author #:xml:id)
                                val))
         (and maybe-blame
              (raise-blame-error
               maybe-blame #:missing-party neg-party
               val
               '(expected: "~a"
                           given: "~e")
               "an author element with xml:id=\"ricoeur\""
               val))))
   #:predicate tei-titleStmt?
   #:constructor [
 #:body/elements-only body/elements-only
 (define (child->plain-text child)
   (string-normalize-spaces
    (string-trim
     (non-element-body->plain-text
      (tei-element-get-body child)))))
 (define/field #:infer title
   (string->immutable-string
    (child->plain-text
     (findf tei-title? body/elements-only))))
 (define/field [resp-table
                #:check (hash/c	symbol? (and/c string? immutable?)
                                #:immutable #t)
                #:accessor titleStmt-resp-table]
   (for*/hasheq ([child (in-list body/elements-only)]
                 #:when (or (tei-author? child)
                            (tei-editor? child))
                 [maybe-id-str
                  (in-value (attributes-ref
                             (tei-element-get-attributes child)
                             'xml:id))]
                 #:when maybe-id-str)
     (values (string->symbol maybe-id-str)
             (string->immutable-string
              (child->plain-text child)))))
 #|END constructor|#]
   #:prose ƒ[]{

 The ƒtag{titleStmt} contains
 one ƒtag{title} element,
 one or more ƒtag{author} elements, and
 zero or more ƒtag{editor} elements.
 These may be intermixed freely in any order.

 As a special case, there must always be an ƒtag{author} element
 representing Paul Ricœur, which should be exactly as follows:
 ƒ(nested #:style 'inset
          ƒlitchar{<author xml:id="ricoeur">Paul Ricoeur</author>})
 
 ƒ(define-element title
    #:inset? #t
    #:contains-text
    #:extra-check
    (λ (val maybe-blame neg-party)
      (or (regexp-match? #px"[^\\s]"
                         (non-element-body->plain-text (get-body val)))
          (and maybe-blame
               (raise-blame-error
                maybe-blame #:missing-party neg-party
                val
                '("title element may not be empty"
                  given: "~e")
                val))))
    #:predicate tei-title?
    #:prose ƒ{
  The ƒtag{title} element contains free-form text.
  It must not be empty.
  })

 ƒ(define-element author
    #:inset? #t
    #:contains-text
    #:attr-contracts ([xml:id any/c])
    #:predicate tei-author?
    #:prose ƒ[]{
  The ƒtag{author} element contains free-form text
  (the author's name) and may have
  an optional ƒattr{xml:id} attribute. As a special case,
  the ID ƒracket["ricoeur"] is reserved for use with Paul Ricœur across all
  documents. ƒTODO/void[author: not empty]
  })
                 
 ƒ(define-element editor
    #:inset? #t
    #:contains-text
    #:attr-contracts ([role (or/c "editor"
                                  "translator"
                                  "compiler"
                                  "preface")]
                      [xml:id (not/c "ricoeur")])
    #:predicate tei-editor?
    #:prose ƒ[]{

  The ƒtag{editor} element contains free-form text (the editor's name)
  and has optional ƒattr{role} and ƒattr{xml:id} attributes.
  If the ƒattr{role} attribute is present,
  its value must be either ƒracket["editor"], ƒracket["translator"],
  ƒracket["compiler"], or ƒracket["preface"] (to indicate the author of
  a preface). Ommiting the ƒattr{role} attribute is equivalent to a value
  of ƒracket["editor"]. ƒTODO/void[editor: not empty]

  ƒmargin-note{If a type of editor arises that does not
   fit neatly into these categories, we should decide on a standard
   value for the ƒtt{role} attribute and amend this document.}

  }) 
 })
