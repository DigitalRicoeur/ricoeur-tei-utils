#lang ricoeur/tei/spec-lang

ƒtitle{The Text Classification}

ƒbegin-for-runtime[
 (provide textClass?
          tei-keywords?)
 (module+ private-to-teiHeader
   (provide textClass-book/article
            textClass-guess-paragraphs-status
            ))
 (require (submod ricoeur/tei/kernel private)
          xml/path)
 ]

ƒ(define-element textClass
   #:children ([1 catRef]
               [1 keywords])
   #:predicate textClass?
   #:constructor
   [#:body/elements-only body/elements-only
    (field guess-paragraphs-status #:infer)
    (match-define
      (list-no-order (? tei-keywords?
                        (app keywords-guess-paragraphs-status
                             guess-paragraphs-status))
                     (? catRef? catRef))
      body/elements-only)
    (define/field #:infer book/article
      (case (attributes-ref (tei-element-get-attributes catRef)
                            'target)
        [("https://schema.digitalricoeur.org/taxonomy/type#article")
         'article]
        [("https://schema.digitalricoeur.org/taxonomy/type#book")
         'book]))]
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
    #:predicate catRef?
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
   #:children ([1 term])
   #:predicate tei-keywords?
   #:constructor
   [#:body/elements-only body/elements-only
    (field guess-paragraphs-status #:infer)
    (match-define (list (app term-guess-paragraphs-status
                             guess-paragraphs-status))
      body/elements-only)
    #|END keywords|#]]
  [term
   #:contains-text
   #:extra-check
   (λ (val maybe-blame neg-party)
     (define body-string
       (non-element-body->plain-text
        (get-body val)))
     (case body-string
       [("todo" "line-breaks" "blank-lines" "done" "skip")
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
              val))]))
   #:constructor
   [#:body body
    (define body-string
      (non-element-body->plain-text body))
    (define/field #:infer guess-paragraphs-status
      (case body-string
        [("todo") 'todo]
        [("line-breaks") 'line-breaks]
        [("blank-lines") 'blank-lines]
        [("done") 'done]
        [("skip") 'skip]))
    #|END term|#]])]{
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