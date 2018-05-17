#lang ricoeur/tei/kernel

ƒ[#:spec custom-spec]

ƒtitle{Another Test}

ƒ(define-element TEI
   #:children ([1 teiHeader]
               [1 text])
   #:required-order (teiHeader text)
   #:attr-contracts ([version "5.0"]
                     [xmlns "http://www.tei-c.org/ns/1.0"])
   #:required-attrs (version xmlns)
   #:prose ƒ{

 This isn't real.

 })

ƒ(define-element teiHeader
   #:contains-text
   #:attr-contracts ([some-attr (or/c "Apples"
                                      "oranges"
                                      "grapefruit")])
   #:extra-check
   (λ (val maybe-blame neg-party)
     (displayln "Yay!")
     val)
   #:prose ƒ{

 This isn't either.
         
 })