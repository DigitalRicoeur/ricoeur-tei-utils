#lang ricoeur/tei/kernel

ƒ[#:spec custom-spec]

ƒtitle{Another Test}

ƒdefine-element[
 TEI
 #:children ([1 teiHeader]
             [1 text])
 #:required-order (teiHeader text)
 #:attr-contracts ([version "5.0"]
                   [xmlns "http://www.tei-c.org/ns/1.0"])
 #:required-attrs (version xmlns)]{

 This isn't real.

}

ƒdefine-element[
 teiHeader
 #:contains-text
 #:attr-contracts ([some-attr (or/c "Apples"
                                    "oranges"
                                    "grapefruit")])
 #:extra-check
 (λ (val maybe-blame neg-party)
   (displayln "Yay!")
   val)]{

 This isn't either.
         
}