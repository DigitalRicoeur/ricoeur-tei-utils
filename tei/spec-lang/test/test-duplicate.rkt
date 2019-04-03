#lang ricoeur/tei/spec-lang

ƒ[#:spec duplicate-spec]

ƒ(define-element TEI
   #:children ([1 teiHeader]
               [1 text])
   #:required-order (teiHeader text)
   #:attr-contracts ([version "5.0"]
                     [xmlns "http://www.tei-c.org/ns/1.0"])
   #:required-attrs (version xmlns)
   #:predicate TEI?
   #:prose ƒ{

 This is a duplicate of ƒtt{test3.rkt}.

 })
