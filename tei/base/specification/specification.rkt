#lang ricoeur/tei/kernel

ƒ[#:spec main-spec
  #:extends teiHeader-spec text-spec
  #:with-local local-spec]

ƒtitle[#:version ""]{Formal Specification}

ƒ(require (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date
                                )
                     ))
ƒ(begin-for-runtime
   (provide TEI?
            )
   (require "teiHeader.rkt"
            "text.rkt"
            ))

Digital Ricœur imposes requirements for the structure of TEI
documents that are more stringent than merely being well-formed XML
that is valid with respect to the ƒtt{DR-TEI.dtd} Document Type
Definition.
The rest of this manual has introduced the structure of our documents
and these project-specific requirements in an informal tutorial style.
This section specifies these requirements in a precise, succinct
form suitable for reference once you understand the basics.

To ensure that the prose documenting the required structure of
the TEI XML elements remains in sync with the Racket code that
enforces those requirements, this section is generated from
the same source file that defines the Racket enforcement code.

ƒsection{Document Structure}
ƒ(define-element TEI
   #:children ([1 teiHeader]
               [1 text])
   #:required-order (teiHeader text)
   #:attr-contracts ([version "5.0"]
                     [xmlns "http://www.tei-c.org/ns/1.0"])
   #:required-attrs (version xmlns)
   #:predicate TEI?
   #:constructor
   [#:body/elements-only body/elements-only
    (field teiHeader)
    (field text)
    (match-define (list teiHeader text)
      body/elements-only)
    (lift-property prop:TEI-info
                   (λ (this)
                     (get-plain-TEI-info
                      (get-field teiHeader this))))]
   #:prose ƒ{

 The document should begin with an XML declaration and DOCTYPE
 declaration, which must be exactly as follows:
 ƒ(nested #:style 'inset
          (verbatim
           ƒtt{<?xml version="1.0" encoding="utf-8"?>}"\n"
           ƒtt{<!DOCTYPE TEI SYSTEM "DR-TEI.dtd">}))

 The root element is a ƒtag{TEI} element,
 which contains exactly (in order)
 ƒtag{teiHeader} and ƒtag{text} elements.
 It must have the attributes
 ƒtt{version="5.0"} and
 ƒtt{xmlns="http://www.tei-c.org/ns/1.0"}.

 })



ƒinclude-section[(submod "teiHeader.rkt" doc)]
ƒinclude-section[(submod "text.rkt" doc)]


