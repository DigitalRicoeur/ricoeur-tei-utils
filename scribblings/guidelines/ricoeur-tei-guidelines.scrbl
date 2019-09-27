#lang scribble/manual

@title[#:version ""]{TEI Encoding Guidelines for Digital Ricœur}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com"
                      #:obfuscate? #t)
        @elem{Digital Ricœur}]

@(require "for-guidelines.rkt")

This manual specifies how to encode texts for Digital Ricœur.

A foundational assumption is that we want to
complete an initial encoding quickly by keeping the complexity of TEI
tagging process to a minimum, while still producing well-formed XML
documents that conform to the TEI standards.
Our initial priorities are marking pagebreaks
and recording basic catalog information.

To that end, we have defined a custom subset of TEI, which is codified
in the @tt{DR-TEI.dtd} Document Type Definition (DTD).
(A DTD is a formal, computer-checkable specification of the structure of
an XML document.)
To ensure consistency and facilitate the development of tools,
we also impose additional requirements beyond those specified by the DTD.
These requirements are documented in this manual.

@margin-note{
 For additional documentation pertaining only to the requirements specified
 by the DTD, see @|DR-TEI-link|.
}

We have implemented a number of tools
to assist in preparing and validating TEI XML documents,
which are documented under @secref["Tools"].

@(table-of-contents)

@include-section["background.scrbl"]
@include-section["getting-started.scrbl"]
@include-section["refining.scrbl"]
@include-section[(submod ricoeur/tei/base/specification/specification doc)]
@include-section["tools.scrbl"]

