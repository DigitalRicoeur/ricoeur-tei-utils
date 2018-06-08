#lang ricoeur/tei/kernel

ƒ[#:spec main-spec
  #:extends teiHeader-spec text-spec
  #:with-local local-spec]

ƒtitle[#:version "" #:style '(toc)]{Formal Specification}

ƒ(require (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date
                                )
                     ))
ƒ(begin-for-runtime
   (provide tei-document?
            tei-document-text-element
            (contract-out
             [write-tei-document
              (->* {tei-document?} {output-port?} any)]
             [tei-document-md5
              (-> tei-document? string?)]
             ))
   (require "teiHeader.rkt"
            "text.rkt"
            xml
            openssl/md5
            racket/promise
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

ƒ;section{Document Structure}
ƒ(define-element TEI
   #:children ([1 teiHeader]
               [1 text])
   #:required-order (teiHeader text)
   #:attr-contracts ([version "5.0"]
                     [xmlns "http://www.tei-c.org/ns/1.0"])
   #:required-attrs (version xmlns)
   #:predicate tei-document?
   #:constructor
   [#:body/elements-only body/elements-only
    #:this/thunk get-this
    (field teiHeader #:hide)
    (field text #:accessor tei-document-text-element #:hide)
    (match-define (list teiHeader text)
      body/elements-only)
    (declare-paragraphs-status-field
     (tei-document-paragraphs-status teiHeader))
    (define/field pr:md5
      (delay/thread
       (TODO/void Should md5 really use prettyprint?
                  #: Might be a lot faster to just use write-xexpr
                  and not launch a subprocess.
                  I think the original rationale was not being sensitive
                  to |"changes"| like comments and ignored whitespace:
                  maybe I should handle those explicitly.
                  Prettyprint would still be sensitive to "comments,"
                  and ignored whitespace is already dropped.
                  )
       (define-values (in-from-pipe out-to-pipe)
         (make-pipe))
       (write-xexpr (tei-element->xexpr (get-this)) out-to-pipe)
       ;(write-TEI (get-this) out-to-pipe)
       (close-output-port out-to-pipe)
       (md5 in-from-pipe)))]
   #:property prop:TEI-info (λ (this)
                              (get-plain-TEI-info
                               (get-field teiHeader this)))
   #:begin [(define (tei-document-md5 doc)
              (force (get-field pr:md5 doc)))]
   #:property prop:element->plain-text
   (λ (this)
     (string-append (tei-title this)
                    "\n\n"
                    (tei-citation this)
                    "\n\nDigital Ricoeur: Not for distribution.\f"
                    (element-or-xexpr->plain-text
                     (get-field text this))))
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

ƒ(begin-for-runtime
   (define (write-tei-document doc [out (current-output-port)])
     (parameterize ([current-output-port out])
       (call/prettyprint-xml-out
        (λ () 
          (displayln ƒstring-append{
 <?xml version="1.0" encoding="utf-8"?>
 <!DOCTYPE TEI SYSTEM "DR-TEI.dtd">})
          (write-xexpr (tei-element->xexpr doc) out))))))


ƒ(local-table-of-contents)
         
ƒinclude-section[(submod "teiHeader.rkt" doc)]
ƒinclude-section[(submod "text.rkt" doc)]


