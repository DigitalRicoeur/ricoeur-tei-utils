#lang ricoeur/tei/spec-lang

ƒ[#:spec main-spec
  #:extends teiHeader-spec text-spec
  #:with-local local-spec]

ƒtitle[#:version "" #:style '(toc)]{Formal Specification}

ƒbegin-for-runtime[
 (provide tei-document?
          (contract-out
           [write-tei-document
            (->* {tei-document?} {output-port?} any)]
           [tei-document-checksum
            (-> tei-document? symbol?)]
           [tei-document->plain-text
            (->* {tei-document?}
                 {#:include-header? any/c}
                 string-immutable/c)]
           ))
 (module+ private-to-base
   (provide tei-document-text-element)
   (require-provide (submod "teiHeader.rkt" private-to-base)
                    (submod "text.rkt" private-to-base)))
 (require-provide (except-out "teiHeader.rkt" teiHeader-spec)
                  (except-out "text.rkt" text-spec))
 (require (submod "teiHeader.rkt" private-to-base)
          (submod "teiHeader.rkt" private-to-spec)
          (submod "text.rkt" private-to-base)
          xml
          openssl/md5
          racket/promise
          (submod ricoeur/tei/kernel
                  private-plain-instance-info))
 ]
ƒ(require (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date)))

Digital Ricœur imposes requirements for the structure of TEI
documents that are more stringent than merely being well-formed XML
that is valid with respect to the ƒtt{DR-TEI.dtd} Document Type
Definition.

The rest of this manual has introduced the structure of our documents
and these project-specific requirements in an informal tutorial style.
This section specifies these requirements in a precise, succinct
form suitable for reference once you understand the basics.
The prose is generated from the same source code
as the Racket programs that enforce these requirements,
ensuring that the two remain in sync.

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
    (field text #:accessor tei-document-text-element #:hide)
    (match-define (list teiHeader text)
      body/elements-only)
    (declare-paragraphs-status-field
     (tei-document-paragraphs-status teiHeader))
    (define/field info
      (make-plain-instance-info
       #:title (tH-title teiHeader)
       #:resp-table (tH-resp-table teiHeader)
       #:citation (tH-citation teiHeader)
       #:orig-publication-date (tH-orig-publication-date teiHeader)
       #:publication-date (tH-publication-date teiHeader)
       #:publication-original? (tH-publication-original? teiHeader)
       #:language (text-lang text)
       #:book/article (tH-book/article teiHeader)))
    (define/field pr:md5
      (delay/thread
       (define-values (in-from-pipe out-to-pipe)
         (make-pipe))
       (thread
        (λ ()
          ;; Theoretically avoids storing the intermediate
          ;; bytes in memory.
          (write-xexpr/standardized (tei-element->xexpr (get-this))
                                    out-to-pipe)
          (close-output-port out-to-pipe)))
       (string->symbol (md5 in-from-pipe))))]
   #:property prop:instance-info (λ (this)
                                   (get-field info this))
   #:begin [(define (tei-document-checksum doc)
              (force (get-field pr:md5 doc)))]
   #:property prop:element->plain-text
   (λ (this)
     (element-or-xexpr->plain-text
      (get-field text this)))
   #:prose ƒ{

 The document should begin with a prelude,
 which must be exactly as follows:
 ƒ(nested #:style 'inset
          (verbatim
           ƒtt{<?xml version="1.0" encoding="utf-8"?>}))

 The root element is a ƒtag{TEI} element,
 which contains exactly (in order)
 ƒtag{teiHeader} and ƒtag{text} elements.
 It must have the attributes
 ƒtt{version="5.0"} and
 ƒtt{xmlns="http://www.tei-c.org/ns/1.0"}.

 })

ƒ(local-table-of-contents)
         
ƒinclude-section[(submod "teiHeader.rkt" doc)]
ƒinclude-section[(submod "text.rkt" doc)]

ƒbegin-for-runtime[
 (define (tei-document->plain-text doc
                                   #:include-header? [include-header? #t])
   (define body
     (element-or-xexpr->plain-text doc))
   (cond
     [include-header?
      (match-define (instance-info #:title title
                                   #:citation citation)
        doc)
      (string->immutable-string
       (string-append title
                      "\n\n"
                      citation
                      "\n\nDigital Ricœur: Not for distribution.\f"
                      body))]
     [else
      body])) 
 (define* (write-tei-document doc [out (current-output-port)])
   #:with [(match-define prelude
             (string->immutable-string
              ƒstring-append{
             <?xml version="1.0" encoding="utf-8"?>ƒ"\n"}))]        
   (parameterize ([current-output-port out])
     (call/prettyprint-xml-out
      (λ () 
        (write-string prelude)
        (write-xexpr/standardized (tei-element->xexpr doc))))))
 ]


