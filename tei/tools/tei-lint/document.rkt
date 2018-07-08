#lang racket/gui

(require framework
         ricoeur/tei/base
         "lib.rkt"
         "interfaces.rkt"
         "document/make-class.rkt"
         "document/warning-components.rkt"
         "document/info-components.rkt"
         )

(provide (contract-out
          [tei-document-proto-frame%
           tei-document-proto-frame%/c]
          ))

(define tei-document-proto-frame%
  (make-tei-document-proto-frame-class
   (document-frame-component-append
    document-frame-component:heading
    document-frame-component:citation
    document-frame-component:book/article
    document-frame-component:date
    document-frame-component:paragraphs
    document-frame-component:pages)))

