#lang racket/base

(require racket/runtime-path)

(provide DR-TEI_doc.html
         DR-TEI.dtd
         )

(define-runtime-path DR-TEI_doc.html
  "schema/DR-TEI_doc.html")

(define-runtime-path DR-TEI.dtd
  "schema/DR-TEI.dtd")
