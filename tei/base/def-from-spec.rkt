#lang racket/base

(require ricoeur/tei/kernel
         (submod ricoeur/tei/kernel private)
         racket/contract)

(require-provide (except-out "specification/specification.rkt"
                             main-spec))

(provide tei-xexpr/c
         dynamic-tei-xexpr/c
         any-tei-xexpr/c
         tei-element-name/c
         (contract-out
          [file->tei-document
           (-> (and/c path-string-immutable/c
                      file-exists?)
               tei-document?)] 
          [read-tei-document
           (->* {} {input-port?} tei-document?)] 
          [xexpr->tei-element
           (-> any-tei-xexpr/c tei-element?)]
          ))

(module+ private-to-base
  (require-provide (submod "specification/specification.rkt"
                           private-to-base)))


(define-values/elements-specifications [main-spec
                                        ]
  #:tei-xexpr/c tei-xexpr/c
  #:dynamic-tei-xexpr/c dynamic-tei-xexpr/c
  #:any-tei-xexpr/c any-tei-xexpr/c
  #:xexpr->tei-element xexpr->tei-element #:define/contract
  #:tei-element-name/c tei-element-name/c)


(define (read-tei-document [in (current-input-port)])
  (TODO/void read-tei-document: improve error message)
  (xexpr->tei-element
   (contract (tei-xexpr/c TEI)
             (read-xexpr/standardized in)
             in ;; positive party
             '(definition read-tei-document) ;; negative party
             (or (object-name in) in) ;; value name
             #f)))


(define (file->tei-document pth-str)
  (call-with-input-file* pth-str
    #:mode 'text
    read-tei-document))


(module+ test
  (provide example)
  (define example
    (file->tei-document "/Users/philip/code/ricoeur/texts/TEI/ways_of_Worldmaking.xml"))
  (tei-document-checksum example)
  example)

