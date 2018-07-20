#lang racket/base

(require ricoeur/tei/kernel
         (submod ricoeur/tei/kernel private)
         racket/contract
         (only-in xml
                  xml->xexpr
                  document-element
                  read-xml)
         "specification/specification.rkt"
         (except-in "specification/text.rkt"
                    text-spec)
         (except-in "specification/teiHeader.rkt"
                    teiHeader-spec)
         )

(provide tei-xexpr/c
         static-tei-xexpr/c
         any-tei-xexpr/c
         tei-element-name/c
         (all-from-out "specification/text.rkt")
         (all-from-out "specification/teiHeader.rkt")
         (except-out (all-from-out "specification/specification.rkt")
                     main-spec)
         (contract-out
          [file->tei-document
           (-> (and/c path-string? file-exists?)
               tei-document?)] 
          [read-tei-document
           (->* {} {input-port?} tei-document?)] 
          [xexpr->element
           (-> any-tei-xexpr/c tei-element?)]
          ))

(define-values/elements-specifications [main-spec
                                        ]
  #:tei-xexpr/c tei-xexpr/c
  #:static-tei-xexpr/c static-tei-xexpr/c
  #:any-tei-xexpr/c any-tei-xexpr/c
  #:xexpr->element xexpr->element #:define/contract
  #:tei-element-name/c tei-element-name/c)


(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))


(define (read-tei-document [in (current-input-port)])
  (discard-bom in)
  ;TODO: improve error message
  (xexpr->element
   (contract (static-tei-xexpr/c TEI)
             (xml->xexpr
              (document-element
               (read-xml in)))
             in
             '(definition read-tei-document)
             (or (object-name in) in)
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

