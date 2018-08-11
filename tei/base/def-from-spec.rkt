#lang racket/base

(require ricoeur/tei/kernel
         (submod ricoeur/tei/kernel private)
         racket/contract
         (only-in xml
                  collapse-whitespace
                  read-comments
                  xml-count-bytes
                  xexpr-drop-empty-attributes
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
         dynamic-tei-xexpr/c
         any-tei-xexpr/c
         tei-element-name/c
         (except-out (combine-out
                      (all-from-out "specification/text.rkt")
                      (all-from-out "specification/teiHeader.rkt")
                      (except-out (all-from-out "specification/specification.rkt")
                                  main-spec))
                     profileDesc?
                     tei-ab?
                     tei-document-text-element
                     tei-keywords?
                     tei-text-element?
                     teiHeader?
                     textClass?)
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

(module+ private
  (provide profileDesc?
           tei-ab?
           tei-document-text-element
           tei-keywords?
           tei-text-element?
           teiHeader?
           textClass?))

(define-values/elements-specifications [main-spec
                                        ]
  #:tei-xexpr/c tei-xexpr/c
  #:dynamic-tei-xexpr/c dynamic-tei-xexpr/c
  #:any-tei-xexpr/c any-tei-xexpr/c
  #:xexpr->tei-element xexpr->tei-element #:define/contract
  #:tei-element-name/c tei-element-name/c)


(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))


(define (read-tei-document [in (current-input-port)])
  (discard-bom in)
  ;TODO: improve error message
  (xexpr->tei-element
   (contract (tei-xexpr/c TEI)
             (parameterize ([collapse-whitespace #f]
                            [read-comments #t]
                            [xml-count-bytes #f]
                            [xexpr-drop-empty-attributes #f])
               (xml->xexpr
                (document-element
                 (read-xml in))))
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

