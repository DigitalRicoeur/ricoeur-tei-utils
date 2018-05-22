#lang racket/base

(require ricoeur/tei/kernel
         (submod ricoeur/tei/kernel private)
         racket/contract
         (only-in xml
                  xml->xexpr
                  document-element
                  read-xml)
         "specification/specification.rkt"
         )

(provide tei-xexpr/c
         static-tei-xexpr/c
         any-tei-xexpr/c
         tei-element-name/c
         TEI?
         (contract-out
          [file->TEI
           (-> (and/c path-string? file-exists?)
               TEI?)] 
          [read-TEI
           (->* {} {input-port?} TEI?)] 
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


(define (read-TEI [in (current-input-port)])
  (discard-bom in)
  ;TODO: improve error message
  (xexpr->element
   (contract (static-tei-xexpr/c TEI)
             (xml->xexpr
              (document-element
               (read-xml in)))
             in
             '(definition read-TEI)
             (or (object-name in) in)
             #f)))


(define (file->TEI pth-str)
  (call-with-input-file* pth-str
    #:mode 'text
    read-TEI))


(module+ test
  (file->TEI "/Users/philip/code/ricoeur/texts/TEI/ways_of_Worldmaking.xml"))

