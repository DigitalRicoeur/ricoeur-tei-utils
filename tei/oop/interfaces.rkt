#lang racket

(require adjutor
         )

(require-provide "interfaces/element.rkt"
                 "interfaces/elements-only.rkt"
                 "interfaces/info-interfaces.rkt"
                 "interfaces/pb.rkt"
                 "interfaces/guess-paragraphs.rkt"
                 "interfaces/specific.rkt"
                 )

(provide teiHeader<%> 
         TEI<%>
         )                       

(define teiHeader<%>
  (interface (TEI-info<%> element<%>)))

(define TEI<%>
  (interface (element<%>
              TEI-info<%>
              get-page-breaks<%>
              guess-paragraphs<%>
              elements-only<%>)
    [get-md5 (->m string?)]
    [get-teiHeader (->m (is-a?/c teiHeader<%>))]
    [write-TEI (->*m {} {output-port?} any)]
    ))






