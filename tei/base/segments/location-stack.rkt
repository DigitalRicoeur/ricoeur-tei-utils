#lang racket/base

(require data/maybe
         racket/contract
         racket/match
         racket/serialize
         ricoeur/tei/kernel
         ricoeur/tei/base/def-from-spec
         )

(provide location-stack?
         (contract-out
          [location-stack->strings
           (-> location-stack?
               (listof string-immutable/c))]
          ))

(module+ private
  (provide (contract-out
            [struct location-stack-entry:root
              ([sym (or/c 'front 'back 'body)])]
            [struct location-stack-entry:div
              ([type div-type/c]
               [n (maybe/c string?)]
               [rest (or/c location-stack-entry:div?
                           location-stack-entry:root?)])]
            [struct location-stack-entry:note
              ([place (or/c 'foot 'end)]
               [n string?]
               [transl? (or/c #f 'transl)]
               [rest (or/c location-stack-entry:root? ;; <note> tags might be added before <div> tags
                           location-stack-entry:note? 
                           location-stack-entry:div?)])]
            )))

(serializable-struct location-stack-entry:root (sym)
  ;; to provide a deserialization hook for future changes
  #:transparent)

(serializable-struct location-stack-entry:div (type n rest)
  #:transparent)

(serializable-struct location-stack-entry:note (place n transl? rest)
  #:transparent)
                    
(define/final-prop location-stack?
  (or/c location-stack-entry:root?
        location-stack-entry:div?
        location-stack-entry:note?))

(define (location-stack->strings stk)
  (reverse
   (let to-strings ([stk stk])
     (match stk
       [(location-stack-entry:root 'body)
        null]
       [(location-stack-entry:root 'front)
        '("Front-matter")]
       [(location-stack-entry:root 'back)
        '("Back-matter")]
       [(location-stack-entry:div type n rest)
        (cons (string->immutable-string
               (string-append (case type
                                [(chapter) "Chapter"]
                                [(part) "Part"]
                                [(section) "Section"]
                                [(dedication) "Dedication"]
                                [(contents) "Table of Contents"]
                                [(intro) "Introduction"]
                                [(bibl) "Bibliography"]
                                [(ack) "Acknowledgements"]
                                [(index) "Index"]
                                [else (error 'location-stack->string
                                             (format "unknown div type ~e"
                                                     type))])
                              (maybe ""
                                     (λ (n) (string-append " " n))
                                     n)))
              (to-strings rest))]
       [(location-stack-entry:note place n transl? rest)
        (cons (string->immutable-string
               (string-append (if transl?
                                  "Translator's "
                                  "")
                              (case place
                                [(foot) "Footnote "]
                                [(end) "Endnote "]
                                [else (error 'location-stack->string
                                             (format "unknown note place ~e"
                                                     place))])
                              n))
              (to-strings rest))]))))



