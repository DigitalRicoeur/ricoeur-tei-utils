#lang ricoeur/tei

ƒtitle[#:version ""]{Formal Specification}
ƒ(require (for-label racket/base
                     ))
          

The rain in spain stays ƒbegin-for-spec[12 #"43"]{
 on the plain.
}

ƒ(begin-for-spec
   (define x 41))

ƒ(begin "12345"
        (begin-for-spec
          (add1 x)))


