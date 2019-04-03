#lang ricoeur/tei/spec-lang

ƒtitle[#:version ""]{Formal Specification}
ƒ(require (for-label racket/base))
          

The rain in spain stays ƒbegin-for-runtime[12 #"43"]{
 on the plain.
}

ƒ(begin-for-runtime
   (define x 41))

ƒ(begin "12345"
        (begin-for-runtime
          (add1 x)))


