#lang ricoeur/tei/kernel

ƒ[#:spec text-spec
  #:extends structural-elems-spec content-elems-spec
  #:with-local local-spec]

ƒtitle{The ƒtt{text} Element}

ƒbegin-for-runtime[
 (module+ private-to-base
   (require-provide (submod "content-elements.rkt" private-to-base))
   (provide tei-text-element? 
            text-lang))
 (require-provide (except-out (rename-in "structural-elements.rkt"
                                         [spec structural-elems-spec])
                              structural-elems-spec)
                  (except-out (rename-in "content-elements.rkt"
                                         [spec content-elems-spec])
                              content-elems-spec))
 (require (submod ricoeur/tei/kernel private)
          roman-numeral)
 ]

ƒ(require-self-for-label)
ƒ(require (for-label ricoeur/tei/kernel
                     (except-in racket
                                date?
                                date)))


ƒ(define-element text
   #:children ([0-1 front]
               [1 body]
               [0-1 back])
   #:required-order (front body back)
   #:attr-contracts
   ([xml:lang (or/c "en" "fr" "de")])
   #:required-attrs (xml:lang)
   #:predicate tei-text-element?
   #:constructor [
 #:attributes attrs
 (define/field #:infer lang
   (string->symbol
    (attributes-ref attrs 'xml:lang)))]
   #:prose ƒ{

  The ƒtag{text} element may contain only (in order) 
  a ƒtag{front} element, a ƒtag{body} element,
  and a ƒtag{back} element,
  but the ƒtag{front} and ƒtag{back} elements are optional.
  It must have a ƒattr{xml:lang} attribute specifying the
  primary language of the document: ƒracket["en"] for English;
  ƒracket["fr"] for French; or ƒracket["de"] for German.

  ƒdefine-elements-together[
 #:inset? #t
 ;; This is where I miss being able to say that
 ;; body+front+back have the same children
 ([body
   #:children ([0+ head]
               [0+ p]
               [0+ pb]
               [0+ ab]
               [0+ div])]
  [front
   #:children ([0+ head]
               [0+ p]
               [0+ pb]
               [0+ ab]
               [0+ div])]
  [back
   #:children ([0+ head]
               [0+ p]
               [0+ pb]
               [0+ ab]
               [0+ div])])]{
   The ƒtag{body}, ƒtag{front}, and ƒtag{back} elements
   may contain ƒtag{head}, 
   ƒtag{p}, ƒtag{pb}, ƒtag{ab}, and ƒtag{div} elements.
  }
  })




ƒinclude-section[(submod "structural-elements.rkt" doc)]
ƒinclude-section[(submod "content-elements.rkt" doc)]


