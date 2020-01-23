#lang racket/base

(require nanopass/base
         racket/match
         racket/string
         racket/list
         racket/bool
         racket/contract
         "support.rkt"
         "../types.rkt"
         (only-in ricoeur/kernel/utils
                  attributes-ref))

(provide (contract-out
          [parse-abstract
           (-> (xexpr-element/c 'abstract)
               parsed-abstract?)]
          [parse-phrasing-content
           (-> (cons/c strict-xexpr/c (listof strict-xexpr/c))
               parsed-phrasing-content?)]
          [parse-book-part-label
           (-> (or/c #f string?)
               (cons/c strict-xexpr/c (listof strict-xexpr/c))
               parsed-phrasing-content?)]
          ))

;; This module defines a Nanopass compiler
;; from the XML-based language of styled text
;; used in the JSTOR metadata files to:
;;  - plain text strings for searching; and
;;  - html xexprs for display.
;;
;; We only support the very limited subset of
;; the XML schema actually used in these documents.
;;
;; The public interface of this module hides the details
;; of the Nanopass implementation from clients.

(define (parse-phrasing-content forest)
  (compile ir->parsed-phrasing-content
           wrap-phrase
           phrase->ir
           forest))

(define (parse-book-part-label alt-text forest)
  (parse-phrasing-content
   (if alt-text
       `((xref ([alt ,alt-text]) ,@forest))
       forest)))
   
(define (parse-abstract src-elem)
  (compile ir->parsed-abstract
           wrap-preprocessed-abstract
           preprocess-abstract
           src-elem))

(module+ main
  (parse-abstract
   '(abstract ()
              (p ()
                 "hi"
                 (bold () (sup () "world") "!")
                 (xref ([alt "access"]) "      a" "b" (sub () "c"))
                 (uri ([xlink:href "www.artmatters.info"]
                       [xlink:type "simple"])
                      "www.artmatters.info")
                 (tex-math () "${I_{t + 1}} = a + b{I_t} + { \\in _{t + r}}$")
                 "z"))))

(define (compile unparse wrap ->ir arg)
  (unparse
   (resolve-jumbles
    (lift-results
     (accumulate-results
      (add-aria-labels
       (resolve-aria-hidden
        (check-no-nested-aria-hidden-text
         (splice-noop-elements
          (remove-attributes
           (normalize-ir
            (wrap
             (->ir arg)))))))))))))

(define preprocess-abstract
  (match-lambda
    [`(abstract ,_ . ,raw-body)
     (let*-values
         ([{body}
           (filter (match-lambda
                     [(list* (or 'label 'title 'p) _ _)
                      #t]
                     [(? whitespace-string?)
                      #f]
                     [child
                      (raise-arguments-error
                       'preprocess-abstract
                       "unsupported child x-expression"
                       "expected" (unquoted-printing-string
                                   "(or/c pair? whitespace-string?)")
                       "given" child)])
                   raw-body)]
          [{the-label body}
           (match body
             [(cons (and the-label (cons 'label _)) body)
              (values the-label body)]
             [_
              (values #f body)])]
          [{the-title body}
           (match body
             [(cons (and the-title (cons 'title _)) body)
              (values the-title body)]
             [_
              (values #f body)])])
       (preprocessed-abstract->ir
        `(preprocessed-abstract
          ,the-label
          ,the-title
          ,body)))]))

(define/final-prop attributes/c
  (listof (list/c symbol? string?)))

(define/subexpression-pos-prop (xexpr-element/c [name/c symbol?])
  (cons/c name/c (cons/c attributes/c (listof strict-xexpr/c))))

(define/final-prop strict-xexpr/c
  (or/c string?
        (recursive-contract (xexpr-element/c)
                            #:flat
                            #:list-contract?)))

;                                                      
;                                                      
;   ; ;;;   ;;    ; ;;;   ;;;   ; ;;    ;;     ;; ; ;; 
;   ;;  ;  ;  ;   ;;  ;  ;   ;  ;;  ;  ;  ;  ;;  ;;;  ;
;   ;;  ;;    ;;  ;;  ;; ;   ;  ;;  ;     ;;  ;    ;   
;   ;;  ;;  ;;;;  ;;  ;;;;   ;; ;;  ;;  ;;;;   ;;   ;; 
;   ;;  ;; ;  ;;  ;;  ;; ;   ;  ;;  ;  ;  ;;     ;;   ;
;   ;;  ;;;;  ;;  ;;  ;; ;   ;  ;;  ; ;;  ;; ;   ;;   ;
;   ;;  ;; ;;; ;  ;;  ;;  ;;;   ;;;;   ;;; ;  ;;;  ;;; 
;                               ;;                     
;                               ;;                     
;                               ;;                     
;                                                      


(define attributes?
  (match-lambda
    [(list (list (? symbol?) (? string?)) ...)
     #t]
    [_
     #f]))

(define (immutable-string? v)
  (and (string? v)
       (immutable? v)))

(define-language L-Src*
  ;; Allows any supported `start`:
  ;; see specialized variants at bottom.
  (terminals
   (attributes (attrs))
   (false (none))
   (string (str)))
  (Start
   [start]
   (preprocessed-abstract the-label the-title (para ...))
   (phrase emph ...))
  (Label
   [the-label]
   none
   (label attrs emph ...))
  (Title
   [the-title]
   none
   (title attrs emph ...))
  (Paragraph
   [para]
   (p attrs emph ...))
  (Emphasized-Text
   [emph]
   str
   (sc attrs emph ...)
   (xref attrs emph ...)
   (uri attrs emph ...)
   (tex-math attrs str ...)
   ;; simple:
   (bold attrs emph ...)
   (italic attrs emph ...)
   (sup attrs emph ...)
   (sub attrs emph ...)))

(define all-simple-element-names
  '(b i sup sub))
(define (simple-element-name? v)
  (memq v all-simple-element-names))

(define-language L0
  (extends L-Src*)
  (terminals
   (- (string (str)))
   (+ (immutable-string (str))
      (simple-element-name (name))))
  (Emphasized-Text
   [emph]
   (- str
      (tex-math attrs str ...) ;; FIXME: properly support this
      (bold attrs emph ...)
      (italic attrs emph ...)
      (sup attrs emph ...)
      (sub attrs emph ...))
   (+ str
      (simple-element name attrs emph ...))))

(define-pass normalize-ir
  : L-Src* (ir) -> L0 ()
  (Emphasized-Text
   : Emphasized-Text (ir) -> Emphasized-Text ()
   [,str
    `,(string->immutable-string
       (string-normalize-spaces str #:trim? #f))]
   [(tex-math ,attrs ,str* ...)
    ;; http://jats.nlm.nih.gov/archiving/tag-library/1.2/element/tex-math.html
    `,(string->immutable-string
       (string-append* str*))]
   [(bold ,attrs ,[emph] ...)
    `(simple-element ,'b ,attrs ,emph ...)]
   [(italic ,attrs ,[emph] ...)
    `(simple-element ,'i ,attrs ,emph ...)]
   [(sup ,attrs ,[emph] ...)
    `(simple-element ,'sup ,attrs ,emph ...)]
   [(sub ,attrs ,[emph] ...)
    `(simple-element ,'sub ,attrs ,emph ...)]))

(define-language L1
  (extends L0)
  (terminals
   (- (attributes (attrs))))
  (Start
   [start]
   (- (phrase emph ...))
   (+ (phrase emph-seq)))
  (Emphasized-Text-Seq
   [emph-seq]
   (+ (seq emph ...)))
  (Label
   [the-label]
   (- (label attrs emph ...))
   (+ (label emph-seq)))
  (Title
   [the-title]
   (- (title attrs emph ...))
   (+ (title emph-seq)))
  (Paragraph
   [para]
   (- (p attrs emph ...))
   (+ (p emph-seq)))
  (Emphasized-Text
   [emph]
   (- (simple-element name attrs emph ...)
      (sc attrs emph ...)
      (uri attrs emph ...)
      (xref attrs emph ...))
   (+ (simple-element name emph-seq)
      (sc emph-seq)
      (a (href-attr-pair) emph-seq)
      aria
      (noop emph-seq)))
  (Href-Attr-Pair
   [href-attr-pair]
   (+ (href str)))
  (Aria-Container
   [aria]
   (+ (aria-container (aria-label-attr-pair) emph-seq)))
  (Aria-Label-Attr-Pair
   [aria-label-attr-pair]
   (+ (aria-label str))))


(define-pass remove-attributes
  : L0 (ir) -> L1 ()
  (Start
   : Start (ir) -> Start ()
   [(phrase ,[emph] ...)
    `(phrase (seq ,emph ...))])
  ($check-alt-text
   : * (attrs) -> * (?alt)
   (apply-when string->immutable-string
               (attributes-ref attrs 'alt)))
  (Label
   : Label (ir) -> Label ()
   [(label ,attrs ,[emph] ...)
    (let ([?alt ($check-alt-text attrs)])
      (if ?alt
          `(label (seq (aria-container ((aria-label ,?alt)) (seq ,emph ...))))
          `(label (seq ,emph ...))))])
  (Title
   : Title (ir) -> Title ()
   [(title ,attrs ,[emph] ...)
    `(title (seq ,emph ...))])
  (Paragraph
   : Paragraph (ir) -> Paragraph ()
   [(p ,attrs ,[emph] ...)
    `(p (seq ,emph ...))])
  (Emphasized-Text
   : Emphasized-Text (ir) -> Emphasized-Text ()
   [(simple-element ,name ,attrs ,[emph] ...)
    `(simple-element ,name (seq ,emph ...))]
   [(sc ,attrs ,[emph] ...)
    `(sc (seq ,emph ...))]
   [(xref ,attrs ,[emph] ...)
    (let ([?alt ($check-alt-text attrs)])
      (if ?alt
          `(aria-container ((aria-label ,?alt)) (seq ,emph ...))
          `(noop (seq ,emph ...))))]
   [(uri ,attrs ,[emph] ...)
    (let ([href-str (prepare-href-for-html
                     (attributes-ref attrs 'xlink:href))])
      (if href-str
          `(a ([href ,href-str]) (seq ,emph ...))
          `(noop (seq ,emph ...))))]))


(define-language L2
  (extends L1)
  (Emphasized-Text
   [emph]
   (- (noop emph-seq))))


(define-pass splice-noop-elements
  : L1 (ir) -> L2 ()
  (Emphasized-Text-Seq
   : Emphasized-Text-Seq (ir) -> Emphasized-Text-Seq ()
   [(seq ,emph ...)
    `(seq ,(append-map Emphasized-Text emph) ...)])
  (Emphasized-Text
   : Emphasized-Text (ir) -> * (lst)
   [(noop ,[emph-seq])
    (nanopass-case
     (L2 Emphasized-Text-Seq)
     emph-seq
     [(seq ,emph ...)
      emph])]
   [(aria-container (,[aria-label-attr-pair]) ,[emph-seq])
    (list (in-context Emphasized-Text
                      `(aria-container (,aria-label-attr-pair) ,emph-seq)))]
   [,str
    (list (in-context Emphasized-Text
                      `,str))]
   [(simple-element ,name ,[emph-seq])
    (list (in-context Emphasized-Text
                      `(simple-element ,name ,emph-seq)))]
   [(a (,[href-attr-pair]) ,[emph-seq])
    (list (in-context Emphasized-Text
                      `(a (,href-attr-pair) ,emph-seq)))]
   [(sc ,[emph-seq])
    (list (in-context Emphasized-Text
                      `(sc ,emph-seq)))]))


(define-language L3
  (extends L2)
  (Aria-Container
   [aria]
   (- (aria-container (aria-label-attr-pair) emph-seq))
   (+ (aria-container (aria-label-attr-pair) aht-seq)))
  (Aria-Hidden-Text-Seq
   [aht-seq]
   (+ (seq aht ...)))
  (Aria-Hidden-Text
   [aht]
   ;; You can't undo aria-hidden, so forbid nested containers.
   ;; It also seems quite bad to put a link inside an aria-hidden span.
   (+ str
      (simple-element name aht-seq)
      (sc aht-seq))))

(define-pass check-no-nested-aria-hidden-text
  : L2 (ir) -> L3 ()
  (Aria-Container
   : Aria-Container (ir) -> Aria-Container ()
   [(aria-container (,[aria-label-attr-pair]) ,[$convert-seq : -> aht-seq])
    `(aria-container (,aria-label-attr-pair) ,aht-seq)])
  ($convert-seq
   : Emphasized-Text-Seq (ir) -> Aria-Hidden-Text-Seq ()
   [(seq ,[$emph->aht : -> aht] ...)
    `(seq ,aht ...)])
  ($emph->aht
   : Emphasized-Text (ir) -> Aria-Hidden-Text ()
   [(aria-container ((aria-label ,str)) ,emph-seq)
    (raise-arguments-error 'check-no-nested-aria-hidden-text
                           "found illegally nested aria-container"
                           "label" str
                           "body" emph-seq)]
   [(a ((href ,str)) ,emph-seq)
    (raise-arguments-error 'check-no-nested-aria-hidden-text
                           "found illegally nested link"
                           "href" str
                           "body" emph-seq)]
   [(simple-element ,name ,[$convert-seq : -> aht-seq])
    `(simple-element ,name ,aht-seq)]
   [(sc ,[$convert-seq : -> aht-seq])
    `(sc maht-seq)]))

(define smallcaps-attrs
  '([font-variant "small-caps"]))

(define (smallcaps-attrs? v)
  (equal? smallcaps-attrs v))

(define-language L4
  (extends L3)
  (terminals
   (+ (smallcaps-attrs (smallcaps))))
  (Results
   [rslt]
   (+ (results j (ah-html ...))))
  (Jumble
   [j]
   (+ str
      [=> (jumble j ...)
          (j ...)]))
  (AH-Html
   [ah-html]
   (+ str
      (span smallcaps ah-html ...)
      [=> (simple-element name ah-html ...)
          (name () ah-html ...)]))
  (Aria-Container
   [aria]
   (- (aria-container (aria-label-attr-pair) aht-seq))
   (+ (aria-container (aria-label-attr-pair) rslt)))
  (Aria-Hidden-Text-Seq
   [aht-seq]
   (- (seq aht ...)))
  (Aria-Hidden-Text
   [aht]
   (- str
      (simple-element name aht-seq)
      (sc aht-seq))))

(define-pass resolve-aria-hidden
  : L3 (ir) -> L4 ()
  {definitions
   (define ($unpack-results ir)
     (nanopass-case
      (L4 Results)
      ir
      [(results ,j (,ah-html ...))
       (values j ah-html)]))}
  (Aria-Hidden-Text-Seq
   : Aria-Hidden-Text-Seq (ir) -> Results ()
   [(seq ,[Aria-Hidden-Text : -> rslt*] ...)
    (for/lists [jumbles
                ah-html* 
                #:result `(results (jumble ,jumbles ...)
                                   (,(append* ah-html*) ...))]
               ([ir (in-list rslt*)])
      ($unpack-results ir))])
  ($unpack-aht-seq
   : Aria-Hidden-Text-Seq (ir) -> * (j ah-html*)
   [else
    ($unpack-results (Aria-Hidden-Text-Seq ir))])
  (Aria-Hidden-Text
   : Aria-Hidden-Text (ir) -> Results ()
   [,str
    `(results ,str (,str))]
   [(simple-element ,name ,[$unpack-aht-seq : aht-seq -> j ah-html*])
    `(results ,j ((simple-element ,name ,ah-html* ...)))]
   [(sc ,[$unpack-aht-seq : aht-seq -> j ah-html*])
    `(results ,j ((span ,smallcaps-attrs ,ah-html* ...)))])
  (Aria-Container
   : Aria-Container (ir) -> Aria-Container ()
   [(aria-container (,[aria-label-attr-pair]) ,[Aria-Hidden-Text-Seq : aht-seq -> rslt])
    `(aria-container (,aria-label-attr-pair) ,rslt)]))


(define aria-hidden-true-attrs
  '([aria-hidden "true"]))

(define (aria-hidden-true-attrs? v)
  (equal? aria-hidden-true-attrs v))

(define-language L5
  (extends L4)
  (terminals
   (+ (aria-hidden-true-attrs (aria-hidden-true))))
  (Results
   [rslt]
   (- (results j (ah-html ...))))
  (Aria-Hidden-Span
   [aria-hidden-span]
   (+ (span aria-hidden-true ah-html ...)))
  (Aria-Labeled-Span
   [aria-labeled-span]
   (+ (span (aria-label-attr-pair) aria-hidden-span)))
  (Aria-Container
   [aria]
   (- (aria-container (aria-label-attr-pair) rslt))
   (+ (aria-container j aria-labeled-span))))

(define-pass add-aria-labels
  : L4 (ir) -> L5 ()
  (Aria-Container
   : Aria-Container (ir) -> Aria-Container ()
   [(aria-container (,[aria-label-attr-pair]) (results ,[j] (,[ah-html*] ...)))
    `(aria-container ,j (span (,aria-label-attr-pair)
                              (span ,aria-hidden-true-attrs ,ah-html* ...)))]))

(define-language L6
  (extends L5)
  (Phrase-Html
   [html]
   (+ str
      aria-labeled-span
      (span smallcaps html ...)
      (a (href-attr-pair) html ...)
      [=> (simple-element name html ...)
          (name () html ...)]))
  (Html-Paragraph
   [html-p]
   (+ (p () html ...)))
  (Phrase-Html-Forest
   [phrase-html-forest]
   (+ [=> (phrase-html html ...)
          (html ...)]))
  (Emphasized-Text-Seq
   [emph-seq]
   (- (seq emph ...)))
  (Tmp-Result
   [tmp-rslt]
   (+ (tmp-result j (html ...))))
  (Emphasized-Text
   [emph]
   (- str
      aria
      (sc emph-seq)
      (a (href-attr-pair) emph-seq)
      (simple-element name emph-seq)))
  (Aria-Container
   [aria]
   (- (aria-container j aria-labeled-span)))
  (Start
   [start]
   (- (phrase emph-seq))
   (+ (phrase j phrase-html-forest)))
  (Label
   [the-label]
   (- (label emph-seq))
   (+ (label j phrase-html-forest)))
  (Title
   [the-title]
   (- (title emph-seq))
   (+ (title j phrase-html-forest)))
  (Paragraph
   [para]
   (- (p emph-seq))
   (+ (p j html-p))))


(define-pass accumulate-results
  : L5 (ir) -> L6 ()
  {definitions
   (define ($unpack-tmp-result ir)
     (nanopass-case
      (L6 Tmp-Result)
      ir
      [(tmp-result ,j (,html* ...))
       (values j html*)]))}
  (Emphasized-Text-Seq
   : Emphasized-Text-Seq (ir) -> Tmp-Result ()
   [(seq ,[Emphasized-Text : -> tmp-rslt*] ...)
    (for/lists [jumbles
                html* 
                #:result `(tmp-result (jumble ,jumbles ...)
                                      (,(append* html*) ...))]
               ([ir (in-list tmp-rslt*)])
      ($unpack-tmp-result ir))])
  ($unpack-children
   : Emphasized-Text-Seq (ir) -> * (j html*)
   [else
    ($unpack-tmp-result (Emphasized-Text-Seq ir))])
  (Emphasized-Text
   : Emphasized-Text (ir) -> Tmp-Result ()
   [,str
    `(tmp-result ,str (,str))]
   [(aria-container ,[j] ,[aria-labeled-span])
    `(tmp-result ,j (,aria-labeled-span))]
   [(sc ,[$unpack-children : emph-seq -> j html*])
    `(tmp-result ,j ((span ,smallcaps-attrs ,html* ...)))]
   [(a (,[href-attr-pair]) ,[$unpack-children : emph-seq -> j html*])
    `(tmp-result ,j ((a (,href-attr-pair) ,html* ...)))]
   [(simple-element ,name ,[$unpack-children : emph-seq -> j html*])
    `(tmp-result ,j ((simple-element ,name ,html* ...)))])
  (Paragraph
   : Paragraph (ir) -> Paragraph ()
   [(p ,[$unpack-children : emph-seq -> j html*])
    `(p ,j (p () ,html* ...))])
  (Label
   : Label (ir) -> Label ()
   [(label ,[$unpack-children : emph-seq -> j html*])
    `(label ,j (phrase-html ,html* ...))])
  (Title
   : Title (ir) -> Title ()
   [(title ,[$unpack-children : emph-seq -> j html*])
    `(title ,j (phrase-html ,html* ...))])
  (Start
   : Start (ir) -> Start ()
   [(phrase ,[$unpack-children : emph-seq -> j html*])
    `(phrase ,j (phrase-html ,html* ...))]))


(define-language L7
  (extends L6)
  (Tmp-Result
   [tmp-rslt]
   (- (tmp-result j (html ...))))
  (Start
   [start]
   (- (preprocessed-abstract the-label the-title (para ...)))
   (+ (abstract j the-label the-title abstract-body)))
  (Label
   [the-label]
   (- (label j phrase-html-forest))
   (+ phrase-html-forest))
  (Title
   [the-title]
   (- (title j phrase-html-forest))
   (+ phrase-html-forest))
  (Abstract-Body
   [abstract-body]
   (+ [=> (body html-p ...) (html-p ...)]))
  (Paragraph
   [para]
   (- (p j html-p))))

(define-pass lift-results
  : L6 (ir) -> L7 ()
  {definitions
   (define empty-jumble
     (with-output-language [L7 Jumble] `(jumble)))
   (define (jumble-space-right j)
     (with-output-language [L7 Jumble] `(jumble ,j " ")))}
  (Start
   : Start (ir) -> Start ()
   [(preprocessed-abstract ,[Label : -> the-label? j0]
                           ,[Title : -> the-title? j1]
                           ((p ,[j*] ,[html-p*]) ...))
    `(abstract (jumble ,j0 ,j1 ,j* ...)
               ,the-label?
               ,the-title?
               (body ,html-p* ...))])
  (Label
   : Label (ir) -> Label (j)
   [(label ,[j] ,[phrase-html-forest])
    (values `,phrase-html-forest (jumble-space-right j))]
   [,none
    (values none empty-jumble)])
  (Title
   : Title (ir) -> Title (j)
   [(title ,[j] ,[phrase-html-forest])
    (values `,phrase-html-forest (jumble-space-right j))]
   [,none
    (values none empty-jumble)]))
  


(define-language L8
  (extends L7)
  (Jumble
   [j]
   (- (jumble j ...))))

(define (jumble->list j)
  (let jumble->list ([j j]
                     [tail null])
    (match j
      [(cons fst rst)
       (jumble->list fst (jumble->list rst tail))]
      ['()
       tail]
      [str
       (cons str tail)])))

(define-pass resolve-jumbles
  : L7 (ir) -> L8 ()
  (Jumble
   : Jumble (ir) -> Jumble ()
   [else
    `,(string->immutable-string
       (string-normalize-spaces
        (string-append*
         (jumble->list (unparse-L7 ir)))))]))

;                                              
;                                              
;   ;; ;;  ; ;;;  ; ;;    ;;    ;; ;  ;; ; ;;  
;   ;; ;;  ;;  ;  ;;  ;  ;  ;   ;;; ;;  ; ;  ; 
;   ;; ;;  ;;  ;; ;;  ;     ;;  ;;   ;    ;  ; 
;   ;; ;;  ;;  ;; ;;  ;;  ;;;;  ;;    ;; ;;;;;;
;   ;; ;;  ;;  ;; ;;  ;  ;  ;;  ;;      ;;;    
;    ; ;;  ;;  ;; ;;  ; ;;  ;;  ;;  ;   ; ;    
;    ;;;;  ;;  ;; ;;;;   ;;; ;  ;;   ;;;   ;;; 
;                 ;;                           
;                 ;;                           
;                 ;;                           
;                                              

(define (ir->parsed-abstract ir)
  (nanopass-case
   (L8 Start)
   ir
   [(abstract ,str ,the-label ,the-title ,abstract-body)
    (make-parsed-abstract
     #:normalized-string str
     #:label (unparse-L8 the-label)
     #:title (unparse-L8 the-title)
     #:body (unparse-L8 abstract-body))]))


(define (ir->parsed-phrasing-content ir)
  (nanopass-case
   (L8 Start)
   ir
   [(phrase ,str ,phrase-html-forest)
    (make-parsed-phrasing-content str
                                  (unparse-L8 phrase-html-forest))]))

;                                                                  
;                                                                  
;                ;;                              ;;        ;;      
;                ;;                                        ;;      
;    ;;   ; ;;; ;;;;;;; ;;    ;    ; ;;    ;;;   ;; ; ;;; ;;;;; ;; 
;   ;  ;  ;;  ;  ;;  ;;;  ;  ;     ;;  ;  ;   ;  ;; ;;  ;  ;; ;;  ;
;   ;  ;  ;;  ;; ;;  ;;   ;  ;     ;;  ;  ;   ;  ;; ;;  ;; ;;  ;   
;  ;;;;;; ;;  ;; ;;  ;;   ;  ;     ;;  ;;;;   ;; ;; ;;  ;; ;;   ;; 
;   ;     ;;  ;; ;;  ;;    ; ;     ;;  ;  ;   ;  ;; ;;  ;; ;;     ;
;   ;     ;;  ;;  ;  ;;    ;;      ;;  ;  ;   ;  ;; ;;  ;;  ; ;   ;
;    ;;;  ;;  ;;  ;;;;;     ;      ;;;;    ;;;   ;; ;;  ;;  ;;;;;; 
;                           ;      ;;                              
;                          ;       ;;                              
;                        ;;        ;;                              
;                                                                  

(define-language L-Preprocessed-Abstract
  (extends L-Src*)
  (Start
   [start]
   (- (phrase emph ...))))

(define-pass wrap-preprocessed-abstract
  : L-Preprocessed-Abstract (ir) -> L-Src* ())

(define-parser preprocessed-abstract->ir L-Preprocessed-Abstract)

(define-language L-Phrase
  (extends L-Src*)
  (Start
   [start]
   (- (preprocessed-abstract the-label the-title (para ...))
      (phrase emph ...))
   (+ (emph ...))))

(define-pass wrap-phrase
  : L-Phrase (ir) -> L-Src* ()
  (Start
   : Start (ir) -> Start ()
   [(,[emph] ...)
    `(phrase ,emph ...)]))

(define-parser phrase->ir L-Phrase)

