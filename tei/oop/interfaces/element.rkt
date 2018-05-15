#lang racket

(require ricoeur/tei/oop-kernel
         (only-in ricoeur/tei/normalize-placeholder
                  non-element-xexpr->plain-text)
         data/maybe
         gregor
         adjutor
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     ))

(provide element-or-xexpr/c
         element<%>
         (contract-out
          [element%
           (class/c (init [name symbol?]
                          [attributes (listof (list/c symbol? string?))]
                          [body (listof element-or-xexpr/c)]))]
          [tei-element?
           (-> any/c any/c)]
          [element-or-xexpr->plain-text
           (-> element-or-xexpr/c string?)]
          ))

(define-values {element% element-or-xexpr/c}
  (let ([element<%> (interface ())])
    ;; This is a little complicated to get the contract
    ;; to have a private version of element<%> that is fully defined.
    ;; We also give element% a private lexically-scoped method.
    (define element-or-xexpr/c
      (or/c (is-a?/c element<%>)
            string?
            comment?
            p-i?))
    (define-member-name private-method (generate-member-key))
    (define element%
      (class* object% ((interface (element<%>)
                         [to-xexpr (->m any-tei-xexpr/c)]
                         [to-plain-text (->m string?)]
                         [get-name (->m symbol?)]
                         [get-attributes (->m (listof (list/c symbol? string?)))]
                         [get-body (->m (listof element-or-xexpr/c))]
                         private-method
                         ))
        (super-new)
        (init [(:name name)]
              [(:attributes attributes) null]
              [(:body body) null])
        (def
          [name :name]
          [attributes :attributes]
          [body :body])
        (define/public (to-xexpr)
          (list* name attributes (for/list ([child (in-list body)])
                                   (if (tei-element? child)
                                       (send child to-xexpr)
                                       child))))
        (define/public (to-plain-text)
          (string-join (map element-or-xexpr->plain-text
                            body)
                       ""))
        (public-final*
         [private-method (λ () (void))]
         [get-name (λ () name)]
         [get-attributes (λ () attributes)]
         [get-body (λ () body)])
        #|END element%|#))
    (values element% element-or-xexpr/c)))


(define (element-or-xexpr->plain-text child)
  (if (tei-element? child)
      (send child to-plain-text)
      (non-element-xexpr->plain-text child)))


(define element<%>
  (class->interface element%))


(define tei-element?
  (is-a?/c element<%>))




