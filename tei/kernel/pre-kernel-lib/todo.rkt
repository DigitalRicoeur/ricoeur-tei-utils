#lang racket/base

(require (for-syntax syntax/parse
                     racket/base
                     racket/string
                     ))

(provide TODO
         TODO/void
         TODO/scrbl
         )

(begin-for-syntax
  (struct todo-item (full summary) #:prefab)
  (struct located (loc value) #:prefab)
  (define-syntax-class message-part
    #:description #f
    #:attributes {datum}
    (pattern v:id
             #:attr datum
             (symbol->string (syntax->datum #'v)))
    (pattern v:string
             #:attr datum (syntax->datum #'v))
    (pattern v:number
             #:attr datum (number->string (syntax->datum #'v))))
  (define-splicing-syntax-class message
    #:attributes {summary full}
    (pattern (~seq msg:message-part ...+)
             #:attr summary (string-join (attribute msg.datum) " ")
             #:attr full (attribute summary))
    (pattern (~seq msg:message-part ...+ #: more:message-part ...+)
             #:attr summary (string-join (attribute msg.datum) " ")
             #:attr full (string-append (attribute summary)
                                        "\n\n"
                                        (string-join (attribute more.datum)
                                                     " "))))
  (define-splicing-syntax-class runtime-expr
    #:attributes {runtime}
    #:description #f
    (pattern (~seq #:expr runtime:expr)))
  (define (make-todo-syntax stx full summary)
    (syntax-property
     stx
     'todo (todo-item full summary)))
  #|END begin-for-syntax|#)

(define-syntax TODO
  (syntax-parser
    [(_ msg:message)
     (make-todo-syntax
      (quasisyntax/loc this-syntax
        (error #,(datum->syntax this-syntax (attribute msg.summary))))
      (attribute msg.full)
      (attribute msg.summary))]
    [(_ (~alt (~once runtime:runtime-expr)
              (~once msg:message))
        ...)
     (make-todo-syntax
      (quasisyntax/loc this-syntax
        runtime.runtime)
      (attribute msg.full)
      (attribute msg.summary))]))

(define-syntax TODO/void
  ;; expanding to TODO seems to cause some source-location issue ... :(
  (syntax-parser
    [(_ msg:message)
     (make-todo-syntax
      (quasisyntax/loc this-syntax
        (void))
      (attribute msg.full)
      (attribute msg.summary))]))

(define-syntax TODO/scrbl
  (syntax-parser
    [(_ [msg:message])
     (make-todo-syntax
       (quasisyntax/loc this-syntax
         (void))
       (attribute msg.full)
       (attribute msg.summary))]
    [(_ [msg:message] body:expr ...+)
      (make-todo-syntax
       (quasisyntax/loc this-syntax
         (begin body ...))
       (attribute msg.full)
       (attribute msg.summary))]))


