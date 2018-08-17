#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         )

(provide token-sym->style
         tokenize-xml
         )

(define-lex-abbrevs
  [quoted-string
   (:seq #\"
         (:* (union (char-complement (union #\\ #\"))
                    (:seq #\\ #\\)
                    (:seq #\\ #\")))
         #\")]
  [tag
   (:seq #\<
         (:+ (union (char-complement (union #\> #\"))
                    quoted-string))
         #\>)]
  [entity (:seq #\& (:+ (char-complement #\;)) #\;)]
  [text (:+ (char-complement (union #\< #\&)))]
  )

(define tokenize-xml
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]
   [tag (values lexeme
                'tag
                #f
                (position-offset start-pos)
                (position-offset end-pos))]
   [entity (values lexeme
                   'entity
                   #f
                   (position-offset start-pos)
                   (position-offset end-pos))]
   [text (values lexeme
                 'text
                 #f
                 (position-offset start-pos)
                 (position-offset end-pos))]
   ))

(define (token-sym->style sym)
  (case sym
    [(text) "Standard"]
    [(tag) "ricoeur:xml-tag"]
    [(entity) "ricoeur:xml-entity"]))



