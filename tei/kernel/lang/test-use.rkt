#lang racket

(require ricoeur/tei/kernel/lang/test3
         ricoeur/tei/kernel/lang/test4
         ricoeur/tei/kernel/lang/test-duplicate
         ricoeur/tei/kernel/lang/define
         (submod ricoeur/tei/kernel/lang/define
                 private)
         )

(provide tei-xexpr/c
         static-tei-xexpr/c
         any-tei-xexpr/c
         tei-element-name/c
         )

#;
(show-elements-specification-transformer
 custom-spec)


(define-values/elements-specifications [custom-spec
                                        spec
                                        ;duplicate-spec
                                        ]
  #:tei-xexpr/c tei-xexpr/c
  #:static-tei-xexpr/c static-tei-xexpr/c
  #:any-tei-xexpr/c any-tei-xexpr/c
  #:tei-element-name/c tei-element-name/c)
  
