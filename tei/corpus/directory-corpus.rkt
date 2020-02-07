#lang racket/base

(require racket/contract
         racket/class
         ricoeur/tei/base
         (only-in adjutor/unstable TODO/void)
         "plain-corpus.rkt")
         
(provide (contract-out 
          [directory-corpus%
           (class/c
            (init [path (and/c path-string-immutable/c ;; path-string-immutable*/c
                               directory-exists?)]))]))
          
;; TODO: it would be good to centralize
;; duplicate checking

(define directory-corpus% 
  (class corpus%
    (init path)
    ;; I used to have (invariant-assertion (absolute-path? (simplify-path path)))
    ;; Why?
    (super-new
     [docs
      (let ([dir-valid? (directory-validate-xml #:quiet? #t
                                                path)])
        (TODO/void Should there be an abstraction for reading in directory?
                   #: Happens here & in TEI Lint.
                   Would be faster to try valid-xml-file? on subgroups.)
        (instance-set 
         (for/list ([pth (in-directory path)]
                    [i (in-naturals)] ;; for debugging
                    #:when (xml-path? pth)
                    #:when (or dir-valid?
                               (valid-xml-file? #:quiet? #t pth))
                    [maybe-doc (in-value
                                (with-handlers
                                    ([exn:fail?
                                      (λ (e)
                                        #|(match-define-values {_ name _}
                                           (split-path pth))
                                         (eprintf "invalid: ~a: ~e\n  message: ~e\n"
                                                  i
                                                  name
                                                  (exn-message e))|#
                                        #f)])
                                  (file->tei-document pth)))]
                    #:when maybe-doc)
           maybe-doc)))])))

