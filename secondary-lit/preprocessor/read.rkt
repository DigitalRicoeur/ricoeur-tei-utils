#lang racket/base

(require file/unzip
         "../kernel/types.rkt"
         "types.rkt"
         "parse.rkt"
         ricoeur/kernel/xml-path
         ricoeur/tei/kernel/xexpr/normalize
         racket/port
         racket/path
         racket/match
         racket/contract
         syntax/parse/define
         (for-syntax racket/base))

(provide (contract-out
          [read-jstor-metadata-files
           (->* [(or/c path-string? (listof path-string?))
                 #:model topic-model?]
                [#:show-progress? any/c]
                metadata-archive?)]
          ))

(define (read-jstor-metadata-files args
                                   #:model model
                                   #:show-progress? [show-progress? #f])
  (define (call-with-newline-error-handler thunk)
    (with-handlers ([values (λ (e)
                              (newline (current-error-port))
                              (raise e))])
      (thunk)))
  (define-simple-macro (with-progress-newline-handler body:expr ...+)
    #:with thunk:id (or (syntax-local-name) #'progress-newline-handler-thunk)
    (let ([thunk (λ () body ...)])
      (if show-progress?
          (call-with-newline-error-handler thunk)
          (thunk))))
  ;; there are around 18963 files; 18076 unique
  ;; https://www.jstor.org/dfr/about/technical-specifications
  ;; "Multiple zip files may be necessary for larger datasets."
  ;; Only metadata is ".xml", so we can ignore directory structure.
  ;; (N-Grams & OCR, which we don't get, are ".txt".)
  (define store (make-metadata-parsing-store model))
  (define checksums-done (make-hash))
  (define (read-input in)
    (define checksum
      (sha1-bytes (peeking-input-port in)))
    (cond
      [(hash-has-key? checksums-done checksum)
       (when show-progress?
         (eprintf "    duplicate checksum; skipping\n"))]
      [else
       (hash-set! checksums-done checksum #t)
       (when show-progress?
         (eprintf "    parsing..."))
       (define item
         (with-progress-newline-handler
          (parse-book/article-xexpr
           (read-xexpr/standardized in))))
       (when show-progress?
         (eprintf " done\n    searching..."))
       (define matched?
         (with-progress-newline-handler
          (metadata-parsing-store-insert! store item)))
       (when show-progress?
         (eprintf " done; ~a\n"
                  (if matched?
                      "found matches"
                      "no matches")))]))
  (define (read-zip-element name-bs directory? in)
    (unless directory?
      (when (xml-path? (bytes->path name-bs))
        (when show-progress?
          (eprintf "  reading zip entry ~v\n" name-bs))
        (read-input in))))
  (define args-lst (if (list? args) args (list args)))
  (define count (length args-lst))
  (define modes-lst
    (for/list ([pth (in-list args-lst)])
      (cond
        [(directory-exists? pth) 'directory]
        [(xml-path? pth) 'xml]
        [(equal? #".zip" (path-get-extension pth)) 'zip]
        [else
         (raise-argument-error 'read-jstor-metadata-files
                               "a \".zip\" file, an \".xml\" file, or a directory"
                               pth)])))
  (for ([pth (in-list args-lst)]
        [mode (in-list modes-lst)]
        [i (in-naturals 1)])
    (define description-of-pth
      (match mode
        ['directory "files in directory"]
        ['zip "contents of zip file"]
        ['xml "single xml file"]))
    (when show-progress?
      (eprintf "reading ~a\n" description-of-pth))
    (match mode
      ['xml
       (call-with-input-file* pth
         read-input)]
      ['directory
       (for ([pth (in-directory pth)]
             #:when (xml-path? pth))
         (when show-progress?
           (eprintf "  reading file ~v\n" pth))
         (call-with-input-file* pth
           read-input))]
      ['zip
       (call-with-input-file* pth
         (λ (zip-in)
           (unzip zip-in read-zip-element)))])
    (when show-progress?
      (eprintf "finished reading ~a\n" description-of-pth)))
  (metadata-parsing-store->archive store))
