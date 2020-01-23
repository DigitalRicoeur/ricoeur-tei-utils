#lang racket/base

;; Keep this fast to load, since it's needed to get to --help

(require racket/cmdline
         racket/runtime-path
         (for-syntax racket/base))

(provide start-interactive-preprocessor)

(define-runtime-module-path-index mpi:racket/pretty
  'racket/pretty)
(define-runtime-module-path-index mpi:racket/gui/dynamic
  'racket/gui/dynamic)
(define-runtime-module-path-index mpi:write
  "write.rkt")

(define (start-interactive-preprocessor model)
  (define show-progress? #f)
  (define output-pth #f)
  (define exists-flag 'error)
  (define args
    (command-line
     #:usage-help
     "Preprocesses JSTOR secondary-literature metadata from the specified"
     "<path>s to create a binary archive."
     "Each <path> should be a \".zip\" file, a directory, or an \".xml\" file."
     "The preprocessor will use the topic model defined in this script."
     "By convention, the binary archive file uses the extension \".fasl.rktd\"."
     #:once-each
     [("-o") archive-file "Write to <archive-file> rather than stdout)"
             (set! output-pth archive-file)]
     [("--overwrite") "With -o, overwrite <archive-file> if it already exists"
                      (set! exists-flag 'truncate/replace)]
     [("-p" "--progress") "Write verbose progress messages to stderr"
                          (set! show-progress? #t)]
     [("--show-model") "Print the topic model to stdout and exit immediately"
                       ((dynamic-require mpi:racket/pretty 'pretty-print) model)
                       (exit 0)]
     #:args path
     path))
  (cond
    [((dynamic-require mpi:racket/gui/dynamic 'gui-available?))
     ;; TODO: gui stuff
     (void)]
    [else
     (define build/write-metadata-archive
       (dynamic-require mpi:write 'build/write-metadata-archive))
     (build/write-metadata-archive args
                                   #:model model
                                   #:output (or output-pth (current-output-port))
                                   #:exists exists-flag
                                   #:show-progress? show-progress?)]))
  