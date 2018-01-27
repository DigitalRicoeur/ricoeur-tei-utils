#lang racket/base

(require (submod "icon.rkt" write)
         launcher/launcher
         setup/dirs
         setup/cross-system
         racket/runtime-path
         setup/path-to-relative
         racket/file
         )

(provide installer
         )

(define-runtime-path this-dir
  ".")

(define (installer parent-of-collects-dir
                   this-collection-dir
                   user-specific?
                   avoid-modifying-installation?
                   )
  (write-icons)
  (for ([variant (available-gracket-variants)])
    (parameterize ([current-launcher-variant variant])
      (make-launcher/variant user-specific?
                             avoid-modifying-installation?))))

(define (make-launcher/variant user-specific?
                               skip-non-addon?)
  ;; Based on setup/setup-core
  (define name
    "DR Migration Assistant")
  (define p
    (gracket-program-launcher-path name #:user? user-specific?))
  (define addon-p
    (and (find-addon-tethered-gui-bin-dir)
         (gracket-program-launcher-path name #:user? #t #:tethered? #t)))
  (define config-p
    (and (not user-specific?)
         (find-config-tethered-gui-bin-dir)
         (gracket-program-launcher-path name #:user? #f #:tethered? #t)))
  (unless skip-non-addon?
    (make-parent-directory* p)
    (when config-p
      (make-parent-directory* config-p)))
  (when addon-p
    (make-parent-directory* addon-p))
  (define aux-from-path
    (build-aux-from-path (build-path this-dir "migration-assistant")))
  (define (create p user? tethered?)
    (define aux
      `((exe-name . ,name)
        (relative? . ,(and (not user-specific?)
                           (not tethered?)
                           (not (get-absolute-installation?))))
        (install-mode . ,(if tethered?
                             (if user? 'addon-tethered 'config-tethered)
                             (if (not user-specific?) 'main 'user)))
        ,@aux-from-path))
    (unless (gracket-launcher-up-to-date? p aux)
      (printf
       "launcher: ~a~a\n"
       (path->relative-string/gui-bin p)
       (let ([v (current-launcher-variant)])
         (if (eq? v (cross-system-type 'gc))
             ""
             (format " [~a]" v))))
      (flush-output (current-output-port))
      (make-gracket-launcher
       #:tether-mode (and tethered?
                          (if user?
                              'addon
                              'config))
       '("-l-" "ricoeur/tei/tools/migration/migration-assistant.rkt")
       p
       aux)))
  (unless skip-non-addon?
    (create p user-specific? #f)
    (when config-p
      (create config-p #f #t)))
  (when addon-p
    (create addon-p #t #t)))



(define path->relative-string/gui-bin 
  (make-path->relative-string
   (list (cons find-gui-bin-dir "<gui-bin>/"))))






