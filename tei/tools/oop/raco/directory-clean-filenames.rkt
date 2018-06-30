#!/usr/bin/env racket
#lang racket/base

(module real racket/base
  (require (only-in ricoeur/tei/kernel
                    xml-path?)
           racket/lazy-require
           adjutor
           )
  (lazy-require [racket/system (system*)])
  (provide directory-clean-filenames
           git-mv
           git
           )
  (define git
    (let ([git (find-executable-path "git")])
      (unless git
        (log-warning "\"git\" executable not found"))
      git))
  (define (git-mv from to)
    (if git
        (system* git #"mv" from to)
        (rename-file-or-directory from to)))
  (define (directory-clean-filenames [dir (current-directory)]
                                     #:mv [mv rename-file-or-directory]
                                     #:box:ok? [box:ok? (box #t)])
    (for ([orig-pth (in-directory dir)]
          #:when (xml-path? orig-pth)
          [(base orig-name-pth _) (in-value*/expression
                                   (split-path orig-pth))]
          #:when (regexp-match? #px#"\\s|^\\P{Ll}"
                                orig-name-pth))
      (define new-pth
        (build-path
         base
         (bytes->path-element
          (regexp-replaces (path-element->bytes orig-name-pth)
                           `([#px#"\\s" "_"]
                             [#px#"^\\P{Ll}"
                              ,(λ (bs)
                                 (if (regexp-match? #px#"\\p{Lu}" bs)
                                     (string->bytes/utf-8
                                      (string-foldcase
                                       (bytes->string/utf-8 bs)))
                                     (bytes-append #"autocleaned_" bs)))])))))
      (cond
        [(file-exists? new-pth)
         (set-box! box:ok? #f)
         (eprintf "WARNING: ~a\n  path: ~e\n  target: ~e\n"
                  "skipping path where target already exists"
                  orig-pth
                  new-pth)]
        [else
         (mv orig-pth new-pth)]))))

(module cmd racket/base
  (require (submod ".." real)
           racket/cmdline)
  (provide directory-clean-filenames-command)
  (define (directory-clean-filenames-command [argv (current-command-line-arguments)])
    (define mv
      rename-file-or-directory)
    (define box:ok?
      (box #t))
    (command-line
     #:program "raco tei directory-clean-filenames"
     #:argv argv
     #:usage-help
     "For any XML files in <dir>, which defaults to the current directory,"
     "  and recursive subdirectories, renames files as needed to ensure that"
     "  every name starts with a lowercase letter and that there are no spaces"
     "  in the names."
     "This works around a bug in ricoeur/TEI/xmllint."
     #:once-each
     [("--git" "-g")
      "Move files using \"git mv\" (when available)"
      (unless git
        (eprintf "WARNING: \"git\" executable not found\n  using fallback\n"))
      (set! mv git-mv)]
     #:args ([dir (current-directory)])
     (directory-clean-filenames dir #:mv mv #:box:ok? box:ok?)
     (unless (unbox box:ok?)
       (exit 1)))))

(require 'real
         'cmd
         racket/contract)

(provide (contract-out
          [git-mv (-> path? path? any)]
          [directory-clean-filenames
           (->* {(and/c path? directory-exists?)}
                                          {#:mv (-> path? path? any)}
                                          any)]
          [directory-clean-filenames-command
           (->* {}
                {(or/c list? vector?)})]
          ))




          
