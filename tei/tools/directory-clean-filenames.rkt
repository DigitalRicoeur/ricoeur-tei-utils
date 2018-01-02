#!/usr/bin/env racket
#lang racket/base

(module real racket/base
  (require ricoeur/lib/xml-path
           racket/lazy-require
           adjutor
           )
  (lazy-require [racket/system (system*)])
  (provide directory-clean-filenames
           git-mv
           )
  (define git
    (let ([git (find-executable-path "git")])
      (unless git
        (log-error "Warning: git not found"))
      git))
  (define (git-mv from to)
    (if git
        (system* git #"mv" from to)
        (rename-file-or-directory from to)))
  (define (directory-clean-filenames [dir (current-directory)]
                                     #:mv [mv rename-file-or-directory])
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
      (mv orig-pth new-pth))))

(module cmd racket/base
  (require (submod ".." real)
           racket/cmdline)
  (provide directory-clean-filenames-command)
  (define (directory-clean-filenames-command [argv (current-command-line-arguments)])
    (define mv
      rename-file-or-directory)
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
      (set! mv git-mv)]
     #:args ([dir (current-directory)])
     (directory-clean-filenames dir #:mv mv))))

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




          
