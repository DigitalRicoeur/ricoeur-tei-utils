#lang racket/base

(require ricoeur/tei/base
         racket/cmdline)

(provide validate-directory-command)

(define (validate-directory-command [argv (current-command-line-arguments)])
  (command-line
   #:program "raco tei to-plain-text"
   #:argv argv
   #:usage-help
   "Validates TEI XML files in <dir>, which defaults to"
   "  the current directory, and recursive subdirectories."
   "Checks both the DTD (if xmllint is available) and"
   "  Digital-Ricoeur-specific requirements."
   #:args ([dir (current-directory)])
   (unless (directory-validate-xml dir)
     (exit 1))
   (for ([pth (in-directory dir)]
         #:when (xml-path? pth))
     (void (file->tei-document pth)))))

