#lang racket/gui

(require framework
         ricoeur/tei/base
         ricoeur/tei/tools/tei-lint/lib
         "../toolkit.rkt"
         )

(provide document-frame-component:heading
         document-frame-component:citation
         document-frame-component:book/article
         )

(define-syntax-rule (define-info-component (name doc)
                      body ...)
  (define name
    (document-frame-component/ok
     (λ (doc)
       body ...))))

(define-info-component (document-frame-component:heading doc)
  (define title
    (instance-title doc))
  (λ (this)
    ;; Status and Path
    (let* ([row (new horizontal-pane%
                     [parent this]
                     [alignment '(left center)])])
      (new status-canvas%
           [status (send this get-lint-status)]
           [parent row])
      (new path-message%
           [parent row]
           [font bold-system-font]
           [path (send this get-path)]))
    ;; Title
    (let ([row (new horizontal-pane%
                    [parent this]
                    [alignment '(left top)])])
      (new message%
           [parent row]
           [font bold-system-font]
           [label "Title:"])
      (new message%
           [parent row]
           [label title]))))



(define-info-component (document-frame-component:citation doc)
  (define citation
    (string-trim
     (instance-citation doc)))
  (λ (this)
    (define row
      (new horizontal-pane%
           [parent this]
           [alignment '(left top)]))
    (new message%
         [parent row]
         [font bold-system-font]
         [label "Citation:"])
    (new citation-editor-canvas%
         [parent row]
         [content citation])))

(define citation-editor-canvas%
  (natural-height-mixin constant-editor-canvas%))



(define-info-component (document-frame-component:book/article doc)
  (define book/article
    (instance-book/article doc))
  (λ (this) 
    (insert-message-row this
                        "Type: "
                        (case book/article
                          [(book)
                           "Book"]
                          [(article)
                           "Article"]))))


