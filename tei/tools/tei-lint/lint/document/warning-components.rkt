#lang racket/gui

(require framework
         ricoeur/tei/base
         ricoeur/tei/tools/tei-lint/lib
         "../toolkit.rkt"
         "../paragraphs.rkt"
         "pb-report.rkt"
         )

(provide document-frame-component:paragraphs
         document-frame-component:pages
         document-frame-component:date
         )

(define document-frame-component:pages
  (document-frame-component
   (λ (doc)
     (match-define (pb-report messages warning?)
       (tei-document-pb-report doc))
     (values
      (if warning?
          'warning
          'ok)
      (λ (this)
        (define row
          (new horizontal-pane%
               [parent this]
               [alignment '(left top)]))
        (new message%
             [parent row]
             [font bold-system-font]
             [label "Pages:"])
        (cond
          [(null? messages)
           (new none-message%
                [parent row])]
          [else
           (new constant-editor-canvas%
                [parent row]
                [content (for/list ([m (in-list messages)])
                           (format "• ~a\n" m))]
                [min-height 300])]))))))

(define none-message%
  (red-text-message "NONE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define document-frame-component:date
  (document-frame-component
   (λ (doc)
     (def
       [this-dt
        (instance-publication-date doc)]
       [orig-dt
        (instance-orig-publication-date doc)]
       [this-is-orig?
        (instance-publication-original? doc)]
       [order-ok?
        (or this-is-orig?
            (date<=? orig-dt this-dt))])
     (values
      (if order-ok?
          'ok
          'warning)
      (λ (frame)
        (define sect
          (new vertical-pane%
               [parent frame]
               [alignment '(left top)]))
        (unless order-ok?
          (new bad-date-order-message%
               [parent sect]))
        (cond
          [this-is-orig?
           (install-date-row sect
                             "Publication Date (this is original):"
                             this-dt)]
          [else
           (install-date-row sect "Publication Date:" this-dt)
           (install-date-row sect "Original Publication Date:" orig-dt)]))))))

(define bad-date-order-message%
  (red-text-message "Original publication date after this publication date."))

(define (install-date-row parent str dt)
  (let ([row (new horizontal-pane%
                  [parent parent]
                  [alignment '(left top)])])
    (new message%
         [parent row]
         [font bold-system-font]
         [label str])
    (new message%
         [parent row]
         [label (~t dt "y")])))







