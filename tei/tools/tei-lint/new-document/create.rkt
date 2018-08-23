#lang racket/gui

(require "bib-info.rkt"
         "pages.rkt"
         ricoeur/tei/tools/tei-lint/lib
         ricoeur/tei
         framework
         adjutor
         )

(define (get-txt-path [parent #f])
  (define pth
    (get-file
     "To begin, select a plain text file from OCR."
     parent
     #f
     #f
     #f
     null
     '(("Plain Text" "*.txt"))))
  (let ([pth (if (string? pth)
                 (string->immutable-string pth)
                 pth)])
    (and pth
         (file-exists? pth)
         pth)))

(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))
  
(define (file->text-string pth)
  (string->immutable-string
   (call-with-input-file* pth
     #:mode 'text
     (λ (in)
       (discard-bom in)
       (port->string in)))))





(define (confirm-ready-to-save maybe-pth
                               valid-info
                               page-description-strings
                               [parent #f])
  (TODO/void confirm-ready-to-save)
  #t)

(define (show-unfinished-steps info
                                pages-done?
                                [parent #f])
  (TODO/void show-unfinished-steps))






(define workspace-panel%
  (class tab-panel%
    (inherit get-selection change-children reflow-container)
    (init [(str string)])
    (define active-child 0)
    (super-new [choices '("Bibliographic Information"
                          "Pages")]
               [callback (λ (t e) (on-choose-tab))]
               [alignment '(left top)])
    (define bib-container
      (new panel:horizontal-dragable%
           [parent this]))
    (define bib-info-panel
      (new bib-info-panel%
           [parent bib-container]))
    (new constant-editor-canvas%
         [content (regexp-replace* #rx"\f"
                                   str
                                   "\n\n\n")]
         [min-width 300]
         [parent bib-container])
    (define pages-panel
      (new pages-panel%
           [string str]
           [style '(deleted)]
           [parent this]))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public-final (get-bib-info+pages-panels)
      (values bib-info-panel pages-panel))
    (define/private (on-choose-tab)
      (define n (get-selection))
      (unless (= n active-child)
        (set! active-child n)
        (change-children
         (λ (old-children)
           (list (case n
                   [(0) bib-container]
                   [(1) pages-panel]))))
        (reflow-container)))
    #|END class workspace-panel%|#))


(define new-document-frame%
  (class frame%
    (inherit show)
    (init [(str string)]
          [(_maybe-pth path)]
          [label "New TEI Document - TEI Lint"])
    (define maybe-pth _maybe-pth)
    (super-new [label label]
               [alignment '(center top)])
    (new message%
         [label "New TEI Document"]
         [font big-bold-system-font]
         [parent this])
    (let ([row (new horizontal-pane%
                    [stretchable-height #f]
                    [alignment '(left top)]
                    [parent this])])
      (cond
        [maybe-pth
         (new message%
              [label "Source:"]
              [parent row])
         (new path-message%
              [path maybe-pth]
              [parent row])]
        [else
         (new message%
              [label "Not created from a file."]
              [parent row])]))
    (define-values {bib-info-panel pages-panel}
      (send (new workspace-panel%
                 [string str]
                 [parent this])
            get-bib-info+pages-panels))
    (TODO/void ok + cancel: align left or right?)
    (gui-utils:ok/cancel-buttons
     (new horizontal-pane%
          [stretchable-height #f]
          [parent this])
     (λ (b e) (on-save-clicked))
     (λ (b e) (show #f))
     #:confirm-style null
     "Save")
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/private (on-save-clicked)
      (define info
        (send bib-info-panel get-bib-info))
      (define maybe-ab-xexpr
        (send pages-panel get-ab-xexpr))
      (cond
        [(and (bib-info-valid? info)
              maybe-ab-xexpr)
         (when (confirm-ready-to-save maybe-pth
                                      info
                                      (send pages-panel get-description-strings)
                                      this)
           (do-save-document info maybe-ab-xexpr))]
        [else
         (show-unfinished-steps info
                                (any->boolean maybe-ab-xexpr)
                                this)]))
    (define/private (do-save-document valid-info ab-xexpr)
      (TODO/void do-save-document))
    #|END class new-document-frame%|#))


;                                  
;                                  
;                                  
;                                  
;                      ;           
;                      ;;          
;  ; ;; ;;    ;;    ;;;;;   ;; ;   
;  ;; ;; ;   ;  ;      ;;   ;;; ;  
;  ;; ;; ;;     ;;     ;;   ;;  ;; 
;  ;; ;; ;;   ;;;;     ;;   ;;  ;; 
;  ;; ;; ;;  ;  ;;     ;;   ;;  ;; 
;  ;; ;; ;; ;;  ;;     ;;   ;;  ;; 
;  ;; ;; ;;  ;;; ;     ;;   ;;  ;; 
;                                  
;                                  
;                                  
;                                  


(define f
  (new new-document-frame%
       [string "a\fb\fc"]
       [path #f]))

(send f show #t)




