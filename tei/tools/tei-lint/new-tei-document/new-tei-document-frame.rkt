#lang racket/gui

(require "bib-info.rkt"
         "pages.rkt"
         ricoeur/tei/tools/tei-lint/lib
         ricoeur/tei
         framework
         adjutor
         )

(provide (contract-out
          [new-tei-document-frame%
           (class/c
            (init [string string-immutable/c]
                  [path (or/c #f path-string-immutable/c)]))]
          ))




(define dialog:confirm-ready-to-save%
  (class dialog%
    (inherit show)
    (init [(maybe-pth maybe-plain-text-path)]
          info
          page-description-strings
          [(_bx bx:result)])
    (define bx:result _bx)
    (super-new [label "Confirm Save - TEI Lint"]
               [alignment '(center top)])
    (new message%
         [label "Are you ready to save?"]
         [font big-bold-system-font]
         [parent this])
    (define body
      (new vertical-pane%
           [alignment '(left top)]
           [parent this]))
    (new message%
         [label (string-append "Please confirm that you are ready to "
                               "save a new TEI XML document with the "
                               "following information:")]
         [parent body])
    (let ()
      (match-define (bib-info title-str
                              b/a-sym
                              lang-sym
                              citation-str
                              (publication-date-spec this-dt orig-dt)
                              a+e)
        info)
      (insert-message-row
       body
       "Title:"
       title-str
       #:stretchable-height #f)
      (insert-message-row
       body
       "Document Type:"
       (case b/a-sym
         [(book) "Book"]
         [else "Article"])
       #:stretchable-height #f)
      (insert-message-row
       body
       "Language:"
       (case lang-sym
         [(en) "English"]
         [else "French"])
       #:stretchable-height #f)
      (let ([row (new horizontal-pane%
                      [alignment '(left top)]
                      [stretchable-height #f]
                      [parent body])])
        (new message%
             [parent row]
             [font bold-system-font]
             [label "Citation:"])
        (new editor-message%
             [parent row]
             [content citation-str]))
      (let ([grp (new vertical-pane%
                      [alignment '(left top)]
                      [stretchable-height #f]
                      [parent body])])
        (insert-message-row
         grp
         "Publication Date:"
         (publication-date-list->english this-dt)
         #:stretchable-height #f)
        (case orig-dt
          [(thisIsOriginal)
           (new message%
                [label "This is the first published instance."]
                [font bold-system-font]
                [parent grp])]
          [else
           (insert-message-row
            grp
            "Original Publication Date:"
            (publication-date-list->english orig-dt)
            #:stretchable-height #f)]))
      (let ([grp (new group-box-panel%
                      [label "Authors & Editors:"]
                      [alignment '(left top)]
                      [stretchable-height #f]
                      [parent body])])
        (for ([it (in-list a+e)])
          (new author/editor-panel%
               [author/editor-spec it]
               [parent grp])))
      (let ([grp (new group-box-panel%
                      [label "Pages:"]
                      [alignment '(left top)]
                      [stretchable-height #f]
                      [parent body])])
        (for ([s (in-list page-description-strings)])
          (new message%
               [label s]
               [parent grp]))))
    (gui-utils:ok/cancel-buttons
     (new horizontal-pane%
          [alignment '(right center)]
          [stretchable-height #f]
          [parent body])
     (λ (b e) (return-result #t))
     (λ (b e) (return-result #f))
     "Save")
    (define/public-final (return-result rslt)
      (set-box! bx:result rslt)
      (show #f))
    #|END class dialog:confirm-ready-to-save%|#))


(define (confirm-ready-to-save maybe-pth
                               valid-info
                               page-description-strings
                               [parent #f])
  (call-in-eventspace-thread
   #:parent parent
   (λ ()
     (define bx:result
       (box #f))
     (define it
       (new dialog:confirm-ready-to-save%
            [maybe-plain-text-path maybe-pth]
            [info valid-info]
            [page-description-strings page-description-strings]
            [bx:result bx:result]
            [parent parent]))
     (send it show #t)
     (unbox bx:result))))







(define unfinished-steps-dialog%
  (class dialog%
    (inherit show)
    (init info pages-done?)
    (super-new [label "Missing Required Information"])
    (define row
      (new horizontal-pane%
           [alignment '(left top)]
           [parent this]))
    (new message%
         [label 'caution]
         [parent row])
    (define col
      (new vertical-pane%
           [alignment '(left top)]
           [parent row]))
    (new message%
         [label "Missing Required Information"]
         [font big-bold-system-font]
         [parent col])
    (new message%
         [label "Please complete the remaining steps."]
         [parent col])
    (let ([grp (new vertical-pane%
                    [alignment '(left top)]
                    [stretchable-height #f]
                    [parent col])])
      (match-define (bib-info title b/a lang citation pub-date _)
        info)
      (unless title
        (new message%
             [label "• Provide the title."]
             [parent grp]))
      (unless b/a
        (new message%
             [label "• Identify the document as a book or an article."]
             [parent grp]))
      (unless lang
        (new message%
             [label "• Identify the primary language of the document."]
             [parent grp]))
      (unless citation
        (new message%
             [label "• Provide a human-readable citation."]
             [parent grp]))
      (unless pub-date
        (new message%
             [label "• Provide details about the publication date."]
             [parent grp]))
      (unless pages-done?
        (new message%
             [label "• Finish numbering all pages."]
             [parent grp])))
    (new button%
         [label "Ok"]
         [callback (λ (b e) (show #f))]
         [parent (new horizontal-pane%
                      [alignment '(right center)]
                      [parent this])])
    #|END class unfinished-steps-dialog%|#))


(define (show-unfinished-steps info
                               pages-done?
                               [parent #f])
  (send (new unfinished-steps-dialog%
             [info info]
             [pages-done? pages-done?]
             [parent parent])
        show
        #t))









(define (month-int->2digit-string x)
  (define str
    (number->string x))
  (string->immutable-string
   (if (= 2 (string-length str))
       str
       (string-append "0" str))))

(define (month-int->english-string x)
  (case x
    [(1) "January"]
    [(2) "February"]
    [(3) "March"]
    [(4) "April"]
    [(5) "May"]
    [(6) "June"]
    [(7) "July"]
    [(8) "August"]
    [(9) "September"]
    [(10) "October"]
    [(11) "November"]
    [(12) "December"]))

(define (publication-date-list->iso it)
  (string->immutable-string
   (match it
     [(list (app number->string y))
      y]
     [(list (app number->string y)
            (app month-int->2digit-string m))
      (string-append y "-" m)])))

(define (publication-date-list->english it)
  (string->immutable-string
   (match it
     [(list (app number->string y))
      y]
     [(list (app number->string y)
            (app month-int->english-string m))
      (string-append m " " y)])))

(define (make-date-element subtype p-d-l)
  `(date ([type "publication"]
          [subtype ,subtype]
          [when ,(publication-date-list->iso p-d-l)])
         ,(publication-date-list->english p-d-l)))

(define publication-date-spec->xexpr-forest
  (match-lambda
    [(publication-date-spec this 'thisIsOriginal)
     (list " This instance was published on "
           (make-date-element "thisIsOriginal" this)
           ". It was the first instance published in any language.")]
    [(publication-date-spec this orig)
     (list " This instance was published on "
           (make-date-element "this" this)
           ". This work was first publised as a whole on "
           (make-date-element "original" orig)
           ".")]))

(define (make-tei-document valid-info ab-xexpr)
  (match-define (bib-info title-str
                          (app symbol->string
                               (app string->immutable-string
                                    book/article-str))
                          (app symbol->string
                               (app string->immutable-string
                                    lang-str))
                          citation-str
                          date-spec
                          a+e)
    valid-info)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; fileDesc
  (define* fileDesc-xexpr
    (define titleStmt-xexpr
      `(titleStmt
        (title ,title-str)
        ,@(map author/editor-spec->xexpr a+e)))
    (define sourceDesc-xexpr
      `(sourceDesc
        (bibl ,citation-str
              ,@(publication-date-spec->xexpr-forest date-spec))))
    ;;;;;;;;;;;;;;;;;;;;
    `(fileDesc
      ,titleStmt-xexpr
      (publicationStmt
       (authority "Digital Ricoeur")
       (availability ([status "restricted"])
                     (p "Not for distribution.")))
      ,sourceDesc-xexpr))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; profileDesc
  (define* profileDesc-xexpr
    (define taxonomy/type-uri-base-str
      "https://schema.digitalricoeur.org/taxonomy/type")
    `(profileDesc
      (textClass
       (catRef ([scheme ,taxonomy/type-uri-base-str]
                [target ,(string-append taxonomy/type-uri-base-str
                                        "#"
                                        book/article-str)]))
       (keywords ([scheme "https://schema.digitalricoeur.org/tools/tei-guess-paragraphs"])
                 (term "todo")))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TEI
  (define* TEI-xexpr
    (define teiHeader-xexpr
      `(teiHeader ,fileDesc-xexpr
                  ,profileDesc-xexpr))
    (define text-xexpr
      `(text ([xml:lang ,lang-str])
             (body () ,ab-xexpr)))
    `(TEI ([version "5.0"]
           [xmlns "http://www.tei-c.org/ns/1.0"])
          ,teiHeader-xexpr
          ,text-xexpr))
  (xexpr->tei-element TEI-xexpr))


(define (get-destination-path [parent #f])
  (define pth
    (put-file
     "Save the new TEI XML document as:"
     parent
     #f
     #f
     "xml"
     null
     '(["TEI XML" "*.xml"])))
  (let ([pth (if (string? pth)
                 (string->immutable-string pth)
                 pth)])
    (cond
      [(not pth)
       #f]
      [(not (xml-path? pth))
       (message-box
        "Not an XML Path - TEI Lint"
        (string-append
         "The file name you selected does not end with the extension \".xml\".\n\n"
         "Please choose a different file name.\n\n"
         "You selected: \"" (path->string* pth) "\"")
        parent
        '(ok stop))
       #f]
      [(file-exists? pth)
       (case (message-box/custom
              "File Exists - TEI Lint"
              (string-append
               "The file \"" (path->string* pth) "\" already exists.\n\n"
               "Do you want to replace it?")
              "Overwrite"
              "Cancel"
              #f
              parent
              '(caution no-default))
         [(1) pth]
         [else #f])]
      [else
       pth])))






(define dialog:confirm-saved+ask-to-delete%
  (class dialog%
    (inherit show)
    (init maybe-plain-text-path
          xml-path
          [(_bx bx:result)])
    (define bx:result _bx)
    (super-new [label "File Saved Successfully - TEI Lint"]
               [alignment '(center top)])
    (new message%
         [label "File Saved Successfully"]
         [font big-bold-system-font]
         [parent this])
    (define body
      (new vertical-pane%
           [alignment '(left top)]
           [parent this]))
    (new message%
         [label "Your new TEI XML document has been saved successfully."]
         [parent body])
    (let ([row (new horizontal-pane%
                    [alignment '(left top)]
                    [parent body])])
      (new message%
           [label "New File:"]
           [font bold-system-font]
           [parent row])
      (new path-message%
           [path xml-path]
           [parent row]))
    (new message%
         [label "Would you like to delete the old plain-text file?"]
         [parent body])
    (let ([row (new horizontal-pane%
                    [alignment '(left top)]
                    [parent body])])
      (cond
        [maybe-plain-text-path
         (new message%
              [label "Old Plain-Text File:"]
              [parent row])
         (new path-message%
              [path maybe-plain-text-path]
              [parent row])]
        [else
         (new message%
              [label "(There was no plain-text file, so the answer doesn't matter.)"]
              [parent row])]))
    (gui-utils:ok/cancel-buttons
     (new horizontal-pane%
          [alignment '(right center)]
          [stretchable-height #f]
          [parent body])
     (λ (b e) (return-result #t))
     (λ (b e) (return-result #f))
     "Delete Plain-Text File"
     "Keep Plain-Text File")
    (define/public-final (return-result rslt)
      (set-box! bx:result rslt)
      (show #f))
    #|END class dialog:confirm-saved+ask-to-delete%|#))


(define (confirm-saved maybe-plain-text-path
                       xml-path
                       [parent #f])
  (define bx:result
    (call-in-eventspace-thread
     #:parent parent
     (λ ()
       (define bx:result
         (box #f))
       (define it
         (new dialog:confirm-saved+ask-to-delete%
              [maybe-plain-text-path maybe-plain-text-path]
              [xml-path xml-path]
              [bx:result bx:result]
              [parent parent]))
       (send it show #t)
       bx:result)))
  (when (and maybe-plain-text-path
             (unbox bx:result)
             (file-exists? maybe-plain-text-path))
    (delete-file maybe-plain-text-path)
    (message-box
     "Plain Text File Deleted - TEI Lint"
     (string-append
      "The plain text file \"" (path->string* maybe-plain-text-path)
      "\" was deleted successfully.")
     parent
     '(ok))
    (void)))
     

;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                                   ;;;;                                           
;                                                     ;;                                           
;      ;;;   ;;;    ;; ;;;    ;;;              ;;;    ;;      ;;      ;;      ;;      ;;;     ;;   
;    ;;   ; ;   ;   ;;;     ;;   ;           ;;   ;   ;;     ;  ;   ;;  ;   ;;  ;   ;;   ;  ;;  ;  
;    ;      ;   ;   ;;      ;    ;           ;        ;;        ;;   ;       ;      ;    ;   ;     
;   ;;     ;;   ;;  ;;     ;;;;;;;;         ;;        ;;      ;;;;    ;;      ;;   ;;;;;;;;   ;;   
;    ;      ;   ;   ;;      ;                ;        ;;     ;  ;;      ;;      ;;  ;           ;; 
;    ;;   ; ;   ;   ;;      ;;   ;           ;;   ;    ;    ;;  ;;  ;   ;   ;   ;   ;;   ;  ;   ;  
;      ;;;   ;;;    ;;        ;;;              ;;;      ;;   ;;; ;   ;;;     ;;;      ;;;    ;;;   
;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                                                                                  


(define workspace-panel%
  (class tab-panel%
    (inherit get-selection change-children)
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
                   [(1) pages-panel]))))))
    #|END class workspace-panel%|#))



  

(define new-tei-document-frame%
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
              [font bold-system-font]
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
    (gui-utils:ok/cancel-buttons
     (new horizontal-pane%
          [alignment '(right center)]
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
      (define doc
        (make-tei-document valid-info ab-xexpr))
      (define xml-path
        (get-destination-path this))
      (when xml-path
        (with-output-to-file xml-path
          #:mode 'text
          #:exists 'replace ;; checked in get-destination-path
          (λ ()
            (write-tei-document doc)))
        (confirm-saved maybe-pth xml-path this)
        (show #f)))
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

(module+ main
  (provide (all-defined-out))
  (define f
    (new new-tei-document-frame%
         [string "a\fb\fc"]
         [path #f]))

  (send f show #t))




