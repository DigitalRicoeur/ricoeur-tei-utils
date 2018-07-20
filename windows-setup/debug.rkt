#lang racket/gui

(require xml
         racket/serialize
         ricoeur/tei/base)

(module+ main
  (run))
  
(struct file-info (path xexpr write string)
  #:prefab)

(define (make-file-info pth)
  (file-info pth
             (file->xexpr pth)
             (file->write-tei-document-string pth)
             (file->string/text pth)))

(define (file->xexpr pth)
  (call-with-input-file pth
    #:mode 'text
    (λ (in)
      (regexp-try-match #rx"^\uFEFF" in)
      (xml->xexpr
       (document-element
        (read-xml in))))))

(define (file->write-tei-document-string pth)
  (with-output-to-string
    (λ ()
      (write-tei-document
       (file->tei-document pth)))))

(define (file->string/text pth)
  (file->string pth #:mode 'text))

(define (directory->file-info-list dir)
  (map (λ (p) (make-file-info (build-path dir p)))
       '("blank-lines-example.xml"
         "line-breaks-example.xml")))

                   
;                          
;                          
;     ;;    ;;  ;;  ;;;;;; 
;    ;  ;;  ;;  ;;    ;;   
;   ;       ;;  ;;    ;;   
;   ;       ;;  ;;    ;;   
;  ;;       ;;  ;;    ;;   
;  ;;  ;;;  ;;  ;;    ;;   
;   ;   ;;   ;  ;     ;;   
;   ;   ;;   ;  ;     ;;   
;    ;;;      ;;    ;;;;;; 
;                          
;   

(define (on-canceled)
  (message-box "Program Canceled"
               "You canceled the program. Press \"Run\" in DrRacket to try again."
               #f
               '(ok stop))
  (exit 1))

(define (get-file-info-list)
  (define dir
    (get-directory
     "Please select the \"texts/debug/windows-paragraphs/\" directory."))
  (unless dir
    (on-canceled))
  (directory->file-info-list dir))

(define (get-output-file)
  (or
   (put-file "Please save the debugging information file."
             #f
             #f
             "windows-paragraphs-debug-info-revised.rktd"
             "rktd"
             null
             '(["Racket Data" "*.rktd"]))
   (on-canceled)))

(define (save-file-info-list lst pth)
  (write-to-file (serialize lst)
                 pth
                 #:exists 'truncate/replace)
  (message-box "Saving Complete"
               "The file has been saved. You may now quit DrRacket.")
  (void))


(define (run)
  (save-file-info-list (get-file-info-list)
                       (get-output-file)))

