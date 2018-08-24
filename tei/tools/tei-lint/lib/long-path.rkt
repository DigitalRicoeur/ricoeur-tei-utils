#lang racket/gui

(require adjutor
         pict
         )

(module+ test
  (require rackunit))

(provide (contract-out
          [path->wrapped-pict
           (->* {path-string?}
                {#:font (is-a?/c font%)
                 #:max-width (>/c 0)
                 #:indent (>=/c 0)} ;; there are more invariants ...
                pict?)]
          [path-message%
           (class/c
            (init [path (or/c #f path-string?)] ;; either path or label is required
                  [label (or/c #f path-string?)]
                  [max-width (>/c 0)]
                  [indent (>=/c 0)]
                  [font (is-a?/c font%)]))]
          ))


(define (pth->l-strings p)
  (define exploded
    (explode-path p))
  (cond
    [(infix: (length exploded) <= 2)
     (list (path->string p))]
    [else
     (define open-bs
       (path->bytes (apply build-path (take exploded 2))))
     (define sep-str
       (string (integer->char
                (bytes-ref (path->bytes p)
                           (bytes-length open-bs)))))
     (let loop ([so-far (list (let ([root (car exploded)])
                                (if (path? root)
                                    (path->string root)
                                    (bytes->string/utf-8
                                     (path->bytes root)))))]
                [to-go (map path-element->string
                            (cdr exploded))])
       (match to-go
         [(list final)
          (reverse (cons final so-far))]
         [(cons this more)
          (loop (cons (string-append this sep-str)
                      so-far)
                more)]))]))

(module+ test
  (check-equal?
   (pth->l-strings (bytes->path #"C:\\Users\\Foo\\bar.xml"
                                'windows))
   '("C:\\" "Users\\" "Foo\\" "bar.xml")
   "pth->l-strings : windows")
  (check-equal?
   (pth->l-strings (bytes->path #"/home/ubuntu/bar.xml"
                                'unix))
   '("/" "home/" "ubuntu/" "bar.xml")
   "pth->l-strings : unix"))

(define (path->wrapped-pict pth
                            #:max-width [max-width 500]
                            #:indent [indent-width 10]
                            #:font [font normal-control-font])
  (define to-pict
    (let ([style (cons 'aligned font)])
      (λ (str) (text str style))))
  (define indent-pict
    (blank indent-width 1))
  (define subseq-line-width
    (- max-width indent-width))
  (define rows
    (let make-rows ([so-far null]
                    [row-so-far (blank 0)]
                    [to-go (pth->l-strings (simple-form-path pth))])
      (match to-go
        ['()
         (reverse (cons row-so-far so-far))]
        [(cons str more)
         (define next-pict
           (to-pict str))
         (define extended-row
           (hbl-append row-so-far next-pict))
         (cond
           [(infix: (pict-width extended-row) <= max-width)
            (make-rows so-far extended-row more)]
           [(infix: (pict-width next-pict) <= subseq-line-width)
            (make-rows (cons row-so-far so-far)
                       (hbl-append indent-pict next-pict)
                       more)]
           [else
            (let make-rows/oversized-part ([so-far (cons row-so-far so-far)]
                                           [str str]
                                           [end (- (string-length str) 5)])
              (cond
                [(infix: end <= 5)
                 (make-rows so-far
                            (hbl-append indent-pict (to-pict str))
                            more)]
                [else
                 (define sub-pict
                   (to-pict (substring str 0 end)))
                 (cond
                   [(infix: (pict-width sub-pict) <= subseq-line-width)
                    (let* ([so-far (cons (hbl-append indent-pict sub-pict)
                                         so-far)]
                           [rest-str (substring str end)]
                           [rest-pict (to-pict rest-str)])
                      (cond
                        [(infix: (pict-width rest-pict) <= subseq-line-width)
                         (make-rows so-far
                                    (hbl-append indent-pict rest-pict)
                                    more)]
                        [else
                         (make-rows/oversized-part
                          so-far
                          rest-str
                          (- (string-length rest-str) 5))]))]
                   [else
                    (make-rows/oversized-part
                     so-far
                     str
                     (- end 5))])]))])])))
  (apply vl-append rows))



(define path-message%
  (class message%
    (init [path #f]
          [label #f]
          [max-width 500]
          [indent 10]
          [font normal-control-font])
    (super-new [label (pict->bitmap
                       #:make-bitmap make-screen-bitmap
                       (path->wrapped-pict
                        (or label path)
                        #:max-width max-width
                        #:indent indent
                        #:font font))])))


