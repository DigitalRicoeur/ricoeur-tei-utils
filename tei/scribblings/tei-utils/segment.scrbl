#lang scribble/manual

@title{Document Segments}
@(declare-exporting ricoeur/tei/base
                    ricoeur/tei)

@(require "for-manual.rkt"
          (for-label data/order
                     ))

@deftech{segment}: define it

@deftech{segment metadata} value: define that, too

@; Base Segments
@defproc[(tei-document-segments [doc tei-document?])
         (listof base-segment?)]
@defstruct*[base-segment ([meta segment-meta?]
                          [body (and/c string-immutable/c #px"\\S")])
            #:omit-constructor]

@; Meta
@defpredicate[segment?]
@defpredicate[segment-meta?]


@defthing[segment-order order?]         
@defthing[prop:segment
          (struct-type-property/c
           (-> any/c segment-meta?))]
@defproc[(segment-get-meta [seg segment?])
         segment-meta?]
@defproc[(segment-meta=? [a segment?] [b segment?])
         boolean?]


@defproc[(segment-title/symbol [seg segment?])
         symbol?]
@defproc[(segment-counter [seg segment?])
         natural-number/c]
@deftogether[
 (@defproc[(segment-resp-string [seg segment?])
           string-immutable/c]
   @defproc[(segment-by-ricoeur? [seg segment?])
            boolean?])]
@deftogether[
 (@defproc[(segment-page-spec [seg segment?])
           page-spec/c]
   @defthing[page-spec/c flat-contract?
             #:value (or/c (maybe/c string-immutable/c)
                           (list/c (maybe/c string-immutable/c)
                                   (maybe/c string-immutable/c)))])]
@deftogether[
 (@defproc[(segment-location-stack [seg segment?])
           location-stack?]
   @defproc[(location-stack->strings [location-stack location-stack?])
            (listof string-immutable/c)]
   @defpredicate[location-stack?])]




