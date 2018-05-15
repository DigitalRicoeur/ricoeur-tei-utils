#lang racket

(require data/maybe
         (rename-in data/functor
                    [map fmap])
         )

(provide (all-from-out data/maybe)
         fmap
         )
