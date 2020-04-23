#!/bin/bash

# Script to reporoduce <https://github.com/racket/typed-racket/issues/902>.

# set RACKET to use a different Racket

RACKET=${RACKET:-racket}

# See "./kernel/types.rkt" for more details.

DIR=typed-racket-issue-902-data
BOOK=$DIR/book-chapter-10.2307_j.ctt1gxpd95.15.xml
ARTICLE=$DIR/journal-article-10.5406_ethnomusicology.57.1.0001.xml

# The issue occurs with any one of these definitions for `TARGET`:
# ----------------------------------------------------------------
#TARGET=$BOOK
#TARGET=$ARTICLE
TARGET=$DIR # does both $BOOK and $ARTICLE

"$RACKET" --version && "$RACKET" demo.rkt $TARGET >/dev/null
