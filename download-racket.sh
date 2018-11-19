#!/bin/bash

# Based on install-racket.sh from
#   https://github.com/greghendershott/travis-racket
# This script does less work than that one:
#   it just builds the right URL and downloads it for you.
# It optionally takes a destination path at the command line.
# By default, it writes to "./install-racket.exe".
# If the "DRY_RUN" environment variable is set, 
#   it doesn't actually download anything.
# Nothing in this script depends on Windows or CygWin.

set -e

# Optionally give 
INSTALLER="${1:-./install-racket.exe}"


if [[ "$RACKET_MINIMAL" = "1" ]]; then
    MIN="minimal-"
else
    MIN=""
fi


DL_BASE="https://mirror.racket-lang.org/installers/${RACKET_VERSION}"
UTAH_BASE="https://www.cs.utah.edu/plt/snapshots/current/installers"
RELEASE_BASE="https://pre-release.racket-lang.org/installers"

############################################################
# Build the url
if [[ "$RACKET_VERSION" = "HEAD" ]] || [[ "$RACKET_VERSION" = "HEADCS" ]]; then
    if [[ "$RACKET_MINIMAL" = "1" ]]; then
	MIN="min-"
    fi
    if [[ "$RACKET_VERSION" = "HEADCS" ]]; then
	CS="-cs"
    else
	CS=""
    fi
    URL="${UTAH_BASE}/${MIN}racket-current-x86_64-win32${CS}.exe"
elif [[ "$RACKET_VERSION" = "RELEASE" ]]; then
    URL="${RELEASE_BASE}/racket-${MIN}current-x86_64-win32.exe"
elif [[ "$RACKET_VERSION" = 7.* ]] \
	 || [[ "$RACKET_VERSION" = 6.* ]] \
	 || [[ "$RACKET_VERSION" = 6.[0-4].[0-9] ]] \
	 || [[ "$RACKET_VERSION" = 5.9* ]]; then
    URL="${DL_BASE}/racket-${MIN}${RACKET_VERSION}-x86_64-win32.exe"
elif [[ "$RACKET_VERSION" = 5.3* ]]; then
    if [[ "$RACKET_MINIMAL" = "1" ]]; then
	RACKET="racket-textual"
    else
	RACKET="racket"
    fi
    URL="${DL_BASE}/${RACKET}/${RACKET}-${RACKET_VERSION}-bin-x86_64-win32.exe"
else
    echo "ERROR: Unsupported version ${RACKET_VERSION}"
    exit 1
fi
    
############################################################
# Some tests

if [ -n "$DRY_RUN" ]; then
    printf "RACKET_MINIMAL\t%s\n" $RACKET_MINIMAL
    printf "RACKET_VERSION\t%s\n" $RACKET_VERSION
    printf "           URL\t%s\n" $URL
fi


echo "Checking installer"
LOGS="https://www.cs.utah.edu/plt/snapshots/current/log/"
if  curl -I -L $URL 2>&1 | grep 404.Not.Found ; then
    echo "Installer not available"
    if [[ "$RACKET_VERSION" = "HEAD" ]]; then
        echo "Did the build fail? Check the logs at ${LOGS}"
    fi
    exit 1
fi


if [ -n "$DRY_RUN" ]; then
    echo "DRY_RUN finished OK"
    exit 0
fi

############################################################
# Actually get the installer

echo "Downloading $URL to $INSTALLER:"
curl -L -o $INSTALLER $URL

exit 0
