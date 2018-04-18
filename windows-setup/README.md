# Windows Setup Tools #

This directory contains batch files to help Windows users to install
the tei-utils repository and to set up updated versions.

Unlike the Makefile for Mac OS and GNU/Linux users, these batch files
do not handle pulling updates to the git repository automatically.

All of these batch files assume that you have `raco` (the Racket
command-line tool) installed in the default location:
'C:\Program Files\Racket\raco.exe'

## Batch Files: ##

- __install.cmd__ Run this to install tei-utils for the first time
  or after you have installed an upgraded version of Racket.
  
- __setup.cmd__ Run this to set up minor updates to tei-utils.

- __reinstall.cmd__ Run this to set up major updates to tei-utils.

