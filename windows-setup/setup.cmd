:: This batch file sets up minor changes to the tei-utils repository.

:: Do not print command names.
ECHO OFF

ECHO This batch file sets up minor changes to the tei-utils repository.
ECHO It does not handle pulling updates to the git repository.
ECHO Press enter to begin.
PAUSE
cd ..
"C:\Program Files\Racket\raco.exe" setup --doc-index ricoeur
ECHO Press enter to close this window.
PAUSE
