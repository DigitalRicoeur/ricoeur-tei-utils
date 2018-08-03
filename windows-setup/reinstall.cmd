:: This batch file sets up major changes to the tei-utils repository.

:: Do not print command names.
ECHO OFF

ECHO This batch file sets up major changes to the tei-utils repository
ECHO by uninstalling it and installing it again from scratch.
ECHO It is normal to see warnings after the uninstall step.
ECHO This batch file does not handle pulling updates to the git repository.
ECHO Press enter to begin.
PAUSE
cd ..
"C:\Program Files\Racket\raco.exe" pkg remove --force ricoeur-tei-utils
"C:\Program Files\Racket\raco.exe" pkg install --name ricoeur-tei-utils
ECHO Press enter to close this window.
PAUSE

