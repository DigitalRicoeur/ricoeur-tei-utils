:: Use this batch file to install tei-utils repository, 
:: either for the first time or after upgrading to a new Racket version.

:: Do not print command names.
ECHO OFF

ECHO Use this batch file to install tei-utils repository,
ECHO either for the first time or after upgrading to a new Racket version.
ECHO If the repository is already installed, you will see an error message.
ECHO Press enter to begin.
PAUSE
cd ..
'C:\Program Files\Racket\raco.exe' pkg install -i
ECHO Press enter to close this window.
PAUSE
