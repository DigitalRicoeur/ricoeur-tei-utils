#lang scribble/manual

@title{Installing & Updating This Library}

@(require (for-label ricoeur/lib))

@margin-note{Installing this library will also install
 the tools documented under
 @secref["Tools"
         #:doc '(lib "ricoeur/tei/scribblings/guidelines.scrbl")].}
            
To use this library, you must install the Racket programming
language and runtime system for your platform from
@url["https://racket-lang.org"]. Racket version 6.11 or later
is currently required. If you use Mac OS, you are free to use the 
"cask" for the Homebrew packacge manager (but note that the
"minimal-racket" Homebrew formula is currently unmaintained and
should be avoided), and, if you use Ubuntu, you may wish to
consider the Racket
@hyperlink["https://launchpad.net/~plt/+archive/ubuntu/racket"]{PPA}.
Otherwise, you generally should @italic{not} use the version
of Racket from your OS's package manager,
as it will generally not be up-to-date.

You should also configure your @envvar{PATH} environment variable
so that the @tt{racket} and @tt{raco} programs can be run
from the command line. For example, on Mac OS, you should add a
line like the following to @filepath{~/.bash_profile}:
@verbatim[#:indent 2]{export PATH="/Applications/Racket v6.11/bin":$PATH}

While it is not strictly required, some features of this library
are implemented using the utility @tt{xmllint} from @tt{libxml2}.
This is included by default with Mac OS and is available via
the system package manager on GNU/Linux.
On Windows, the necessary binaries are provided as a platform-specific
dependency through the Racket package system.
@margin-note{Specifically, binaries are provided for platforms where
 @racket[(matching-platform? "win32\\x86_64")] returns @racket[#t].}

To install this library, you must first obtain a copy of the source
code by cloning its git repository from
@url["https://bitbucket.org/digitalricoeur/tei-utils"].
You then must install it as a Racket package.
Two methods are provided to streamline this process:
@(itemlist
  @item{@bold{Using @tt{make}}:
 On platforms which provide the utility @tt{make},
 this package can be installed by running
 @exec{make install} from the directory into which you have cloned
 the repository.

 Later, you can install updated versions of the repository simply
 by running @exec{make}, which also handles pulling updates from
 the server for you. More substantial changes may occasionally
 require you to reinstall the package by running
 @exec{make reinstall}.
}
  @item{@bold{Windows batch files}:
 For Windows users, batch files are included in the
 @filepath{windows-setup} directory of this repository.
 The details are explained in @filepath{windows-setup/README.md}.

 These files are still somewhat experimental.
 Please report any problems.
 })

If you are developing (as oposed to merely using) this library,
you may also wish to install the Racket package
@hyperlink["https://pkgs.racket-lang.org/package/todo-list"]{todo-list},
which provides a DrRacket plugin that cooperates with the
@racket[TODO] macro.


