#lang scribble/manual

@title{Installing & Updating This Library}

@(require "for-manual.rkt"
          )

@margin-note{Installing this library will also install
 the tools documented under
 @guidelines-secref["Tools"].}
            
To use this library, you must install the Racket programming
language and runtime system for your platform from
@url["https://racket-lang.org"].
Racket version 7.0 or later is currently required.
You should @italic{not} use the version
of Racket from your OS's package manager,
as it will generally not be up-to-date.
@margin-note{
 @italic{For advanced users:}
 If you use Mac OS, it is ok to use the 
 ``cask'' for the Homebrew package manager, but note that the
 ``minimal-racket'' Homebrew formula is currently unmaintained and
 should be avoided.
 If you use Ubuntu, it is ok to use the Racket
 @hyperlink["https://launchpad.net/~plt/+archive/ubuntu/racket"]{PPA}.
 However, neither of these approaches are recommended.
}

While it is not strictly required, some features of this library
are implemented using the utility @exec{xmllint} from @tt{libxml2}.
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

To use the command-line utilities bundled with this library,
you should also configure your @envvar{PATH} environment variable
so that the @exec{racket} and @exec{raco} programs can be run.
For example, on Mac OS, you could add a
line like the following to @filepath{~/.bash_profile}:
@verbatim[#:indent 2]{export PATH="/Applications/Racket v7.0/bin":$PATH}

If you are developing (as oposed to merely using) this library,
you may also wish to install the Racket package
@hyperlink["https://pkgs.racket-lang.org/package/todo-list"]{todo-list},
which provides a DrRacket plugin that cooperates with the
@racket[TODO] macro.





@section{Status of This Library}

For those not affiliated with Digital @Ricoeur, this library is available
as free software under the GNU Affero GPL.
However, there are some caveats:
@itemlist[
 #:style 'ordered
 @item{This library is @bold{not stable}.
  Digital @Ricoeur is under ongoing, iterative development.
  While we have worked to design this library so that small iterations
  will necesitate only small changes in our tools that rely on it,
  we will not hesitate to make breaking changes when our requirements
  change: indeed, we actively plan to do so.
 }
 @item{Many parts of this library have embedded assumptions that
  are very specific to the needs of our project.
  For example, our TEI XML documents are required to have an
  @tag{author} element with an @attr{xml:id} of @racket["ricoeur"].
  }]

Despite the above, we are very eager to collaborate with others!
If there is some functionality from @racketmodname[ricoeur/tei]
that you are interested in using, we encourage you to get in touch.
We would be delighted to work together on factoring out reusable
components.

