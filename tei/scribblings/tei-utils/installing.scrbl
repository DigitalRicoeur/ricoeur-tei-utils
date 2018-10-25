#lang scribble/manual

@title{Installing & Updating This Library}

@(require "for-manual.rkt"
          racket/runtime-path)

@(define min-racket-version "7.0")

@margin-note{Installing this library will also install
 the tools documented under
 @guidelines-secref["Tools"].}
            
To use this library, you must install the Racket programming
language and runtime system for your platform from
@url["https://download.racket-lang.org"].
Racket version @min-racket-version or later is currently required.
You should @italic{not} use the version
of Racket from your operating system's package manager,
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

After you have installed Racket, you then must install this
library as a Racket package.
For most users, we recommend the following simple method
that doesn't require knowing about Git or the command line.
For more advanced users and those wishing to contribute to the
development of this library, an alternative process is given under
@secref["advanced-install"].

@(define-runtime-path pkg-manager-install.png
   "pkg-manager-install.png")
@(define-runtime-path pkg-manager-update.png
   "pkg-manager-update.png")

@itemlist[
 #:style 'ordered
 @item{Open the program DrRacket, which you installed as part of
  the Racket distribution.}
 @item{Choose @menuitem["File" "Package Manager…"], which will open
  a new window labeled @onscreen{Package Manager}.}
 @item{In the field labeled @onscreen{Package Source}, enter the
  exact text @onscreen{ricoeur-tei-utils}.
  Press the @onscreen{Install} button.
  @image[pkg-manager-install.png
         #:style style:scale-down-to-fit]{
   Screenshot with @onscreen{Install} button}}
 @item{Later, to install updated versions of this library,
  repeat the above steps.
  Instead of an @onscreen{Install} button, there will be an
  @onscreen{Update} button, which will download and install any updates.
  @image[pkg-manager-update.png
         #:style style:scale-down-to-fit]{
   Screenshot with @onscreen{Update} button}}
 ]

@section[#:tag "advanced-install"]{Additional Installation Details}

While it is not strictly required, some features of this library
are implemented using the utility @exec{xmllint} from @tt{libxml2}.
This is included by default with Mac OS and is available via
the system package manager on GNU/Linux.
On Windows, the necessary binaries are provided as a platform-specific
dependency through the Racket package system.
@margin-note{Specifically, binaries are provided for platforms where
 @racket[(matching-platform? "win32\\x86_64")] returns @racket[#t].}

To use the command-line utilities bundled with this library,
you should also configure your @envvar{PATH} environment variable
so that the @exec{racket} and @exec{raco} programs can be run.
For example, on Mac OS, you could run the following command:
@verbatim[#:indent 2]{sudo echo "/Applications/Racket v@|min-racket-version|/bin" > /etc/paths.d/racket}

If you plan to develop (as opposed to merely use) this library,
you probably do not want to follow the GUI installation instructions.
Instead, start by cloning the Git repository for this library from
@url["https://bitbucket.org/digitalricoeur/tei-utils"].
You can then link your local clone as a Racket package to take advantage
of the support described under @secref["git-workflow" #:doc '(lib "pkg/scribblings/pkg.scrbl")]
in @other-doc['(lib "pkg/scribblings/pkg.scrbl")].
As a convienience, the repository includes a makefile which
provides @tt{make} targets for most common tasks:
@itemlist[
 @item{@exec{make install}: Installs the package. This is needed only
  when you install a new version of Racket.}
 @item{@exec{make update}: Checks for updates to the package and tries
  to install them, which may involve updating this package's dependencies.
  ``Updates'' are changes to the package source listed
  in the Racket package catalog: currently, that is the ``master'' branch
  of the canonical Git repository.
  Updates are pulled with the equivalent of @exec{git pull --ff-only}
  so they won't overwrite any local changes.}
 @item{@exec{make setup}: Builds compiled files, executables, documentation,
  @etc for this library, as needed.
  Unlike @exec{make update}, @exec{make setup} doesn't look for upstream
  updates: use @exec{make setup} when you want to explicitly build your
  local changes.}
 ]

@;{
 @; This is currently a build-dep.
 If you are developing (as oposed to merely using) this library,
 you may also wish to install the Racket package
 @hyperlink["https://pkgs.racket-lang.org/package/todo-list"]{todo-list},
 which provides a DrRacket plugin that cooperates with the
 @racket[TODO] macro.
}




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

