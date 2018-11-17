.PHONY: all
PKGNAME = ricoeur-tei-utils
all:
	echo "Please specify a target," \
	"e.g. \"install\", \"update\", or \"setup\"."
	exit 1


.PHONY: install
REPO = https://bitbucket.org/digitalricoeur/tei-utils
THIS_DIR = $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
INSTALL = raco pkg install -i --auto --name $(PKGNAME) \
	--clone $(THIS_DIR) $(REPO)
install:
	$(INSTALL)


.PHONY: update
update:
	raco pkg update $(PKGNAME)


.PHONY: setup
setup:
	raco setup --doc-index --pkgs $(PKGNAME)


.PHONY: fast
fast:
	raco setup --no-docs --no-pkg-deps ricoeur/tei


.PHONY: reinstall
reinstall:
	-raco pkg remove --force $(PKGNAME)
	$(INSTALL)

########################################

.PHONY: gui-icons
gui-icons: tei-lint-icons 

.PHONY: tei-lint-icons
tei-lint-icons: \
  tei/tools/tei-lint/tei-lint.icns \
  tei/tools/tei-lint/tei-lint.ico

tei/tools/tei-lint/%icns tei/tools/tei-lint/%ico: \
  tei/tools/tei-lint/tei-lint.png tei/tools/tei-lint/convert-icon.rkt
	racket -l ricoeur/tei/tools/tei-lint/convert-icon.rkt


