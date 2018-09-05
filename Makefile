.PHONY: all
PKGNAME = ricoeur-tei-utils
all: setup


.PHONY: setup
setup:
	git pull --ff-only
	raco setup --doc-index ricoeur


.PHONY: fast
fast:
	raco setup --no-docs --no-pkg-deps ricoeur/tei


.PHONY: install
INSTALL = raco pkg install --auto --name $(PKGNAME) -i
install:
	$(INSTALL)


.PHONY: reinstall
reinstall:
	git pull --ff-only
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


