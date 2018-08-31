.PHONY: setup
setup:
	git pull --ff-only
	raco setup --doc-index ricoeur

.PHONY: fast
fast:
	raco setup --no-docs --no-pkg-deps ricoeur/tei

.PHONY: install
install:
	raco pkg install --auto --name ricoeur-tei-utils -i

.PHONY: reinstall
reinstall:
	git pull --ff-only
	-raco pkg remove --force ricoeur-tei-utils
	raco pkg install --auto --name ricoeur-tei-utils -i


.PHONY: gui-icons
gui-icons: tei-lint-icons 

.PHONY: tei-lint-icons
tei-lint-icons: \
  tei/tools/tei-lint/tei-lint.icns \
  tei/tools/tei-lint/tei-lint.ico

tei/tools/tei-lint/%icns tei/tools/tei-lint/%ico: \
  tei/tools/tei-lint/tei-lint.png tei/tools/tei-lint/convert-icon.rkt
	racket -l ricoeur/tei/tools/tei-lint/convert-icon.rkt


