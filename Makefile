.PHONY: setup
setup:
	git pull --ff-only
	raco setup --doc-index ricoeur

.PHONY: fast
fast:
	raco setup --no-docs --no-pkg-deps ricoeur/lib ricoeur/tei

.PHONY: install
install:
	raco pkg install -i

.PHONY: reinstall
reinstall:
	git pull --ff-only
	-raco pkg remove --force tei-utils
	raco pkg install -i



.PHONY: gui-icons
gui-icons: tei-lint-icons migration-assistant-icons

.PHONY: migration-assistant-icons
migration-assistant-icons: \
  tei/tools/migration/migration-assistant.png \
  tei/tools/migration/migration-assistant.icns \
  tei/tools/migration/migration-assistant.ico

tei/tools/migration/%png tei/tools/migration/%icns tei/tools/migration/%ico: \
  tei/tools/migration/icon.rkt
	racket -l ricoeur/tei/tools/migration/icon.rkt

.PHONY: tei-lint-icons
tei-lint-icons: \
  tei/tools/tei-lint/tei-lint.icns \
  tei/tools/tei-lint/tei-lint.ico

tei/tools/tei-lint/%icns tei/tools/tei-lint/%ico: \
  tei/tools/tei-lint/tei-lint.png tei/tools/tei-lint/convert-icon.rkt
	racket -l ricoeur/tei/tools/tei-lint/convert-icon.rkt
