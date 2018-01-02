setup:
	git pull --ff-only
	raco setup --doc-index ricoeur

fast:
	raco setup --no-docs --no-pkg-deps ricoeur/lib ricoeur/tei

install:
	raco pkg install -i

reinstall:
	git pull --ff-only
	-raco pkg remove --force tei-utils
	raco pkg install -i
