setup:
	git pull --ff-only
	raco setup --doc-index ricoeur

install:
	raco pkg install -i

reinstall:
	git pull --ff-only
	-raco pkg remove --force tei-utils
	raco pkg install -i
