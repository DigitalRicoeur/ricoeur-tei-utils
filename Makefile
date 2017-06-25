setup:
	git pull --ff-only
	raco setup --doc-index ricoeur

install:
	raco pkg install -i
