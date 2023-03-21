compile-grammar:
	bnfc -m --outputdir=src/bin src/modules/grammar.cf
	(cd src/bin && make all)
	src/bin/TestGrammar src/modules/test.pas