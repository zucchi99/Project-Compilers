compile-grammar:
	bnfc -m --outputdir=src/bin src/modules/grammar.cf
	(cd src/bin && make all)
	src/bin/TestGrammar src/test/test_1.pas