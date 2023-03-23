TEST_FILE = 1

compile-grammar:
	bnfc -m --outputdir=src/bin src/modules/grammar.cf
	(cd src/bin && make all)

test: 
	./src/bin/TestGrammar src/test/test_$(TEST_FILE).pas

clean:
	(cd src/bin && rm -r *)