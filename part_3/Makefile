TEST_FILE = 1

compile-grammar:
	bnfc -m --outputdir=src/bin src/modules/grammar.cf
	(cd src/bin && make all)

test: 
	./src/bin/TestGrammar src/test/test_$(TEST_FILE).pas

clean:
	(cd src/bin && rm -r *)

compile-main:
	ghc -outputdir src/bin/ -o src/bin/Main src/modules/Main.hs src/modules/PrettyPrinter.hs src/bin/ParGrammar.hs src/bin/LexGrammar.hs src/bin/PrintGrammar.hs src/bin/ErrM.hs src/bin/AbsGrammar.hs
	./src/bin/Main

test-main:
	happy src/modules/Parser.y
	ghci src/modules/Main.hs src/modules/Parser.hs src/bin/LexGrammar.hs src/modules/Errors.hs src/modules/AbstractSyntax.hs src/modules/TypeSys.hs src/bin/ErrM.hs