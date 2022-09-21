compile:
	@ flex Flex.l
	@ bison -t -v -d Bison.ypp
	@ g++ -o main Bison.tab.cpp lex.yy.c

test:
	./main < example.conf

test-value:
	@ g++ Value.hpp -o value_main
	@ ./value_main

clean:
	@ rm Bison.output Bison.tab.cpp Bison.tab.hpp lex.yy.c main

dirt:
	@ rm Bison.output Bison.tab.cpp Bison.tab.hpp lex.yy.c

compile_prova:
	@ g++ -o prova prova.cpp

test_prova:	
	@ ./prova

clean_prova:
	@ rm prova

list:
	@grep '^[^#[:space:]].*:' Makefile