#bin directory
OBJDIR = bin
IN_DIR = test_files

# lex objects
LEX_OBJ = $(OBJDIR)/lex.yy.c 

# bison objects :
# 1) output file always generated
BIS_CPP_OBJ = $(OBJDIR)/Bison.tab.cpp 
# 2) option -d
BIS_HPP_OBJ = $(OBJDIR)/Bison.tab.hpp
# 3) option -v
# BIS_OUT_OBJ = $(OBJDIR)/Bison.output

# g++ main file
MAIN = $(OBJDIR)/main.out

# mkdir if not exists
$(OBJDIR):
	@ test -d $(OBJDIR) || mkdir $(OBJDIR)

#@ bison -t -v -d Bison.ypp
compile: $(OBJDIR) 
	@ flex -o $(LEX_OBJ) Flex.l
	@ bison -o $(BIS_CPP_OBJ) -d Bison.ypp
	@ g++ -o $(MAIN) $(BIS_CPP_OBJ) $(LEX_OBJ)

run_test: compile
	@ $(MAIN) < $(IN_DIR)/example.conf

clear:
	@ rm $(OBJDIR)/*

prova: $(OBJDIR)
	@ g++ -o $(OBJDIR)/prova.out prova.cpp

list:
	@ grep '^[^#[:space:]].*:' Makefile | sed s/:.*//g | grep -v \\$