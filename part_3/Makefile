#########################################################
# make variables

# directories
d_src     = src
d_bin     = $(d_src)/bin
d_modules = $(d_src)/modules
d_test    = $(d_src)/test_files

# source file name without extension
grammar = grammar
parser = Parser
lexer = LexGrammar
errors = ErrM
main = Main

# (s)ource files
parser_y  			= $(d_modules)/$(parser).y
grammar_cf    		= $(d_modules)/$(grammar).cf
src_modules_hs 		= $(wildcard $(d_modules)/*.hs)
# (o)utput files generated
lexer_x				= $(d_bin)/$(lexer).x
parser_hs  			= $(d_bin)/$(parser).hs
parser_info    		= $(d_bin)/$(parser).info
lexer_hs 			= $(d_bin)/$(lexer).hs
err_hs     			= $(d_bin)/$(errors).hs
# all *.hs files to import
all_modules  		= $(src_modules_hs) $(parser_hs) $(lexer_hs) $(err_hs)
# output main file
out_main   			= $(d_bin)/$(main)

#########################################################
# make rules

grammar:
	bnfc -m --outputdir=$(d_bin) $(grammar_cf)

lexer: grammar
	alex $(lexer_x)

parser:
	happy --info=$(parser_info) --outfile=$(parser_hs) $(parser_y)

ghc: lexer parser
	ghc -outputdir $(d_bin) -o $(out_main) $(all_modules)
	echo "binary file:" $(out_main)

ghci: lexer parser
	ghci $(all_modules)

clean:
	rm -r $(d_bin)/*
	rm -r $(d_test)/temp/*

#manual compilation:
#ghci src/modules/Main.hs src/modules/parser_hs.hs src/bin/LexGrammar.hs src/modules/ErrorMessage.hs src/modules/AbstractSyntax.hs src/modules/Types.hs src/bin/ErrM.hs src/modules/PrettyPrinter.hs

#TEST_FILE = 1
#test: 
#	./$(d_bin)/TestGrammar $(d_test)/test_$(TEST_FILE).pas

