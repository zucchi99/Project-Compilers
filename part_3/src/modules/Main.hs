-- to compile this file
-- make test-main

import qualified PrettyPrinter
import qualified Parser             as Par
import qualified LexGrammar         as Lex
-- import Giusti


--get_tabs 0 = ""
--get_tabs n = ' ' : (get_tabs (n-1))

--print_abs text = print_abs_aux text 1
--    where
--        print_abs_aux ""       n = ""
--        print_abs_aux (c:xs) n | (c == '{' || c == '[') = (c : '\n' : (get_tabs (n+1)))    ++ (print_abs_aux xs (n+1))
--        print_abs_aux (c:xs) n | (c == '}' || c == ']') = ('\n' : (get_tabs (n-1)) ++ "}") ++ (print_abs_aux xs (n-1))
--        --print_abs_aux (',':xs) n = (",\n" ++ (get_tabs n))           ++ (print_abs_aux xs n)
--        print_abs_aux (x:xs)   n = (x : (print_abs_aux xs n))

testami test = do

    -- Read file
    input <- readFile $ concat ["src/test_files/test_", test, ".pas"]

    putStrLn "input file:"
    putStr input
    putStr "\n\n"

    -- Lexing
    let lex = Lex.tokens input
    putStrLn "lexer output:"
    putStrLn $ show lex
    putStrLn ""

    -- Parsing
    let par = Par.pProgram lex
    putStrLn "parser output:"
    -- putStr $ print_abs $ PrettyPrinter.printAst par
    putStrLn $ PrettyPrinter.printAst par
    putStrLn ""
    
    -- PrettyPrinter
    let pretty = PrettyPrinter.prettyprinter par
    putStrLn "pretty print of abstract syntax:"
    putStrLn pretty
    putStrLn ""

    let out_dir = "src/test_files/temp/"
    let out_file = out_dir ++ "pretty_print_" ++ test ++ ".pas"

    -- PrettyPrinter to file
    putStrLn "checking if output of pretty printer is correctly lexed and parser..."
    writeFile out_file pretty

    -- Repeat for test PrettyPrinter
    input <- readFile out_file
    let output = Par.pProgram $ Lex.tokens input
    -- putStrLn $ PrettyPrinter.prettyprinter output
    putStrLn "check successful!"

main = do

    putStrLn "Insert test name: \n"
    test <- getLine
    
    testami test

    -- input <- readFile "src/test/test_for.pas"

    -- putStr input

    -- putStrLn "Lexing and Parsing... \n"

    -- let output = Par.pProgram $ Lex.tokens input

    -- putStrLn "OK on abstract sintax: \n"
    -- putStrLn $ show output

    -- PrettyPrinter.pprint output

