-- to compile this file
-- ghci src/modules/Main.hs src/modules/PrettyPrinter.hs src/modules/ParGrammar.hs src/bin/LexGrammar.hs src/bin/PrintGrammar.hs src/bin/ErrM.hs src/bin/AbsGrammar.hs src/modules/AbstractSyntax.hs

-- import qualified PrettyPrinter
import qualified ParGrammar         as Par
import qualified LexGrammar         as Lex
-- import Giusti

testami test = do

    -- Read file
    input <- readFile $ concat ["src/test/test_", test, ".pas"]
    
    -- Lexing
    let lex = Lex.tokens input
    putStrLn ""
    putStrLn $ show lex

    -- Parsing
    let par = Par.pProgram lex
    putStrLn ""
    putStrLn $ show par
    
    -- PrettyPrinter
    -- let pretty = PrettyPrinter.printAst par
    -- putStrLn ""
    -- putStrLn pretty

    -- PrettyPrinter to file
    -- writeFile (concat ["src/test/test_prettytest.pas"]) pretty

    -- Repeat for test PrettyPrinter
    -- input <- readFile $ concat ["src/test/test_prettytest.pas"]
    -- let output = Par.pProgram $ Lex.tokens input
    -- putStrLn $ PrettyPrinter.prettyprinter output
    -- putStrLn "OK"

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

