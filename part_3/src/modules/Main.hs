import qualified PrettyPrinter
import qualified ParGrammar         as Par
import qualified LexGrammar         as Lex

testami test = do

    input <- readFile $ concat ["src/test/test_", test, ".pas"]

    let output = Par.pProgram $ Lex.tokens input

    putStrLn ""
    
    PrettyPrinter.printAst output

    putStrLn ""

    putStrLn $ PrettyPrinter.prettyprinter output

    -- print output in a file
    writeFile (concat ["src/test/test_prettytest.pas"]) $ PrettyPrinter.prettyprinter output

    input <- readFile $ concat ["src/test/test_prettytest.pas"]
    let output = Par.pProgram $ Lex.tokens input
    putStrLn $ PrettyPrinter.prettyprinter output
    putStrLn "OK"

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

