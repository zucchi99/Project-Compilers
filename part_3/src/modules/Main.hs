import qualified PrettyPrinter
import qualified ParGrammar         as Par
import qualified LexGrammar         as Lex

main = do

    putStrLn "Reading file... \n"

    input <- readFile "src/test/test_for.pas"

    putStr input

    putStrLn "Lexing and Parsing... \n"

    let output = Par.pProgram $ Lex.tokens input

    putStrLn "OK on abstract sintax: \n"
    putStrLn $ show output

    PrettyPrinter.pprint output

