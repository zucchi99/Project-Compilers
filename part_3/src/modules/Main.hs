-- to compile this file
-- make test-main

module Main where

-- import qualified PrettyPrinter
import qualified Parser             as Par
import qualified LexGrammar         as Lex
import qualified PrettyPrinter      as Printer
import qualified StaticSemantic     as Static
import ErrM
import TAC

testami test = do

    -- Read file
    input <- readFile $ concat ["src/test_files/test_", test, ".pas"]

    putStrLn "input file:"
    putStr input
    putStr "\n\n"

    -- Lexing
    putStrLn "Lexing"
    let lex = Lex.tokens input
    putStrLn "lexer output:"
    putStrLn $ show lex
    putStrLn ""

    -- Parsing
    putStrLn "Parsing"
    let par = Par.pProgram lex
    putStrLn "parser output:"
    putStrLn $ Printer.pretty_print_ast par "inline"
    putStrLn ""
    
    -- PrettyPrinter
    putStrLn "PrettyPrinter"
    let pretty = Printer.serializer par
    putStrLn "pretty print of abstract syntax:"
    putStrLn pretty
    putStrLn ""

    let out_dir = "src/test_files/temp/"
    let out_file = out_dir ++ "pretty_print_" ++ test ++ ".pas"

    -- PrettyPrinter to file
    putStrLn "checking if output of pretty printer is correctly lexed and parser..."
    writeFile out_file pretty

    -- Repeat for test PrettyPrinter
    putStrLn "Repeat for test PrettyPrinter"
    input <- readFile out_file
    let output = Par.pProgram $ Lex.tokens input
    putStrLn $ Printer.serializer output

    -- Static Semantic
    let static = Static.static_semantic_check par
    putStrLn "\nStatic Semantic errors:"
    putStrLn $ Static.static_semantic_errors static
    putStrLn "Static Semantic data structure:"
    putStr $ show $ static

    -- Static Semantic Debug
    let out_file = out_dir ++ "pretty_print_" ++ test ++ ".static"
    writeFile out_file $ Printer.pretty_print_ast (ErrM.Ok static) "ident"

    -- TAC Generation
    putStrLn "TAC Generation"
    let just_tac = case static of
            (ErrM.Ok program) -> Just $ TAC.generate_tac program
            (ErrM.Bad err)    -> Nothing
    
    putStr $ case just_tac of
        Nothing   -> ""
        Just tac  -> TAC.pretty_printer_tac tac

    putStrLn "end"

main = do

    putStrLn "Insert test name: \n"
    test <- getLine
    
    testami test

