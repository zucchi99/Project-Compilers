-- to compile this file
-- make test-main

module Main where

-- import qualified PrettyPrinter
import qualified Parser             as Par
import qualified LexGrammar         as Lex
import qualified PrettyPrinter      as Printer
import qualified StaticSemantic     as Static
import System.Directory
import Data.List
import ErrM
import TAC

-- Displays the compilation errors on the screen
-- Displays the file location where it writes all the compilation details on the screen ()
testami test = do

    let out_dir = "src/test_files/temp/"
    let pretty_out_file = out_dir ++ "pretty_print_" ++ test ++ ".pas"
    let static_out_file = out_dir ++ "pretty_print_" ++ test ++ ".static"
    createDirectoryIfMissing True out_dir

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

    -- PrettyPrinter to file
    putStrLn "checking if output of pretty printer is correctly lexed and parser..."
    writeFile pretty_out_file pretty

    -- Repeat for test PrettyPrinter
    putStrLn "Repeat for test PrettyPrinter"
    input <- readFile pretty_out_file
    let output = Par.pProgram $ Lex.tokens input
    putStrLn $ Printer.serializer output

    -- Static Semantic
    let static = Static.static_semantic_check par
    putStrLn "\nStatic Semantic errors:"
    putStrLn $ Static.static_semantic_errors static
    putStrLn "Static Semantic data structure:"
    putStr $ show $ static
    putStr $ Printer.pretty_print_ast static "ident"

    -- Static Semantic Debug
    let static_debug = Static.static_semantic_debug par
    writeFile static_out_file $ Printer.pretty_print_ast_debug static_debug "ident"

    -- TAC Generation
    putStrLn "TAC Generation"
    let just_tac = case static of
            (ErrM.Ok program) -> Just $ TAC.generate_tac program
            (ErrM.Bad err)    -> Nothing
    
    putStr $ case just_tac of
        Nothing   -> ""
        Just tac  -> TAC.pretty_printer_tac tac

    putStrLn "end"


testami_2 test = do
    -- test single file
    let out_dir = "src/test_files/temp/" ++ test ++ "/"
    putStrLn $ "----------------------------------------------------------------------------------------"
    putStrLn $ "test file: "  ++ test
    putStrLn $ "output dir: " ++ out_dir

    createDirectoryIfMissing True out_dir
    let parsing_out_file = out_dir ++ test ++ ".parsing"
    let pretty_out_file  = out_dir ++ test ++ ".pas"
    let static_out_file  = out_dir ++ test ++ ".static"
    let tac_out_file     = out_dir ++ test ++ ".tac"

    -- Read file
    input <- readFile $ concat ["src/test_files/test_", test, ".pas"]

    -- Parsing & Lexing
    let parsed = Par.pProgram $ Lex.tokens input
    writeFile parsing_out_file ("Parsing & Lexing \n\n" ++ Printer.pretty_print_ast parsed "ident" ++ "\n\n")
    
    -- PrettyPrinter
    writeFile pretty_out_file ("Pretty Printer \n\n" ++ Printer.serializer parsed ++ "\n\n")

    -- Static Semantic
    let static = Static.static_semantic_check parsed
    writeFile static_out_file ("Static Semantic errors: \n\n" ++ Printer.pretty_print_ast static "ident" ++ "\n\n")
    putStrLn $ Static.static_semantic_errors static

    -- TAC
    let just_tac = case static of
            (ErrM.Ok program) -> Just $ TAC.generate_tac program
            (ErrM.Bad err)    -> Nothing
    
    writeFile tac_out_file $ case just_tac of
        Nothing   -> "TAC not generated, found error before.\n"
        Just tac  -> TAC.pretty_printer_tac tac


main = do
    -- test all files
    let test_files_dir = "src/test_files/"
    all_files <- getDirectoryContents test_files_dir
    let test_files = filter (isPrefixOf "test_") all_files
    let test_names = map ( \ f -> reverse $ drop 4 $ reverse $ drop 5 f ) test_files
    mapM ( \ test -> testami_2 test ) test_names
    putStr ""