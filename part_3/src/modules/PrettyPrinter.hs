-- Module for serialize a Abstract Sintax Tree
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter where

import AbstractSyntax
import Types
import ErrM
import Data.Maybe
import Env

-- __________________________ AUXILIAR CLASSES AND FUNCTIONS

-- print the block declarations (already is the last parameter)
-- already = "" -> no var or const was printed before
-- already = "var" -> a var was printed before
-- already = "const" -> a const was printed before
prettyPrinterDeclaration :: Bool -> Int -> [Declaration] -> String -> String
prettyPrinterDeclaration _ _ [] _ = ""
prettyPrinterDeclaration newline numtabs (DeclarationCostant id maybe_type value pos env errors : declarations) "const" =
    pretty_printer True (numtabs + 1) (DeclarationCostant id maybe_type value pos env errors)             ++
    ";"                                                                                                     ++
    prettyPrinterDeclaration newline numtabs declarations "const"
prettyPrinterDeclaration newline numtabs (DeclarationCostant id maybe_type value pos env errors : declarations) _ =
    ident True numtabs "const"                                                                              ++
    pretty_printer True (numtabs + 1) (DeclarationCostant id maybe_type value pos env errors)             ++
    ";"                                                                                                     ++
    prettyPrinterDeclaration newline numtabs declarations "const"
prettyPrinterDeclaration newline numtabs (DeclarationVariable id var_type value_maybe param_maybe pos env errors : declarations) "var" =
    pretty_printer True (numtabs + 1) (DeclarationVariable id var_type value_maybe param_maybe pos env errors)        ++
    ";"                                                                                                     ++
    prettyPrinterDeclaration newline numtabs declarations "var"
prettyPrinterDeclaration newline numtabs (DeclarationVariable id var_type value_maybe param_maybe pos env errors : declarations) _ =
    ident True numtabs "var"                                                                                ++
    pretty_printer True (numtabs + 1) (DeclarationVariable id var_type value_maybe param_maybe pos env errors)        ++
    ";"                                                                                                     ++
    prettyPrinterDeclaration newline numtabs declarations "var"
prettyPrinterDeclaration newline numtabs (declaration : declarations) _ =
    pretty_printer True numtabs declaration                                                               ++
    prettyPrinterDeclaration newline numtabs declarations ""

-- Ident a string
-- INPUT:   newline: a boolean that says if the string is a new line
--          numtabs: the number of tabs to ident the string 
--          str: the string to ident
ident :: Bool -> Int -> String -> String
ident True numtabs str = "\n" ++ concat (replicate numtabs "    ") ++ str ++ " "
ident _ _ str = str ++ " "

-- concat of operations in a string
-- stringPrettyConcat :: PrettyPrinterClass a => Bool -> Int -> [a] -> [Char] -> [Char] -> Bool -> [Char]  -- uncomment this line generate an error at line 55 because a is ambiguous 
stringPrettyConcat newline numtabs [] sep concatType firstnewline   | concatType == "separator" = ""
                                                                    | concatType == "terminator" = sep
stringPrettyConcat newline numtabs (x:[]) sep concatType firstnewline = 
    pretty_printer firstnewline numtabs x                           ++
    stringPrettyConcat newline numtabs [] sep concatType newline
stringPrettyConcat newline numtabs (x:xs) sep concatType firstnewline = 
    pretty_printer firstnewline numtabs x                           ++
    sep                                                             ++
    stringPrettyConcat newline numtabs xs sep concatType newline

pretty_printer_binary :: PrettyPrinterClass a => Int -> a -> a -> String -> String
pretty_printer_binary numtabs sx dx op = "( " ++ pretty_printer False numtabs sx ++ op ++ pretty_printer False numtabs dx ++ ") "

pretty_printer_unary :: PrettyPrinterClass a => Int -> a -> String -> String
pretty_printer_unary numtabs dx op = "( " ++ op ++ pretty_printer False numtabs dx ++ ") "

-- pretty printer di una srtinga, per identare correttamente e andare a capo in occorrenza di parentesi
pretty_printer_naive :: String -> String
pretty_printer_naive text = pretty_printer_naive_aux text 1 where
    pretty_printer_naive_aux ""       _ = "\n\n"
    pretty_printer_naive_aux (c:xs) n | (c == '{') = (c : '\n' : (replicate (n+1) '\t'))    ++ (pretty_printer_naive_aux xs (n+1))
    pretty_printer_naive_aux (c:xs) n | (c == '}') = ('\n' : (replicate (n-1) '\t') ++ [c]) ++ (pretty_printer_naive_aux xs (n-1))
    pretty_printer_naive_aux (x:xs) n = (x : (pretty_printer_naive_aux xs n))

-- __________________________ PRETTY PRINTER ABSTRACT SINTAX TREE FOR DEBUG
-- pretty_print_ast :: Show a => Err a -> String -> String
pretty_print_ast x ident = case x of
    (ErrM.Ok a)     | ident == "ident"  -> pretty_printer_naive $ show a
                    | ident == "inline" -> show a
    (ErrM.Bad err) -> err

pretty_print_ast_debug x ident = case ident of
    "ident"     -> pretty_printer_naive $ show x
    "inline"    -> show x

-- __________________________ PRETTY PRINTER ABSTRACT SINTAX TREE FOR REGENERATE THE CODE
serializer :: PrettyPrinterClass a => Err a -> String
serializer x = case x of
    -- parse successful
    (ErrM.Ok a)    -> pretty_printer True 0 a
    -- parse error
    (ErrM.Bad err) -> err

-- __________________________ PRETTY PRINTER CLASS & INSTANCES
class PrettyPrinterClass a where
    -- this way we can define the same function to serialize each type of the abstract sintax tree
    pretty_printer :: Bool -> Int -> a -> String

instance PrettyPrinterClass Program where
    pretty_printer newline numtabs (ProgramStart name block pos env errors) =
        ident False 0 "program"                                             ++
        pretty_printer False 0 name                                       ++ 
        ";"                                                                 ++
        pretty_printer True 0 block                                       ++ 
        "."

instance PrettyPrinterClass Ident where
    pretty_printer newline numtabs (Ident name _ _ _) = 
        ident newline numtabs name

instance PrettyPrinterClass [Ident] where
    pretty_printer newline numtabs idents = 
        stringPrettyConcat False numtabs idents ", " "separator" newline

instance PrettyPrinterClass Block where
    pretty_printer newline numtabs (Block decls stmts pos env errors) =
        pretty_printer newline numtabs decls                              ++
        ident True numtabs "begin"                                          ++
        pretty_printer True (numtabs + 1) stmts                           ++
        ident True numtabs "end"

instance PrettyPrinterClass Declaration where
    pretty_printer newline numtabs (DeclarationCostant id maybe_type value pos env errors) =
        pretty_printer newline numtabs id                                 ++
        ident False numtabs "= "                                            ++
        pretty_printer False numtabs value
    pretty_printer newline numtabs(DeclarationVariable id var_type value_maybe param_maybe pos env errors) =
        let (before, t) = case param_maybe of
                Just Reference  -> ("var", pType var_type)
                _               -> ("", var_type)
        in ident newline numtabs before                                         ++
            pretty_printer False numtabs id                                 ++
            ident False numtabs ":"                                             ++
            pretty_printer False numtabs t                             ++
            if isNothing value_maybe then "" else 
                ident False numtabs "= "                                        ++
                pretty_printer False numtabs (fromJust value_maybe)
    pretty_printer newline numtabs (DeclarationFunction id params fun_type maybe_block pos env errors) =
        ident newline numtabs "function"                                    ++
        pretty_printer False numtabs id                                 ++
        ident False numtabs "( "                                            ++
        stringPrettyConcat False numtabs params "; " "separator" False      ++
        ident False numtabs ") :"                                           ++
        pretty_printer False numtabs fun_type                                 ++
        ident False numtabs "; "                                            ++
        if isNothing maybe_block then "forward ;" else 
            pretty_printer False (numtabs + 1) (fromJust maybe_block)                  ++
            ident False numtabs ";"
    pretty_printer newline numtabs (DeclarationProcedure id params maybe_block pos env errors) =
        ident newline numtabs "procedure"                                   ++
        pretty_printer False numtabs id                                 ++
        ident False numtabs "( "                                            ++
        stringPrettyConcat False numtabs params "; " "separator" False      ++
        ident False numtabs ") ; "                                          ++
        if isNothing maybe_block then "forward ;" else 
            pretty_printer False (numtabs + 1) (fromJust maybe_block)                  ++
            ident False numtabs ";"

instance PrettyPrinterClass [Declaration] where
    pretty_printer newline numtabs blocks =
        prettyPrinterDeclaration newline numtabs blocks ""

instance PrettyPrinterClass Statement where
    pretty_printer newline numtabs (StatementBlock block pos env errors) =
        pretty_printer newline numtabs block
    pretty_printer newline numtabs (StatementIf cond then_body maybe_else_body pos env errors) =
        ident newline numtabs "if"                                          ++
        pretty_printer False (numtabs + 1) cond                                 ++
        "then "                                                           ++
        pretty_printer newline (numtabs + 1) then_body                    ++      
        if isNothing maybe_else_body then "" else 
            pretty_printer newline (numtabs + 1) (fromJust maybe_else_body)
    pretty_printer newline numtabs (StatementFor cond then_body for_var pos env errors) =
        let cond_type = case cond of
                RightExpLessEqual _ _ _ _ _ _       -> "to"
                RightExpGreaterEqual _ _ _ _ _ _    -> "downto"
                _                                   -> "not_yet_implemented"
        in  ident newline numtabs "for"                                         ++
            pretty_printer False numtabs for_var                              ++
            ident False numtabs cond_type                                            ++
            pretty_printer False numtabs (dx cond)                                 ++
            ident False numtabs "do"                                            ++
            pretty_printer newline (numtabs + 1) then_body
    pretty_printer newline numtabs (StatementWhile cond then_body pos env errors) =
        ident newline numtabs "while"                                       ++
        pretty_printer newline numtabs cond                               ++
        ident False numtabs "do"                                            ++
        pretty_printer newline (numtabs + 1) then_body
    pretty_printer newline numtabs (StatementRepeatUntil cond then_body pos env errors) =
        ident newline numtabs "repeat"                                      ++
        pretty_printer newline (numtabs + 1) then_body                    ++
        ident newline numtabs "until"                                       ++
        pretty_printer False numtabs cond
    pretty_printer newline numtabs (StatementAssign assign pos env errors) =
        pretty_printer newline numtabs assign
    pretty_printer newline numtabs (StatementFuncProcCall id params pos env errors) =
        pretty_printer newline numtabs id                               ++
        "("                                                                 ++
        pretty_printer False numtabs params                               ++
        ")"
    pretty_printer newline numtabs (StatementWrite write_primitive pos env errors) =
        pretty_printer newline numtabs write_primitive
    pretty_printer newline numtabs (StatementRead read_primitive pos env errors) =
        pretty_printer newline numtabs read_primitive
    pretty_printer newline numtabs (StatementContinue pos env errors) =
        ident newline numtabs "continue"
    pretty_printer newline numtabs (StatementBreak pos env errors) =
        ident newline numtabs "break"

instance PrettyPrinterClass [Statement] where
    pretty_printer newline numtabs statements =
        stringPrettyConcat True numtabs statements "; " "separator" newline

instance PrettyPrinterClass ElseBlock where
    pretty_printer newline numtabs (ElseBlock else_body pos env errors) =
        ident newline numtabs "else"                                        ++
        pretty_printer newline (numtabs + 1) else_body 

instance PrettyPrinterClass RightExp where
    pretty_printer newline numtabs (RightExpOr sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "or "
    pretty_printer newline numtabs (RightExpAnd sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "and "
    pretty_printer newline numtabs (RightExpGreater sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "> "
    pretty_printer newline numtabs (RightExpLess sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "< "
    pretty_printer newline numtabs (RightExpGreaterEqual sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx ">= "
    pretty_printer newline numtabs (RightExpLessEqual sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "<= "
    pretty_printer newline numtabs (RightExpEqual sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "= "
    pretty_printer newline numtabs (RightExpNotEqual sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "<> "
    pretty_printer newline numtabs (RightExpPlus sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "+ "
    pretty_printer newline numtabs (RightExpMinus sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "- "
    pretty_printer newline numtabs (RightExpTimes sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "* "
    pretty_printer newline numtabs (RightExpDivide sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "/ "
    pretty_printer newline numtabs (RightExpMod sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "mod "
    pretty_printer newline numtabs (RightExpDiv sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "div "
    pretty_printer newline numtabs (RightExpPower sx dx pos ty parent_env errors) =
        pretty_printer_binary numtabs sx dx "** "
    pretty_printer newline numtabs (RightExpNot dx pos ty parent_env errors) =
        pretty_printer_unary numtabs dx "not "                        
    pretty_printer newline numtabs (RightExpMinusUnary dx pos ty parent_env errors) =
        pretty_printer_unary numtabs dx "- "                      
    pretty_printer newline numtabs (RightExpPlusUnary dx pos ty parent_env errors) =
        pretty_printer_unary numtabs dx "+ "
    pretty_printer newline numtabs (RightExpInteger dx pos ty parent_env errors) =
        ident newline numtabs $ show dx
    pretty_printer newline numtabs (RightExpReal dx pos ty parent_env errors) =
        ident newline numtabs $ show dx
    pretty_printer newline numtabs (RightExpBoolean dx pos ty parent_env errors) =
        ident newline numtabs $ show dx
    pretty_printer newline numtabs (RightExpChar dx pos ty parent_env errors) =
        ident newline numtabs $ show dx
    pretty_printer newline numtabs (RightExpString dx pos ty parent_env errors) =
        ident newline numtabs $ show dx
    pretty_printer newline numtabs (RightExpFuncProcCall id params pos ty parent_env errors) =
        pretty_printer newline numtabs id                                 ++
        "("                                                                 ++
        pretty_printer False numtabs params                               ++
        ")"
    pretty_printer newline numtabs (RightExpLeftExp left_exp pos ty parent_env errors) =
        pretty_printer newline numtabs left_exp

instance PrettyPrinterClass [RightExp] where
    pretty_printer newline numtabs expressions =
        stringPrettyConcat False numtabs expressions ", " "separator" newline

instance PrettyPrinterClass LeftExp where
    pretty_printer newline numtabs (LeftExpIdent id pos ty env errors) =
        pretty_printer newline numtabs id
    pretty_printer newline numtabs (LeftExpArrayAccess array_name array_locations lexp_type pos env errors) =
        pretty_printer newline numtabs array_name                         ++
        "["                                                                 ++
        pretty_printer False numtabs array_locations                      ++
        "]"
    pretty_printer newline numtabs (LeftExpPointerValue left_exp pos left_exp_ty env errors) =
        pretty_printer newline numtabs left_exp                           ++
        "^"
    pretty_printer newline numtabs (LeftExpPointerAddress left_exp pos left_exp_ty env errors) =
        pretty_printer newline numtabs left_exp                           ++
        "@"

instance PrettyPrinterClass Assign where
    pretty_printer newline numtabs (VariableAssignment left_exp right_exp pos env errors) =
        pretty_printer newline numtabs left_exp                           ++
        ":= "                                                               ++
        pretty_printer False numtabs right_exp  

instance PrettyPrinterClass WritePrimitive where
    pretty_printer newline numtabs (WriteInt right_exp pos env errors) =
        ident newline numtabs "writeInt ("                                  ++
        pretty_printer False numtabs right_exp                             ++
        ") "
    pretty_printer newline numtabs (WriteReal right_exp pos env errors) =
        ident newline numtabs "writeReal ("                                 ++
        pretty_printer False numtabs right_exp                             ++
        ") "
    pretty_printer newline numtabs (WriteChar right_exp pos env errors) =
        ident newline numtabs "writeChar ("                                 ++
        pretty_printer False numtabs right_exp                             ++
        ") "
    pretty_printer newline numtabs (WriteString right_exp pos env errors) =
        ident newline numtabs "writeString ("                               ++
        pretty_printer False numtabs right_exp                             ++
        ") "

instance PrettyPrinterClass ReadPrimitive where
    pretty_printer newline numtabs (ReadInt left_exp pos env errors) =
        ident newline numtabs "readInt ("                                   ++
        pretty_printer False numtabs left_exp                              ++
        ") "
    pretty_printer newline numtabs (ReadReal left_exp pos env errors) =
        ident newline numtabs "readReal ("                                  ++
        pretty_printer False numtabs left_exp                              ++
        ") "
    pretty_printer newline numtabs (ReadChar left_exp pos env errors) =
        ident newline numtabs "readChar ("                                  ++
        pretty_printer False numtabs left_exp                              ++
        ") "
    pretty_printer newline numtabs (ReadString left_exp pos env errors) =
        ident newline numtabs "readString ("                                ++
        pretty_printer False numtabs left_exp                              ++
        ") "

instance PrettyPrinterClass Type where
    pretty_printer newline numtabs (BooleanType) =
        ident newline numtabs "boolean"
    pretty_printer newline numtabs (IntegerType) =
        ident newline numtabs "integer"
    pretty_printer newline numtabs (RealType) =
        ident newline numtabs "real"
    pretty_printer newline numtabs (CharType) =
        ident newline numtabs "char"
    pretty_printer newline numtabs (StringType) =
        ident newline numtabs "string"
    pretty_printer newline numtabs (ArrayType array_type (dimension:[])) =
        ident newline numtabs "array ["                                     ++
        show (fst dimension)                                                ++
        ".."                                                                ++
        show (snd dimension)                                                       ++
        "] of "                                                             ++
        pretty_printer False numtabs array_type
    pretty_printer newline numtabs (ArrayType array_type (dimension:dimensions)) =
        ident newline numtabs "array ["                                     ++
        show (fst dimension)                                                       ++
        ".."                                                                ++
        show (snd dimension)                                                       ++
        foldl (\a (x,y) -> a ++ ", " ++ show x ++ ".." ++ show y) "" dimensions                ++
        "] of "                                                             ++
        pretty_printer False numtabs array_type
    pretty_printer newline numtabs (PointerType pointer_type) =
        ident newline numtabs "^"                                           ++
        pretty_printer False numtabs pointer_type