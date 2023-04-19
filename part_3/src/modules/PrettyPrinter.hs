-- Module for serialize a Abstract Sintax Tree
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter where

-- from a happy file get a pretty printer
import AbstractSyntax
import Types
import ErrM
import Data.Maybe

-- __________________________ AUXILIAR CLASSES AND FUNCTIONS

-- Extract the value from the monad ErrM.Ok
--fromOk (ErrM.Ok a)    = a
--fromOk (ErrM.Bad err) = err

-- print the block declarations
-- already = "" -> no var or const was printed before
-- already = "var" -> a var was printed before
-- already = "const" -> a const was printed before
prettyPrinterDeclaration _ _ [] _ = ""
prettyPrinterDeclaration newline numtabs (DeclarationCostant id maybe_type value pos env errors : declarations) "const" =
    prettyprinterAux True (numtabs + 1) (DeclarationCostant id maybe_type value pos env errors)             ++
    ";"                                                                                                     ++
    prettyPrinterDeclaration newline numtabs declarations "const"
prettyPrinterDeclaration newline numtabs (DeclarationCostant id maybe_type value pos env errors : declarations) _ =
    ident True numtabs "const"                                                                              ++
    prettyprinterAux True (numtabs + 1) (DeclarationCostant id maybe_type value pos env errors)             ++
    ";"                                                                                                     ++
    prettyPrinterDeclaration newline numtabs declarations "const"
prettyPrinterDeclaration newline numtabs (DeclarationVariable id var_type value_maybe pos env errors : declarations) "var" =
    prettyprinterAux True (numtabs + 1) (DeclarationVariable id var_type value_maybe pos env errors)        ++
    ";"                                                                                                     ++
    prettyPrinterDeclaration newline numtabs declarations "var"
prettyPrinterDeclaration newline numtabs (DeclarationVariable id var_type value_maybe pos env errors : declarations) _ =
    ident True numtabs "var"                                                                                ++
    prettyprinterAux True (numtabs + 1) (DeclarationVariable id var_type value_maybe pos env errors)        ++
    ";"                                                                                                     ++
    prettyPrinterDeclaration newline numtabs declarations "var"
prettyPrinterDeclaration newline numtabs (declaration : declarations) _ =
    prettyprinterAux True numtabs declaration                                                               ++
    prettyPrinterDeclaration newline numtabs declarations ""

-- Ident a string
-- INPUT:   newline: a boolean that says if the string is a new line
--          numtabs: the number of tabs to ident the string 
--          str: the string to ident 
ident newline numtabs str   | newline    =  "\n" ++ concat (replicate numtabs "    ") ++ str ++ " "
                            | otherwise  = str ++ " "


-- concat of operations in a string
stringPrettyConcat newline numtabs [] sep concatType firstnewline   | concatType == "separator" = ""
                                                                    | concatType == "terminator" = sep
stringPrettyConcat newline numtabs (x:[]) sep concatType firstnewline = 
    prettyprinterAux firstnewline numtabs x                                 ++
    stringPrettyConcat newline numtabs [] sep concatType newline
stringPrettyConcat newline numtabs (x:xs) sep concatType firstnewline = 
    prettyprinterAux firstnewline numtabs x                                 ++
    sep                                                                     ++
    stringPrettyConcat newline numtabs xs sep concatType newline

-- define a class for pretty printer
-- this way we can define a pretty printer for each type of the abstract sintax tree
class PrettyPrinterClass a where
    prettyprinterAux :: Bool -> Int -> a -> String

-- __________________________ PRINT THE ABSTRACT SINTAX TREE FROM HAPPY
printAst x = case x of
    (ErrM.Ok a)    -> show a
    (ErrM.Bad err) -> err

-- __________________________ PRETTY PRINTER
prettyprinter x = case x of
    -- parse successful
    (ErrM.Ok a)    -> prettyprinterAux True 0 a
    -- parse error
    (ErrM.Bad err) -> err

-- PROGRAM START (here for debugging)
instance PrettyPrinterClass Program where
    prettyprinterAux newline numtabs (ProgramStart name block pos env errors) =
        ident False 0 "program"                                             ++
        prettyprinterAux False 0 name                                       ++ 
        ";"                                                                 ++
        prettyprinterAux True 0 block                                       ++ 
        "."

instance PrettyPrinterClass Ident where
    prettyprinterAux newline numtabs (Ident name _ _ _) = 
        ident newline numtabs name

instance PrettyPrinterClass [Ident] where
    prettyprinterAux newline numtabs idents = 
        stringPrettyConcat False numtabs idents ", " "separator" newline

instance PrettyPrinterClass Block where
    prettyprinterAux newline numtabs (Block decls stmts pos env errors) =
        prettyprinterAux newline numtabs decls                              ++
        ident True numtabs "begin"                                          ++
        prettyprinterAux True (numtabs + 1) stmts                           ++
        ident True numtabs "end"

instance PrettyPrinterClass Declaration where
    prettyprinterAux newline numtabs (DeclarationCostant id maybe_type value pos env errors) =
        prettyprinterAux newline numtabs id                                 ++
        ident False numtabs "= "                                            ++
        prettyprinterAux False numtabs value
    prettyprinterAux newline numtabs(DeclarationVariable id var_type value_maybe pos env errors) =
        prettyprinterAux newline numtabs id                                 ++
        ident False numtabs ":"                                             ++
        prettyprinterAux False numtabs var_type                             ++
        if isNothing value_maybe then "" else 
            ident False numtabs "= "                                        ++
            prettyprinterAux False numtabs (fromJust value_maybe)
    prettyprinterAux newline numtabs (DeclarationFunction id params fun_type maybe_block pos env errors) =
        ident newline numtabs "function"                                    ++
        prettyprinterAux False numtabs id                                 ++
        ident False numtabs "( "                                            ++
        stringPrettyConcat False numtabs params "; " "separator" False      ++
        ident False numtabs ") :"                                           ++
        prettyprinterAux False numtabs fun_type                                 ++
        ident False numtabs "; "                                            ++
        if isNothing maybe_block then "forward ;" else 
            prettyprinterAux False numtabs (fromJust maybe_block)                  ++
            ident False numtabs ";"
    prettyprinterAux newline numtabs (DeclarationProcedure id params maybe_block pos env errors) =
        ident newline numtabs "procedure"                                   ++
        prettyprinterAux False numtabs id                                 ++
        ident False numtabs "( "                                            ++
        stringPrettyConcat False numtabs params "; " "separator" False      ++
        ident False numtabs ") ; "                                          ++
        if isNothing maybe_block then "forward ;" else 
            prettyprinterAux False numtabs (fromJust maybe_block)                  ++
            ident False numtabs ";"

instance PrettyPrinterClass [Declaration] where
    prettyprinterAux newline numtabs blocks =
        prettyPrinterDeclaration newline numtabs blocks ""

instance PrettyPrinterClass Statement where
    prettyprinterAux newline numtabs (StatementBlock block pos env errors) =
        prettyprinterAux newline numtabs block
    prettyprinterAux newline numtabs (StatementIf cond then_body maybe_else_body pos env errors) =
        ident newline numtabs "if"                                          ++
        "( "                                                                ++
        prettyprinterAux False numtabs cond                                 ++
        ") then "                                                           ++
        prettyprinterAux newline (numtabs + 1) then_body                    ++      
        if isNothing maybe_else_body then "" else 
            prettyprinterAux newline (numtabs + 1) (fromJust maybe_else_body)
    prettyprinterAux newline numtabs (StatementFor cond then_body for_var pos env errors) =
        ident newline numtabs "for"                                         ++
        prettyprinterAux False numtabs for_var                              ++
        ident False numtabs "to"                                            ++
        prettyprinterAux False numtabs cond                                 ++
        ident False numtabs "do"                                            ++
        prettyprinterAux newline (numtabs + 1) then_body
    prettyprinterAux newline numtabs (StatementWhile cond then_body pos env errors) =
        ident newline numtabs "while"                                       ++
        prettyprinterAux newline numtabs cond                               ++
        ident False numtabs "do"                                            ++
        prettyprinterAux newline (numtabs + 1) then_body
    prettyprinterAux newline numtabs (StatementRepeatUntil cond then_body pos env errors) =
        ident newline numtabs "repeat"                                      ++
        prettyprinterAux newline (numtabs + 1) then_body                    ++
        ident newline numtabs "until"                                       ++
        prettyprinterAux False numtabs cond
    prettyprinterAux newline numtabs (StatementAssign assign pos env errors) =
        prettyprinterAux newline numtabs assign
    prettyprinterAux newline numtabs (StatementFuncProcCall id params pos env errors) =
        prettyprinterAux newline numtabs id                               ++
        "("                                                                 ++
        prettyprinterAux False numtabs params                               ++
        ")"
    prettyprinterAux newline numtabs (StatementWrite write_primitive pos env errors) =
        prettyprinterAux newline numtabs write_primitive
    prettyprinterAux newline numtabs (StatementRead read_primitive pos env errors) =
        prettyprinterAux newline numtabs read_primitive
    prettyprinterAux newline numtabs (StatementContinue pos env errors) =
        ident newline numtabs "continue"
    prettyprinterAux newline numtabs (StatementBreak pos env errors) =
        ident newline numtabs "break"

instance PrettyPrinterClass [Statement] where
    prettyprinterAux newline numtabs statements =
        stringPrettyConcat True numtabs statements "; " "separator" newline

instance PrettyPrinterClass ElseBlock where
    prettyprinterAux newline numtabs (ElseBlock else_body pos env errors) =
        ident newline numtabs "else"                                        ++
        prettyprinterAux newline (numtabs + 1) else_body 

instance PrettyPrinterClass RightExp where
    prettyprinterAux newline numtabs (RightExpOr sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "or "                                                               ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpAnd sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "and "                                                              ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpGreater sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "> "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpLess sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "< "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpGreaterEqual sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        ">= "                                                               ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpLessEqual sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "<= "                                                               ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpEqual sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "= "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpPlus sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "+ "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpMinus sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "- "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpTimes sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "* "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpDivide sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "/ "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpMod sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "mod "                                                              ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpDiv sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        " div "                                                             ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpPower sx dx pos ty parent_env errors) =
        prettyprinterAux False numtabs sx                                   ++   
        "** "                                                               ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpNot dx pos ty parent_env errors) =
        "not "                                                              ++
        prettyprinterAux False numtabs dx                         
    prettyprinterAux newline numtabs (RightExpMinusUnary dx pos ty parent_env errors) =
        "- "                                                                ++
        prettyprinterAux False numtabs dx                         
    prettyprinterAux newline numtabs (RightExpPlusUnary dx pos ty parent_env errors) =
        "+ "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpInteger dx pos ty parent_env errors) =
        ident newline numtabs $ show dx
    prettyprinterAux newline numtabs (RightExpReal dx pos ty parent_env errors) =
        ident newline numtabs $ show dx
    prettyprinterAux newline numtabs (RightExpBoolean dx pos ty parent_env errors) =
        ident newline numtabs $ show dx
    prettyprinterAux newline numtabs (RightExpChar dx pos ty parent_env errors) =
        ident newline numtabs $ show dx
    prettyprinterAux newline numtabs (RightExpString dx pos ty parent_env errors) =
        ident newline numtabs $ show dx
    prettyprinterAux newline numtabs (RightExpFuncProcCall id params pos ty parent_env errors) =
        prettyprinterAux newline numtabs id                                 ++
        "("                                                                 ++
        prettyprinterAux False numtabs params                               ++
        ")"
    prettyprinterAux newline numtabs (RightExpCopy left_exp pos ty parent_env errors) =
        prettyprinterAux newline numtabs left_exp

instance PrettyPrinterClass [RightExp] where
    prettyprinterAux newline numtabs expressions =
        stringPrettyConcat False numtabs expressions ", " "separator" newline

instance PrettyPrinterClass LeftExp where
    prettyprinterAux newline numtabs (LeftExpIdent id pos ty env errors) =
        prettyprinterAux newline numtabs id
    prettyprinterAux newline numtabs (LeftExpArrayAccess array_name array_locations lexp_type pos env errors) =
        prettyprinterAux newline numtabs array_name                         ++
        "["                                                                 ++
        prettyprinterAux False numtabs array_locations                      ++
        "]"
    prettyprinterAux newline numtabs (LeftExpPointerValue left_exp pos left_exp_ty env errors) =
        prettyprinterAux newline numtabs left_exp                           ++
        "^"
    prettyprinterAux newline numtabs (LeftExpPointerAddress left_exp pos left_exp_ty env errors) =
        prettyprinterAux newline numtabs left_exp                           ++
        "@"

instance PrettyPrinterClass Assign where
    prettyprinterAux newline numtabs (VariableAssignment left_exp right_exp pos env errors) =
        prettyprinterAux newline numtabs left_exp                           ++
        ":= "                                                               ++
        prettyprinterAux False numtabs right_exp  

instance PrettyPrinterClass WritePrimitive where
    prettyprinterAux newline numtabs (WriteInt right_exp pos env errors) =
        ident newline numtabs "writeInt ("                                  ++
        prettyprinterAux False numtabs right_exp                             ++
        ") "
    prettyprinterAux newline numtabs (WriteReal right_exp pos env errors) =
        ident newline numtabs "writeReal ("                                 ++
        prettyprinterAux False numtabs right_exp                             ++
        ") "
    prettyprinterAux newline numtabs (WriteChar right_exp pos env errors) =
        ident newline numtabs "writeChar ("                                 ++
        prettyprinterAux False numtabs right_exp                             ++
        ") "
    prettyprinterAux newline numtabs (WriteString right_exp pos env errors) =
        ident newline numtabs "writeString ("                               ++
        prettyprinterAux False numtabs right_exp                             ++
        ") "

instance PrettyPrinterClass ReadPrimitive where
    prettyprinterAux newline numtabs (ReadInt left_exp pos env errors) =
        ident newline numtabs "readInt ("                                   ++
        prettyprinterAux False numtabs left_exp                              ++
        ") "
    prettyprinterAux newline numtabs (ReadReal left_exp pos env errors) =
        ident newline numtabs "readReal ("                                  ++
        prettyprinterAux False numtabs left_exp                              ++
        ") "
    prettyprinterAux newline numtabs (ReadChar left_exp pos env errors) =
        ident newline numtabs "readChar ("                                  ++
        prettyprinterAux False numtabs left_exp                              ++
        ") "
    prettyprinterAux newline numtabs (ReadString left_exp pos env errors) =
        ident newline numtabs "readString ("                                ++
        prettyprinterAux False numtabs left_exp                              ++
        ") "

instance PrettyPrinterClass Type where
    prettyprinterAux newline numtabs (BooleanType) =
        ident newline numtabs "boolean"
    prettyprinterAux newline numtabs (IntegerType) =
        ident newline numtabs "integer"
    prettyprinterAux newline numtabs (RealType) =
        ident newline numtabs "real"
    prettyprinterAux newline numtabs (CharType) =
        ident newline numtabs "char"
    prettyprinterAux newline numtabs (StringType) =
        ident newline numtabs "string"
    prettyprinterAux newline numtabs (ArrayType array_type (dimension:[])) =
        ident newline numtabs "array ["                                     ++
        show (fst dimension)                                                ++
        ".."                                                                ++
        show (snd dimension)                                                       ++
        "] of "                                                             ++
        prettyprinterAux False numtabs array_type
    prettyprinterAux newline numtabs (ArrayType array_type (dimension:dimensions)) =
        ident newline numtabs "array ["                                     ++
        show (fst dimension)                                                       ++
        ".."                                                                ++
        show (snd dimension)                                                       ++
        foldl (\a (x,y) -> ", " ++ show x ++ ".." ++ show y) "" dimensions                ++
        "] of "                                                             ++
        prettyprinterAux False numtabs array_type
    prettyprinterAux newline numtabs (PointerType pointer_type) =
        ident newline numtabs "^"                                           ++
        prettyprinterAux False numtabs pointer_type