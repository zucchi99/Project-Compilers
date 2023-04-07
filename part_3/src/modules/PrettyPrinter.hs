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
prettyPrinterDeclaration newline numtabs (DeclarationCostant name a value b : declarations) "const" =
    prettyprinterAux True (numtabs + 1) (DeclarationCostant name a value b)                     ++
    ";"                                                                                         ++
    prettyPrinterDeclaration newline numtabs declarations "const"
prettyPrinterDeclaration newline numtabs (DeclarationCostant name a value b : declarations) _ =
    ident True numtabs "const"                                                                  ++
    prettyprinterAux True (numtabs + 1) (DeclarationCostant name a value b)                     ++
    ";"                                                                                         ++
    prettyPrinterDeclaration newline numtabs declarations "const"
prettyPrinterDeclaration newline numtabs (DeclarationVariable name tipo value a : declarations) "var" =
    prettyprinterAux True (numtabs + 1) (DeclarationVariable name tipo value a)                 ++
    ";"                                                                                         ++
    prettyPrinterDeclaration newline numtabs declarations "var"
prettyPrinterDeclaration newline numtabs (DeclarationVariable name tipo value a : declarations) _ =
    ident True numtabs "var"                                                                    ++
    prettyprinterAux True (numtabs + 1) (DeclarationVariable name tipo value a)                 ++
    ";"                                                                                         ++
    prettyPrinterDeclaration newline numtabs declarations "var"
prettyPrinterDeclaration newline numtabs (declaration : declarations) _ =
    prettyprinterAux True numtabs declaration                                                   ++
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
    prettyprinterAux newline numtabs (ProgramStart name block _) =
        ident False 0 "program"                                             ++
        prettyprinterAux False 0 name                                       ++ 
        ";"                                                                 ++
        prettyprinterAux True 0 block                                       ++ 
        "."

instance PrettyPrinterClass Ident where
    prettyprinterAux newline numtabs (Ident name _) = 
        ident newline numtabs name

instance PrettyPrinterClass [Ident] where
    prettyprinterAux newline numtabs idents = 
        stringPrettyConcat False numtabs idents ", " "separator" newline

instance PrettyPrinterClass BlockWithDecl where
    prettyprinterAux newline numtabs (BlockWithDeclaration declarations statements _) =
        prettyprinterAux newline numtabs declarations                       ++
        ident True numtabs "begin"                                          ++
        prettyprinterAux True (numtabs + 1) statements                      ++
        ident True numtabs "end"

{--
instance PrettyPrinterClass BlockExec where
    prettyprinterAux newline numtabs (BlockOnlyExecution statements _) =
        ident True numtabs "begin"                                          ++
        prettyprinterAux True (numtabs + 1) statements                      ++
        ident True numtabs "end"
--}

instance PrettyPrinterClass Declaration where
    prettyprinterAux newline numtabs (DeclarationCostant name _ value _) =
        prettyprinterAux newline numtabs name                               ++
        ident False numtabs "= "                                            ++
        prettyprinterAux False numtabs value
    prettyprinterAux newline numtabs (DeclarationVariable name tipo value _) =
        prettyprinterAux newline numtabs name                               ++
        ident False numtabs ":"                                            ++
        prettyprinterAux False numtabs tipo                                 ++
        if isNothing value then "" else 
            ident False numtabs "= "                                        ++
            prettyprinterAux False numtabs (fromJust value)
    prettyprinterAux newline numtabs (DeclarationFunction name params tipo body _) =
        ident newline numtabs "function"                                    ++
        prettyprinterAux False numtabs name                                 ++
        ident False numtabs "( "                                            ++
        stringPrettyConcat False numtabs params "; " "separator" False      ++
        ident False numtabs ") :"                                           ++
        prettyprinterAux False numtabs tipo                                 ++
        ident False numtabs "; "                                            ++
        if isNothing body then "forward ;" else 
            prettyprinterAux False numtabs (fromJust body)                  ++
            ident False numtabs ";"
    prettyprinterAux newline numtabs (DeclarationProcedure name params body _) =
        ident newline numtabs "procedure"                                   ++
        prettyprinterAux False numtabs name                                 ++
        ident False numtabs "( "                                            ++
        stringPrettyConcat False numtabs params "; " "separator" False      ++
        ident False numtabs ") ; "                                          ++
        if isNothing body then "forward ;" else 
            prettyprinterAux False numtabs (fromJust body)                  ++
            ident False numtabs ";"

instance PrettyPrinterClass [Declaration] where
    prettyprinterAux newline numtabs blocks =
        prettyPrinterDeclaration newline numtabs blocks ""

instance PrettyPrinterClass Statement where
    prettyprinterAux newline numtabs (StatementBlock block _) =
        prettyprinterAux newline numtabs block
    prettyprinterAux newline numtabs (StatementIf condition then_body else_body _) =
        ident newline numtabs "if"                                          ++
        "( "                                                                ++
        prettyprinterAux False numtabs condition                            ++
        ") then "                                                           ++
        prettyprinterAux newline (numtabs + 1) then_body                    ++      
        if isNothing else_body then "" else 
            prettyprinterAux newline (numtabs + 1) (fromJust else_body)
    prettyprinterAux newline numtabs (StatementFor condition then_body for_var _) =
        ident newline numtabs "for"                                         ++
        prettyprinterAux False numtabs for_var                              ++
        ident False numtabs "to"                                            ++
        prettyprinterAux False numtabs condition                            ++
        ident False numtabs "do"                                            ++
        prettyprinterAux newline (numtabs + 1) then_body
    prettyprinterAux newline numtabs (StatementWhile condition then_body _) =
        ident newline numtabs "while"                                       ++
        prettyprinterAux newline numtabs condition                          ++
        ident False numtabs "do"                                            ++
        prettyprinterAux newline (numtabs + 1) then_body
    prettyprinterAux newline numtabs (StatementRepeatUntil condition then_body _) =
        ident newline numtabs "repeat"                                      ++
        prettyprinterAux newline (numtabs + 1) then_body                    ++
        ident newline numtabs "until"                                       ++
        prettyprinterAux False numtabs condition
    prettyprinterAux newline numtabs (StatementAssign assign _) =
        prettyprinterAux newline numtabs assign
    prettyprinterAux newline numtabs (StatementFunctionCall name params _) =
        prettyprinterAux newline numtabs name                               ++
        "("                                                                 ++
        prettyprinterAux False numtabs params                               ++
        ")"
    prettyprinterAux newline numtabs (StatementProcedureCall name params _) =
        prettyprinterAux newline numtabs name                               ++
        "("                                                                 ++
        prettyprinterAux False numtabs params                               ++
        ")"
    prettyprinterAux newline numtabs (StatementWrite writeprimitive _) =
        prettyprinterAux newline numtabs writeprimitive
    prettyprinterAux newline numtabs (StatementRead readprimitive _) =
        prettyprinterAux newline numtabs readprimitive

instance PrettyPrinterClass [Statement] where
    prettyprinterAux newline numtabs statements =
        stringPrettyConcat True numtabs statements "; " "separator" newline

instance PrettyPrinterClass ElseBlock where
    prettyprinterAux newline numtabs (ElseBlock body _) =
        ident newline numtabs "else"                                        ++
        prettyprinterAux newline (numtabs + 1) body

instance PrettyPrinterClass Assign where
    prettyprinterAux newline numtabs (VariableAssignment leftExp rightExp _) =
        prettyprinterAux newline numtabs leftExp                            ++
        ":= "                                                               ++
        prettyprinterAux False numtabs rightExp   

instance PrettyPrinterClass RightExp where
    prettyprinterAux newline numtabs (RightExpOr sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "or "                                                               ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpAnd sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "and "                                                              ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpGreater sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "> "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpLess sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "< "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpGreaterEqual sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        ">= "                                                               ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpLessEqual sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "<= "                                                               ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpEqual sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "= "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpPlus sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "+ "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpMinus sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "- "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpTimes sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "* "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpDivide sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "/ "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpMod sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "mod "                                                              ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpDiv sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        " div "                                                             ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpPower sx dx _) =
        prettyprinterAux False numtabs sx                                   ++   
        "** "                                                               ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpNot dx _) =
        "not "                                                              ++
        prettyprinterAux False numtabs dx                         
    prettyprinterAux newline numtabs (RightExpMinusUnary dx _) =
        "- "                                                                ++
        prettyprinterAux False numtabs dx                         
    prettyprinterAux newline numtabs (RightExpPlusUnary dx _) =
        "+ "                                                                ++
        prettyprinterAux False numtabs dx
    prettyprinterAux newline numtabs (RightExpInteger value _) =
        ident newline numtabs $ show value
    prettyprinterAux newline numtabs (RightExpReal value _) =
        ident newline numtabs $ show value
    prettyprinterAux newline numtabs (RightExpBoolean value _) =
        ident newline numtabs $ show value
    prettyprinterAux newline numtabs (RightExpChar value _) =
        ident newline numtabs $ show value
    prettyprinterAux newline numtabs (RightExpString value _) =
        ident newline numtabs $ show value
    prettyprinterAux newline numtabs (RightExpFunctionCall name params _) =
        prettyprinterAux newline numtabs name                               ++
        "("                                                                 ++
        prettyprinterAux False numtabs params                               ++
        ")"
    prettyprinterAux newline numtabs (RightExpCopy leftExp _) =
        prettyprinterAux newline numtabs leftExp

instance PrettyPrinterClass [RightExp] where
    prettyprinterAux newline numtabs expressions =
        stringPrettyConcat False numtabs expressions ", " "separator" newline

instance PrettyPrinterClass LeftExp where
    prettyprinterAux newline numtabs (LeftExpIdent id _) =
        prettyprinterAux newline numtabs id
    prettyprinterAux newline numtabs (LeftExpArrayAccess name position _) =
        prettyprinterAux newline numtabs name                               ++
        "["                                                                 ++
        prettyprinterAux False numtabs position                             ++
        "]"
    prettyprinterAux newline numtabs (LeftExpPointerValue leftExp _) =
        prettyprinterAux newline numtabs leftExp                            ++
        "^"
    prettyprinterAux newline numtabs (LeftExpPointerAddress leftExp _) =
        prettyprinterAux newline numtabs leftExp                            ++
        "@"

instance PrettyPrinterClass WritePrimitive where
    prettyprinterAux newline numtabs (WriteInt rightexp _) =
        ident newline numtabs "writeInt ("                                  ++
        prettyprinterAux False numtabs rightexp                             ++
        ") "
    prettyprinterAux newline numtabs (WriteReal rightexp _) =
        ident newline numtabs "writeReal ("                                 ++
        prettyprinterAux False numtabs rightexp                             ++
        ") "
    prettyprinterAux newline numtabs (WriteChar rightexp _) =
        ident newline numtabs "writeChar ("                                 ++
        prettyprinterAux False numtabs rightexp                             ++
        ") "
    prettyprinterAux newline numtabs (WriteString rightexp _) =
        ident newline numtabs "writeString ("                               ++
        prettyprinterAux False numtabs rightexp                             ++
        ") "

instance PrettyPrinterClass ReadPrimitive where
    prettyprinterAux newline numtabs (ReadInt leftexp _) =
        ident newline numtabs "readInt ("                                   ++
        prettyprinterAux False numtabs leftexp                              ++
        ") "
    prettyprinterAux newline numtabs (ReadReal leftexp _) =
        ident newline numtabs "readReal ("                                  ++
        prettyprinterAux False numtabs leftexp                              ++
        ") "
    prettyprinterAux newline numtabs (ReadChar leftexp _) =
        ident newline numtabs "readChar ("                                  ++
        prettyprinterAux False numtabs leftexp                              ++
        ") "
    prettyprinterAux newline numtabs (ReadString leftexp _) =
        ident newline numtabs "readString ("                                ++
        prettyprinterAux False numtabs leftexp                              ++
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
    prettyprinterAux newline numtabs (ArrayType tipo (dimension:[])) =
        ident newline numtabs "array ["                                     ++
        show (fst dimension)                                                ++
        ".."                                                                ++
        show (snd dimension)                                                       ++
        "] of "                                                             ++
        prettyprinterAux False numtabs tipo
    prettyprinterAux newline numtabs (ArrayType tipo (dimension:dimensions)) =
        ident newline numtabs "array ["                                     ++
        show (fst dimension)                                                       ++
        ".."                                                                ++
        show (snd dimension)                                                       ++
        foldl (\a (x,y) -> ", " ++ show x ++ ".." ++ show y) "" dimensions                ++
        "] of "                                                             ++
        prettyprinterAux False numtabs tipo
    prettyprinterAux newline numtabs (PointerType tipo) =
        ident newline numtabs "^"                                           ++
        prettyprinterAux False numtabs tipo