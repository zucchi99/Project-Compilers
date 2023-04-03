-- Module for serialize a Abstract Sintax Tree
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter where

-- from a happy file get a pretty printer
import qualified AbsGrammar           as Abs
import ErrM

-- __________________________ AUXILIAR CLASSES AND FUNCTIONS

-- Extract the value from the monad ErrM.Ok
fromOk (ErrM.Ok a) = a

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
    prettyprinterAux firstnewline numtabs x                                         ++
    stringPrettyConcat newline numtabs [] sep concatType newline
stringPrettyConcat newline numtabs (x:xs) sep concatType firstnewline = 
    prettyprinterAux firstnewline numtabs x                                         ++
    sep                                                                             ++
    stringPrettyConcat newline numtabs xs sep concatType newline

-- define a class for pretty printer
-- this way we can define a pretty printer for each type of the abstract sintax tree
class PrettyPrinterClass a where
    prettyprinterAux :: Bool -> Int -> a -> String

-- __________________________ PRINT THE ABSTRACT SINTAX TREE FROM HAPPY
printAst x = putStrLn $ show $ fromOk x

-- __________________________ PRETTY PRINTER
prettyprinter x = prettyprinterAux True 0 $ fromOk x

-- PROGRAM START (here for debugging)
instance PrettyPrinterClass Abs.Program where
    prettyprinterAux newline numtabs (Abs.ProgramStart name block) =
        ident False 0 "program"                                ++
        prettyprinterAux False 0 name                                  ++ 
        ";"                                                             ++
        prettyprinterAux True 0 block                                   ++ 
        "."



instance PrettyPrinterClass Abs.Ident where
    prettyprinterAux newline numtabs (Abs.Ident name) = 
        ident newline numtabs name

instance PrettyPrinterClass [Abs.Ident] where
    prettyprinterAux newline numtabs idents = 
        stringPrettyConcat False numtabs idents ", " "separator" newline

instance PrettyPrinterClass Abs.BlockWithDecl where
    prettyprinterAux newline numtabs (Abs.BlockWithDeclaration declarations block) =
        prettyprinterAux newline numtabs declarations                 ++
        prettyprinterAux newline numtabs block

instance PrettyPrinterClass Abs.BlockExec where
    prettyprinterAux newline numtabs (Abs.BlockOnlyExecution terminator statements) =
        ident True numtabs "begin"                          ++
        prettyprinterAux False numtabs terminator                     ++
        prettyprinterAux True (numtabs + 1) statements                    ++
        ident True numtabs "end"

instance PrettyPrinterClass Abs.NonMandatoryTerminator where
    prettyprinterAux _ _ (Abs.NonMandatoryTerminator1) = 
        ident False 0 ""
    prettyprinterAux _ _ (Abs.NonMandatoryTerminator2) =
        ident False 0 ";"

instance PrettyPrinterClass Abs.Declaration where
    prettyprinterAux newline numtabs (Abs.DeclarationCostantsBlock block) =
        prettyprinterAux newline numtabs block
    prettyprinterAux newline numtabs (Abs.DeclarationVariablesBlock block) =
        prettyprinterAux newline numtabs block
    prettyprinterAux newline numtabs (Abs.DeclarationFunctionForw block) =
        prettyprinterAux newline numtabs block
    prettyprinterAux newline numtabs (Abs.DeclarationProcedureForw block) =
        prettyprinterAux newline numtabs block
    prettyprinterAux newline numtabs (Abs.DeclarationFunctionDecl block) =
        prettyprinterAux newline numtabs block
    prettyprinterAux newline numtabs (Abs.DeclarationProcedureDecl block) =
        prettyprinterAux newline numtabs block

instance PrettyPrinterClass [Abs.Declaration] where
    prettyprinterAux newline numtabs blocks =
        stringPrettyConcat True numtabs blocks "" "separator" newline

instance PrettyPrinterClass Abs.CostantsBlock where
    prettyprinterAux newline numtabs (Abs.CostantsBlock1 declarations) =
        ident newline numtabs "const"                          ++
        prettyprinterAux newline (numtabs + 1) declarations

instance PrettyPrinterClass Abs.ConstantDecl where
    prettyprinterAux newline numtabs (Abs.ConstantDeclaration ident expression) =
        prettyprinterAux newline numtabs ident                          ++
        "= "                                                           ++
        prettyprinterAux False numtabs expression

instance PrettyPrinterClass [Abs.ConstantDecl] where
    prettyprinterAux newline numtabs constants =
        stringPrettyConcat True numtabs constants "; " "terminator" newline

instance PrettyPrinterClass Abs.VariablesBlock where
    prettyprinterAux newline numtabs (Abs.VariablesBlock1 declarations) =
        ident newline numtabs "var"                            ++
        prettyprinterAux newline (numtabs + 1) declarations

instance PrettyPrinterClass Abs.VariableDeclBlock where
    prettyprinterAux newline numtabs (Abs.VariableDeclarationInsideBlock id tipo initassign) =
        prettyprinterAux True numtabs id                             ++
        ": "                                                           ++
        prettyprinterAux False numtabs tipo                           ++
        prettyprinterAux False numtabs initassign

instance PrettyPrinterClass [Abs.VariableDeclBlock] where
    prettyprinterAux newline numtabs variables =
        stringPrettyConcat True numtabs variables "; " "terminator" newline

instance PrettyPrinterClass Abs.InitAssign where
    prettyprinterAux newline numtabs (Abs.InitAssign1) =
        ""
    prettyprinterAux newline numtabs (Abs.InitAssign2 expression) =
        "= "                                                           ++
        prettyprinterAux False numtabs expression

instance PrettyPrinterClass Abs.DeclarationFunc where
    prettyprinterAux newline numtabs (Abs.DeclarationFunc1) =
        ""
    prettyprinterAux newline numtabs (Abs.DeclarationFunc2 declarations) =
        "( "                                                           ++
        prettyprinterAux newline numtabs declarations                   ++
        ") "

instance PrettyPrinterClass Abs.VariableDeclFunc where
    prettyprinterAux newline numtabs (Abs.VariableDeclarationInsideF id tipo) =
        prettyprinterAux newline numtabs id                             ++
        ": "                                                           ++
        prettyprinterAux False numtabs tipo                           

instance PrettyPrinterClass [Abs.VariableDeclFunc] where
    prettyprinterAux newline numtabs variables =
        stringPrettyConcat False numtabs variables "; " "separator" newline

instance PrettyPrinterClass Abs.FunctionSign where
    prettyprinterAux newline numtabs (Abs.FunctionSignature id declaration tipo) =
        ident newline numtabs "function"                       ++
        prettyprinterAux False numtabs id                             ++
        prettyprinterAux False numtabs declaration                    ++
        ": "                                                           ++
        prettyprinterAux False numtabs tipo                           ++
        "; "

instance PrettyPrinterClass Abs.ProcedureSign where
    prettyprinterAux newline numtabs (Abs.ProcedureSignature id declaration) =
        ident newline numtabs "procedure"                       ++
        prettyprinterAux False numtabs id                             ++
        prettyprinterAux False numtabs declaration                    ++
        "; "

instance PrettyPrinterClass Abs.FunctionDecl where
    prettyprinterAux newline numtabs (Abs.FunctionDeclaration functionsign block) =
        prettyprinterAux newline numtabs functionsign                   ++
        prettyprinterAux newline numtabs block                          ++
        "; "

instance PrettyPrinterClass Abs.ProcedureDecl where
    prettyprinterAux newline numtabs (Abs.ProcedureDeclaration proceduresign block) =
        prettyprinterAux newline numtabs proceduresign                  ++
        prettyprinterAux newline numtabs block                          ++
        "; "  

instance PrettyPrinterClass Abs.FunctionForw where
    prettyprinterAux newline numtabs (Abs.FunctionForward functionsign) =
        prettyprinterAux newline numtabs functionsign                   ++
        "forward ; "                                                           

instance PrettyPrinterClass Abs.ProcedureForw where
    prettyprinterAux newline numtabs (Abs.ProcedureForward proceduresign) =
        prettyprinterAux newline numtabs proceduresign                  ++
        "forward ; "                                                           

instance PrettyPrinterClass Abs.FunctionCall where
    prettyprinterAux newline numtabs (Abs.FunctionCall id expressions) =
        prettyprinterAux newline numtabs id                             ++
        "( "                                                           ++
        prettyprinterAux False numtabs expressions                   ++
        ")"

instance PrettyPrinterClass Abs.ProcedureCall where
    prettyprinterAux newline numtabs (Abs.ProcedureCall id expressions) =
        prettyprinterAux newline numtabs id                             ++
        "( "                                                           ++
        prettyprinterAux False numtabs expressions                   ++
        ")"

instance PrettyPrinterClass Abs.Statement where
    prettyprinterAux newline numtabs (Abs.StatementBlock block) =
        prettyprinterAux newline numtabs block
    prettyprinterAux newline numtabs (Abs.StatementIf condition statement elseblock) =
        ident newline numtabs "if"                           ++
        "( "                                                           ++
        prettyprinterAux False numtabs condition                      ++
        ") then "                                                           ++
        prettyprinterAux newline (numtabs + 1) statement                      ++      
        prettyprinterAux newline numtabs elseblock
    prettyprinterAux newline numtabs (Abs.StatementFor assign expression statement) =
        ident newline numtabs "for"                           ++
        prettyprinterAux False numtabs assign                         ++
        ident False numtabs "to"                           ++
        prettyprinterAux False numtabs expression                     ++
        ident False numtabs "do"                           ++
        prettyprinterAux newline (numtabs + 1) statement
    prettyprinterAux newline numtabs (Abs.StatementWhile condition statement) =
        ident newline numtabs "while"                           ++
        prettyprinterAux newline numtabs condition                      ++
        ident False numtabs "do"                           ++
        prettyprinterAux newline (numtabs + 1) statement
    prettyprinterAux newline numtabs (Abs.StatementRepeatUntil statements condition) =
        ident newline numtabs "repeat"                       ++
        prettyprinterAux newline (numtabs + 1) statements                   ++
        ident newline numtabs "until"                        ++
        prettyprinterAux False numtabs condition
    prettyprinterAux newline numtabs (Abs.StatementAssign assign) =
        prettyprinterAux newline numtabs assign
    prettyprinterAux newline numtabs (Abs.StatementFunctionCall functioncall) =
        prettyprinterAux newline numtabs functioncall
    prettyprinterAux newline numtabs (Abs.StatementProcedureCall procedurecall) =
        prettyprinterAux newline numtabs procedurecall
    prettyprinterAux newline numtabs (Abs.StatementWrite writeprimitive) =
        prettyprinterAux newline numtabs writeprimitive
    prettyprinterAux newline numtabs (Abs.StatementRead readprimitive) =
        prettyprinterAux newline numtabs readprimitive

instance PrettyPrinterClass [Abs.Statement] where
    prettyprinterAux newline numtabs statements =
        stringPrettyConcat True numtabs statements "; " "separator" newline

instance PrettyPrinterClass Abs.ElseBlock where
    prettyprinterAux newline numtabs (Abs.ElseBlock1) =
        ""
    prettyprinterAux newline numtabs (Abs.ElseBlock2 statement) =
        ident newline numtabs "else"                         ++
        prettyprinterAux newline (numtabs + 1) statement

instance PrettyPrinterClass Abs.Assign where
    prettyprinterAux newline numtabs (Abs.VariableAssignment leftExp rightExp) =
        prettyprinterAux newline numtabs leftExp                        ++
        ":= "                                                          ++
        prettyprinterAux False numtabs rightExp   

instance PrettyPrinterClass Abs.RightExp where
    prettyprinterAux newline numtabs (Abs.RightExpOr rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "or "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpAnd rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "and "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpGreater rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "> "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpLess rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "< "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpGreaterEqual rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        ">= "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpLessEqual rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "<= "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpEqual rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "= "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpPlus rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "+ "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpMinus rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "- "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpTimes rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "* "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpDivide rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "/ "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpMod rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "mod "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpDiv rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        " div "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpPower rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++   
        "** "                                                          ++
        prettyprinterAux False numtabs rightexp2
    prettyprinterAux newline numtabs (Abs.RightExpNot rightexp) =
        "not "                                                          ++
        prettyprinterAux False numtabs rightexp                         
    prettyprinterAux newline numtabs (Abs.RightExpMinusUnary rightexp) =
        "- "                                                          ++
        prettyprinterAux False numtabs rightexp                         
    prettyprinterAux newline numtabs (Abs.RightExpPlusUnary rightexp) =
        "+ "                                                          ++
        prettyprinterAux False numtabs rightexp
    prettyprinterAux newline numtabs (Abs.RightExpInteger tipo) =
        ident newline numtabs $ show tipo                 
    prettyprinterAux newline numtabs (Abs.RightExpReal tipo) =
        ident newline numtabs $ show tipo                 
    prettyprinterAux newline numtabs (Abs.RightExpBoolean tipo) =
        ident newline numtabs $ show tipo                 
    prettyprinterAux newline numtabs (Abs.RightExpChar tipo) =
        ident newline numtabs $ show tipo                 
    prettyprinterAux newline numtabs (Abs.RightExpString tipo) =
        ident newline numtabs $ show tipo                 
    prettyprinterAux newline numtabs (Abs.RightExpFunctionCall funcall) =
        prettyprinterAux newline numtabs funcall        
    prettyprinterAux newline numtabs (Abs.RightExpCopy leftExp) =
        prettyprinterAux newline numtabs leftExp

instance PrettyPrinterClass [Abs.RightExp] where
    prettyprinterAux newline numtabs expressions =
        stringPrettyConcat False numtabs expressions ", " "separator" newline

instance PrettyPrinterClass Abs.LeftExp where
    prettyprinterAux newline numtabs (Abs.LeftExpIdent id) =
        prettyprinterAux newline numtabs id
    prettyprinterAux newline numtabs (Abs.LeftExpArrayAccess leftExp rightExp) =
        prettyprinterAux newline numtabs leftExp                        ++
        "["                                                             ++
        prettyprinterAux False numtabs rightExp                       ++
        "]"
    prettyprinterAux newline numtabs (Abs.LeftExpPointerValue leftExp) =
        prettyprinterAux newline numtabs leftExp                        ++
        "^"
    prettyprinterAux newline numtabs (Abs.LeftExpPointerAddress leftExp) =
        prettyprinterAux newline numtabs leftExp                        ++
        "@"
    
instance PrettyPrinterClass Abs.Type where
    prettyprinterAux newline numtabs (Abs.TypeBaseType tipo) =
        prettyprinterAux newline numtabs tipo
    prettyprinterAux newline numtabs (Abs.TypeCompositeType tipo) =
        prettyprinterAux newline numtabs tipo
    
instance PrettyPrinterClass Abs.BaseType where
    prettyprinterAux newline numtabs (Abs.BaseType_integer) =
        ident newline numtabs "integer"
    prettyprinterAux newline numtabs (Abs.BaseType_real) =
        ident newline numtabs "real"
    prettyprinterAux newline numtabs (Abs.BaseType_char) =
        ident newline numtabs "char"
    prettyprinterAux newline numtabs (Abs.BaseType_boolean) =
        ident newline numtabs "boolean"
    prettyprinterAux newline numtabs (Abs.BaseType_string) =
        ident newline numtabs "string"

instance PrettyPrinterClass Abs.Boolean where
    prettyprinterAux newline numtabs (Abs.Boolean_true) =
        ident False numtabs "true"
    prettyprinterAux newline numtabs (Abs.Boolean_false) =
        ident False numtabs "false"

instance PrettyPrinterClass Abs.CompositeType where
    prettyprinterAux newline numtabs (Abs.CompTypeArray leftExp rightExp) =
        ident newline numtabs "array ["                                  ++
        prettyprinterAux False numtabs leftExp                        ++
        "] of "                                                            ++
        prettyprinterAux False numtabs rightExp                       
    prettyprinterAux newline numtabs (Abs.CompTypePointer tipo) =       
        "^"                                                             ++
        prettyprinterAux False numtabs tipo
    
instance PrettyPrinterClass Abs.ArrayDeclarationDim where
    prettyprinterAux newline numtabs (Abs.ArrayDeclarationDim rightexp1 rightexp2) =
        prettyprinterAux False numtabs rightexp1                      ++
        ".. "                                                            ++
        prettyprinterAux False numtabs rightexp2

instance PrettyPrinterClass [Abs.ArrayDeclarationDim] where
    prettyprinterAux newline numtabs arrayDeclarations =
        stringPrettyConcat False numtabs arrayDeclarations ", " "separator" newline

instance PrettyPrinterClass Abs.WritePrimitive where
    prettyprinterAux newline numtabs (Abs.WriteInt rightexp) =
        ident newline numtabs "writeInt ("                              ++
        prettyprinterAux False numtabs rightexp                       ++
        ") "
    prettyprinterAux newline numtabs (Abs.WriteReal rightexp) =
        ident newline numtabs "writeReal ("                             ++
        prettyprinterAux False numtabs rightexp                       ++
        ") "
    prettyprinterAux newline numtabs (Abs.WriteChar rightexp) =
        ident newline numtabs "writeChar ("                             ++
        prettyprinterAux False numtabs rightexp                       ++
        ") "
    prettyprinterAux newline numtabs (Abs.WriteString rightexp) =
        ident newline numtabs "writeString ("                           ++
        prettyprinterAux False numtabs rightexp                       ++
        ") "

instance PrettyPrinterClass Abs.ReadPrimitive where
    prettyprinterAux newline numtabs (Abs.ReadInt leftexp) =
        ident newline numtabs "readInt ("                               ++
        prettyprinterAux False numtabs leftexp                        ++
        ") "
    prettyprinterAux newline numtabs (Abs.ReadReal leftexp) =
        ident newline numtabs "readReal ("                              ++
        prettyprinterAux False numtabs leftexp                        ++
        ") "
    prettyprinterAux newline numtabs (Abs.ReadChar leftexp) =
        ident newline numtabs "readChar ("                              ++
        prettyprinterAux False numtabs leftexp                        ++
        ") "
    prettyprinterAux newline numtabs (Abs.ReadString leftexp) =
        ident newline numtabs "readString ("                            ++
        prettyprinterAux False numtabs leftexp                        ++
        ") "