-- This Happy file was machine-generated by the BNF converter and modified by hand

{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}

module Parser where

import ErrM
import LexGrammar
-- import qualified AbstractSyntax as AbsSyn
import AbstractSyntax
import qualified Types as Tipi
-- import AbsGrammar

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
    '(' { PT _ (TS _ 1) }
    ')' { PT _ (TS _ 2) }
    '*' { PT _ (TS _ 3) }
    '**' { PT _ (TS _ 4) }
    '+' { PT _ (TS _ 5) }
    ',' { PT _ (TS _ 6) }
    '-' { PT _ (TS _ 7) }
    '.' { PT _ (TS _ 8) }
    '..' { PT _ (TS _ 9) }
    '/' { PT _ (TS _ 10) }
    ':' { PT _ (TS _ 11) }
    ':=' { PT _ (TS _ 12) }
    ';' { PT _ (TS _ 13) }
    '<' { PT _ (TS _ 14) }
    '<=' { PT _ (TS _ 15) }
    '=' { PT _ (TS _ 16) }
    '>' { PT _ (TS _ 17) }
    '>=' { PT _ (TS _ 18) }
    '@' { PT _ (TS _ 19) }
    '[' { PT _ (TS _ 20) }
    ']' { PT _ (TS _ 21) }
    '^' { PT _ (TS _ 22) }
    'and' { PT _ (TS _ 23) }
    'array' { PT _ (TS _ 24) }
    'begin' { PT _ (TS _ 25) }
    'boolean' { PT _ (TS _ 26) }
    'char' { PT _ (TS _ 27) }
    'const' { PT _ (TS _ 28) }
    'div' { PT _ (TS _ 29) }
    'do' { PT _ (TS _ 30) }
    'else' { PT _ (TS _ 31) }
    'end' { PT _ (TS _ 32) }
    'false' { PT _ (TS _ 33) }
    'for' { PT _ (TS _ 34) }
    'forward' { PT _ (TS _ 35) }
    'function' { PT _ (TS _ 36) }
    'if' { PT _ (TS _ 37) }
    'integer' { PT _ (TS _ 38) }
    'mod' { PT _ (TS _ 39) }
    'not' { PT _ (TS _ 40) }
    'of' { PT _ (TS _ 41) }
    'or' { PT _ (TS _ 42) }
    'procedure' { PT _ (TS _ 43) }
    'program' { PT _ (TS _ 44) }
    'readChar' { PT _ (TS _ 45) }
    'readInt' { PT _ (TS _ 46) }
    'readReal' { PT _ (TS _ 47) }
    'readString' { PT _ (TS _ 48) }
    'real' { PT _ (TS _ 49) }
    'repeat' { PT _ (TS _ 50) }
    'string' { PT _ (TS _ 51) }
    'then' { PT _ (TS _ 52) }
    'to' { PT _ (TS _ 53) }
    'true' { PT _ (TS _ 54) }
    'until' { PT _ (TS _ 55) }
    'var' { PT _ (TS _ 56) }
    'while' { PT _ (TS _ 57) }
    'writeChar' { PT _ (TS _ 58) }
    'writeInt' { PT _ (TS _ 59) }
    'writeReal' { PT _ (TS _ 60) }
    'writeString' { PT _ (TS _ 61) }
    L_ident  { PT _ (TV _) }
    L_integ  { PT _ (TI _) }
    L_doubl  { PT _ (TD _) }
    L_charac { PT _ (TC _) }
    L_quoted { PT _ (TL _) }

%%

--------------------------------------------------------------------------------------------------------------------------------------

Ident   :: { Ident }
Ident   : L_ident {
    Ident {
        id_name = (prToken $1),
        ident_pos = (tokenLineCol $1)
    }                 
}

Integer :: { (Int, (Int, Int)) }
Integer : L_integ   { ((read (prToken $1)) :: Int, tokenLineCol $1) }

Double  :: { (Double, (Int, Int)) }
Double  : L_doubl   { ((read (prToken $1)) :: Double, tokenLineCol $1) }

Char    :: { (Char, (Int, Int)) }
Char    : L_charac  { ((read (prToken $1)) :: Char, tokenLineCol $1) }

String  :: { (String, (Int, Int)) }
String  : L_quoted  { ((read (prToken $1)) :: String, tokenLineCol $1) }

Boolean :: { (Bool, (Int, Int)) }
Boolean : 'true'        { (True, tokenLineCol $1) }
        | 'false'       { (False, tokenLineCol $1) }

Program :: { Program }
Program : 'program' Ident ';' BlockWithDecl '.' {
    ProgramStart  {
        program_name = $2,
        program_block_decl = $4,
        program_pos = (tokenLineCol $1)                                                   
}}

BlockWithDecl   :: { BlockWithDecl }
BlockWithDecl   : ListDeclaration 'begin' NonMandatoryTerminator ListStatement 'end' {
    BlockWithDeclaration  {
        block_declarations = (reverse $1),
        statements = $4,
        block_with_decl_pos = if (null $1) then (tokenLineCol $2) else (declaration_pos (head $1))                                                                
}}

ListDeclaration :: { [Declaration] }
ListDeclaration : {- empty -}                 { [] }
                | ListDeclaration Declaration { $2 ++ $1 }

{--
BlockExec   :: { BlockExec }
BlockExec   : 'begin' NonMandatoryTerminator ListStatement 'end' {
    BlockOnlyExecution {
        statements = $3,
        block_exec_pos = (tokenLineCol $1)                                                                                    
}} 
--}

ListStatement   :: { [Statement] }
ListStatement   : {- empty -}                 { [] }
                | Statement                   { (:[]) $1 }
                | Statement ';' ListStatement { (:) $1 $3 }

NonMandatoryTerminator  :: {}
NonMandatoryTerminator  : {- empty -} {}
                        | ';'         {}

Declaration :: { [Declaration] }
Declaration : CostantsBlock     { $1 } 
            | VariablesBlock    { $1 } 
            | FunctionForw      { [$1] } 
            | ProcedureForw     { [$1] } 
            | FunctionDecl      { [$1] } 
            | ProcedureDecl     { [$1] } 

CostantsBlock   :: { [Declaration] }
CostantsBlock   : 'const' ListConstantDecl  { $2 }

ListConstantDecl    :: { [Declaration] }
ListConstantDecl    : ConstantDecl ';'                  { (:[]) $1 }
                    | ConstantDecl ';' ListConstantDecl { (:) $1 $3 }

ConstantDecl    :: { Declaration }
ConstantDecl    : Ident '=' RightExp {
    DeclarationCostant   {
        costant_name = $1,
        costant_type_maybe = Nothing,
        costant_value = $3,
        declaration_pos = (ident_pos $1)                                                            
}}

VariablesBlock  :: { [Declaration] }
VariablesBlock  : 'var' ListVariableDeclBlock { $2 }

ListVariableDeclBlock   :: { [Declaration] }
ListVariableDeclBlock   : VariableDeclBlock ';'                         { $1 }
                        | VariableDeclBlock ';' ListVariableDeclBlock   { $1 ++ $3 }

ListVariableDeclFunc    :: { [Declaration] }
ListVariableDeclFunc    : {- empty -}                                   { [] }
                        | VariableDeclFunc                              { $1 }
                        | VariableDeclFunc ';' ListVariableDeclFunc     { $1 ++ $3 }

VariableDeclBlock :: { [Declaration] }
VariableDeclBlock : ListIdent ':' Type InitAssign   {
    -- foreach element in ListIdent, create a DeclarationVariable
    let createDeclarationVariable :: Ident -> Declaration
        createDeclarationVariable ident = DeclarationVariable   {
            variable_name = ident,
            variable_type = $3,
            variable_value_maybe = $4,
            declaration_pos = (ident_pos ident)                                                                
    } 
    in map (createDeclarationVariable) $1
}

InitAssign  :: { Maybe (RightExp) }
InitAssign  : {- empty -}       { Nothing }
            | '=' RightExp      { Just $2 }

DeclarationFunc :: { [Declaration] }
DeclarationFunc : {- empty -}                   { [] }
                | '(' ListVariableDeclFunc ')'  { $2 }

VariableDeclFunc    :: { [Declaration] }
VariableDeclFunc    : ListIdent ':' Type    {
    -- foreach element in ListIdent, create a DeclarationVariable
    let createDeclarationVariable :: Ident -> Declaration
        createDeclarationVariable ident = DeclarationVariable   {
            variable_name = ident,
            variable_type = $3,
            variable_value_maybe = Nothing,
            declaration_pos = (ident_pos ident)                                                            
    } 
    in map (createDeclarationVariable) $1   
}

ListIdent   :: { [Ident] }
ListIdent   : Ident                 { (:[]) $1 } 
            | Ident ',' ListIdent   { (:) $1 $3 }

FunctionSign    :: { Declaration }
FunctionSign    : 'function' Ident DeclarationFunc ':' Type ';' {
    DeclarationFunction   {
        declaration_name = $2,
        declaration_params = $3,
        function_type = $5,
        declaration_body_maybe = Nothing,
        declaration_pos = (tokenLineCol $1)                                                                                
}}

ProcedureSign   :: { Declaration }
ProcedureSign   : 'procedure' Ident DeclarationFunc ';' {
    DeclarationProcedure  {
        declaration_name = $2,
        declaration_params = $3,
        declaration_body_maybe = Nothing,
        declaration_pos = (tokenLineCol $1)                                                                               
}}

FunctionDecl    :: { Declaration }
FunctionDecl    : FunctionSign BlockWithDecl ';'    {
    DeclarationFunction   {
        declaration_name = declaration_name $1,
        declaration_params = declaration_params $1,
        function_type = function_type $1,
        declaration_body_maybe = Just $2,
        declaration_pos = (declaration_pos $1)                                                                            
}}

ProcedureDecl   :: { Declaration }
ProcedureDecl   : ProcedureSign BlockWithDecl ';'   {
    DeclarationProcedure  {
        declaration_name = declaration_name $1,
        declaration_params = declaration_params $1,
        declaration_body_maybe = Just $2,
        declaration_pos = (declaration_pos $1)                                                                        
}}

FunctionForw    :: { Declaration }
FunctionForw    : FunctionSign 'forward' ';'    { $1 }

ProcedureForw   :: { Declaration }
ProcedureForw   : ProcedureSign 'forward' ';'   { $1 }

FunctionCall    :: { Statement }
FunctionCall    : Ident '(' ListRightExp ')'    {
    StatementFunctionCall {
        call_name = $1,
        call_params = $3,
        statement_pos = (ident_pos $1)                                                                        
}}

ProcedureCall   :: { Statement }
ProcedureCall   : Ident '(' ListRightExp ')'  {
    StatementProcedureCall    {
        call_name = $1,
        call_params = $3,
        statement_pos = (ident_pos $1)                                                                        
}}

ListRightExp    :: { [RightExp] }
ListRightExp    : {- empty -}                   { [] }
                | RightExp                      { (:[]) $1 }
                | RightExp ',' ListRightExp     { (:) $1 $3 }

Statement :: { Statement }
Statement   : BlockWithDecl  {
                StatementBlock {
                    block = $1,
                    statement_pos = (block_with_decl_pos $1)                                                                            
                }}
            | 'if' RightExp 'then' Statement ElseBlock {
                StatementIf   {
                    condition = $2,
                    then_body = $4,
                    else_body_maybe = $5,
                    statement_pos = (tokenLineCol $1)                                                                           
                }}
            | 'for' Assign 'to' RightExp 'do' Statement {
                StatementFor  {
                    condition = $4,
                    then_body = $6,
                    for_var = $2,
                    statement_pos = (tokenLineCol $1)
                }}
            | 'while' RightExp 'do' Statement {
                StatementWhile    {
                    condition = $2,
                    then_body = $4,
                    statement_pos = (tokenLineCol $1)
                }}
            | 'repeat' Statement 'until' RightExp {
                StatementRepeatUntil  {
                    condition = $4,
                    then_body = $2,
                    statement_pos = (tokenLineCol $1)
                }}
            | Assign {
                StatementAssign   {
                    assign = $1,
                    statement_pos = (assign_pos $1)
                }}
            | FunctionCall  { $1 }
            | ProcedureCall { $1 }
            | WritePrimitive {
                StatementWrite {
                    write_primitive = $1,
                    statement_pos = (write_primitive_pos $1)
                }}
            | ReadPrimitive {
                StatementRead {
                    read_primitive = $1,
                    statement_pos = (read_primitive_pos $1)
                }}

ElseBlock   :: { Maybe ElseBlock }
ElseBlock   : {- empty -} { Nothing }
            | 'else' Statement {
                Just ElseBlock    {
                    else_body = $2,
                    else_block_pos = (tokenLineCol $1)
                }}

Assign      :: { Assign }
Assign      : LeftExp ':=' RightExp {
    VariableAssignment    {
        left_exp_assignment = $1,
        right_exp_assignment = $3,
        assign_pos = (left_exp_pos $1)
}}

RightExp    :: { RightExp }
RightExp    : RightExp1   { $1 }
            | RightExp 'or' RightExp1 {
                RightExpOr    {
                    sx = $1,
                    dx = $3,
                    right_exp_pos = (right_exp_pos $1)                                                                
}}

RightExp1   :: { RightExp }
RightExp1   : RightExp2  { $1 }
            | RightExp1 'and' RightExp2 {
                RightExpAnd {
                    sx = $1,
                    dx = $3, 
                    right_exp_pos = (right_exp_pos $1) 
}}

RightExp2   :: { RightExp }
RightExp2   : RightExp3   { $1 }
            | RightExp2 '>' RightExp3 {
                RightExpGreater {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1)
                }}
            | RightExp2 '<' RightExp3 {
                RightExpLess {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1) 
                }}
            | RightExp2 '>=' RightExp3 {
                RightExpGreaterEqual {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1) 
                }}
            | RightExp2 '<=' RightExp3 {
                RightExpLessEqual {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1) 
                }}
            | RightExp2 '=' RightExp3 {
                RightExpEqual {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1) 
                }}

RightExp3   :: { RightExp }
RightExp3   : RightExp4 { $1 }
            | RightExp3 '+' RightExp4 {
                RightExpPlus {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1) 
                }}
            | RightExp3 '-' RightExp4 {
                RightExpMinus {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1) 
                }}

RightExp4   :: { RightExp }
RightExp4   : RightExp5 { $1 }
            | RightExp4 '*' RightExp5 {
                RightExpTimes {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1) 
                }}
            | RightExp4 '/' RightExp5 {
                RightExpDivide {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1) 
                }}
            | RightExp4 'mod' RightExp5 {
                RightExpMod {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1) 
                }}
            | RightExp4 'div' RightExp5 {
                RightExpDiv {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1) 
                }}

RightExp5   :: { RightExp }
RightExp5   : RightExp6 { $1 }
            | RightExp5 '**' RightExp6 {
                RightExpPower {
                    sx = $1, dx = $3, right_exp_pos = (right_exp_pos $1) 
                }}

RightExp6   :: { RightExp }
RightExp6   : RightExp7  { $1 }
            | 'not' RightExp7  {
                RightExpNot {
                    dx = $2, right_exp_pos = (tokenLineCol $1) 
                }}
            | '-' RightExp7 {
                RightExpMinusUnary {
                    dx = $2, right_exp_pos = (tokenLineCol $1) 
                }}
            | '+' RightExp7 {
                RightExpPlusUnary {
                    dx = $2, right_exp_pos = (tokenLineCol $1) 
                }}

RightExp7   :: { RightExp }
RightExp7   : '(' RightExp ')' { $2 }
            | Integer {
                RightExpInteger   {
                    right_exp_int = fst $1, right_exp_pos = snd $1
                }}
            | Double {
                RightExpReal {
                    right_exp_double = fst $1, right_exp_pos = snd $1
                }}
            | Char {
                RightExpChar      {
                    right_exp_char = fst $1, right_exp_pos = snd $1
                }}
            | Boolean {
                RightExpBoolean   {
                    right_exp_bool = fst $1, right_exp_pos = snd $1
                }}
            | String {
                RightExpString    {
                    right_exp_string = fst $1,
                    right_exp_pos = snd $1                                                
                }}
            | FunctionCall {
                RightExpFunctionCall  {
                    call_name_right_exp = call_name $1,
                    call_params_right_exp = call_params $1,
                    right_exp_pos = (statement_pos $1) 
                }}
            | LeftExp {
                RightExpCopy {
                    left_exp_right_exp = $1, right_exp_pos = (left_exp_pos $1)
                }}

LeftExp :: { LeftExp }
LeftExp : Ident {
            LeftExpIdent {
                left_exp_name = $1, left_exp_pos = (ident_pos $1) 
            }}
        | LeftExp '[' ListRightExp ']' {
            LeftExpArrayAccess {
                array_name = $1, array_pos = $3, left_exp_pos = (left_exp_pos $1)
            }}
        | LeftExp '^' {
            LeftExpPointerValue {
                pointer_value = $1, left_exp_pos = (left_exp_pos $1)
            }}
        | LeftExp '@' {
            LeftExpPointerAddress {
                pointer_address = $1, left_exp_pos = (left_exp_pos $1) 
            }}

Type    :: { Tipi.Type }
Type    : BaseType { $1 }
        | CompositeType { $1 }

BaseType    :: { Tipi.Type }
BaseType    : 'integer' { Tipi.IntegerType }
            | 'real'    { Tipi.RealType }
            | 'char'    { Tipi.CharType }
            | 'boolean' { Tipi.BooleanType }
            | 'string'  { Tipi.StringType }

CompositeType   :: { Tipi.Type }
CompositeType   : 'array' '[' ListArrayDeclarationDim ']' 'of' BaseType {
                    Tipi.ArrayType { Tipi.aType = $6, Tipi.dimensions = $3 }}
                | '^' Type {
                    Tipi.PointerType { Tipi.pType = $2 }}

ListArrayDeclarationDim :: { [(Int, Int)] }
ListArrayDeclarationDim : {- empty -}                                     { [] }
                        | ArrayDeclarationDim                             { (:[]) $1 }
                        | ArrayDeclarationDim ',' ListArrayDeclarationDim { (:) $1 $3 }

ArrayDeclarationDim :: { (Int, Int) }
ArrayDeclarationDim : Integer '..' Integer { (fst $1, fst $3) }

WritePrimitive :: { WritePrimitive }
WritePrimitive  : 'writeInt' '(' RightExp ')' {
                    WriteInt  {
                        write_exp = $3, write_primitive_pos = (tokenLineCol $1)
                    }}
                | 'writeReal' '(' RightExp ')' {
                    WriteReal {
                        write_exp = $3, write_primitive_pos = (tokenLineCol $1) 
                    }}
                | 'writeChar' '(' RightExp ')' {
                    WriteChar {
                        write_exp = $3, write_primitive_pos = (tokenLineCol $1) 
                    }}
                | 'writeString' '(' RightExp ')' {
                    WriteString {
                        write_exp = $3, write_primitive_pos = (tokenLineCol $1)
                    }}
                
ReadPrimitive :: { ReadPrimitive }
ReadPrimitive   : 'readInt' '(' LeftExp ')'     {
                    ReadInt {
                        read_exp = $3, read_primitive_pos = (tokenLineCol $1)
                    }}
                | 'readReal' '(' LeftExp ')'    {
                    ReadReal {
                        read_exp = $3, read_primitive_pos = (tokenLineCol $1)
                    }}
                | 'readChar' '(' LeftExp ')'    {
                    ReadChar      {
                        read_exp = $3, read_primitive_pos = (tokenLineCol $1)
                    }}
                | 'readString' '(' LeftExp ')'  {
                    ReadString    {
                        read_exp = $3, read_primitive_pos = (tokenLineCol $1)
                    }}

--------------------------------------------------------------------------------------------------------------------------------------

{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
    Bad $ "syntax error at " ++ tokenPos ts ++
    case ts of
        []      -> []
        [Err _] -> " due to lexer error"
        t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}