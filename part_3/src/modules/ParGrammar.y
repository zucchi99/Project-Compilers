-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGrammar where
import AbsGrammar
import LexGrammar
import ErrM

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
  L_ident  { PT _ (TV $$) }
  L_integ  { PT _ (TI $$) }
  L_doubl  { PT _ (TD $$) }
  L_charac { PT _ (TC $$) }
  L_quoted { PT _ (TL $$) }

%%

Ident   :: { Ident }
Ident    : L_ident  { Ident $1 }

Integer :: { Integer }
Integer  : L_integ  { (read ( $1)) :: Integer }

Double  :: { Double }
Double   : L_doubl  { (read ( $1)) :: Double }

Char    :: { Char }
Char     : L_charac { (read ( $1)) :: Char }

String  :: { String }
String   : L_quoted {  $1 }

BeginKW :: { BeginKW }
BeginKW : 'begin' { AbsGrammar.KeyWordBegin }
EndKW :: { EndKW }
EndKW : 'end' { AbsGrammar.KeyWordEnd }
ProgramKW :: { ProgramKW }
ProgramKW : 'program' { AbsGrammar.KeyWordProgram }
FunctionKW :: { FunctionKW }
FunctionKW : 'function' { AbsGrammar.KeyWordFunction }
ProcedureKW :: { ProcedureKW }
ProcedureKW : 'procedure' { AbsGrammar.KeyWordProcedure }
ForwardKW :: { ForwardKW }
ForwardKW : 'forward' { AbsGrammar.KeyWordForward }
VarKW :: { VarKW }
VarKW : 'var' { AbsGrammar.KeyWordVar }
ConstKW :: { ConstKW }
ConstKW : 'const' { AbsGrammar.KeyWordConst }
IfKW :: { IfKW }
IfKW : 'if' { AbsGrammar.KeyWordIf }
ThenKW :: { ThenKW }
ThenKW : 'then' { AbsGrammar.KeyWordThen }
ElseKW :: { ElseKW }
ElseKW : 'else' { AbsGrammar.KeyWordElse }
WhileKW :: { WhileKW }
WhileKW : 'while' { AbsGrammar.KeyWordWhile }
ForKW :: { ForKW }
ForKW : 'for' { AbsGrammar.KeyWordFor }
ToKW :: { ToKW }
ToKW : 'to' { AbsGrammar.KeyWordTo }
DoKW :: { DoKW }
DoKW : 'do' { AbsGrammar.KeyWordDo }
RepeatKW :: { RepeatKW }
RepeatKW : 'repeat' { AbsGrammar.KeyWordRepeat }
UntilKW :: { UntilKW }
UntilKW : 'until' { AbsGrammar.KeyWordUntil }
IntKW :: { IntKW }
IntKW : 'integer' { AbsGrammar.KeyWordTypeInt }
RealKW :: { RealKW }
RealKW : 'real' { AbsGrammar.KeyWordTypeReal }
CharKW :: { CharKW }
CharKW : 'char' { AbsGrammar.KeyWordTypeChar }
BoolKW :: { BoolKW }
BoolKW : 'boolean' { AbsGrammar.KeyWordTypeBoolean }
StringKW :: { StringKW }
StringKW : 'string' { AbsGrammar.KeyWordTypeString }
ArrayKW :: { ArrayKW }
ArrayKW : 'array' { AbsGrammar.KeyWordTypeArray }
Program :: { Program }
Program : ProgramKW Ident ';' BlockWithDecl '.' { AbsGrammar.ProgramStart $1 $2 $4 }
BlockWithDecl :: { BlockWithDecl }
BlockWithDecl : ListDeclaration BlockExec { AbsGrammar.BlockWithDeclaration (reverse $1) $2 }
ListDeclaration :: { [Declaration] }
ListDeclaration : {- empty -} { [] }
                | ListDeclaration Declaration { flip (:) $1 $2 }
BlockExec :: { BlockExec }
BlockExec : BeginKW NonMandatoryTerminator ListStatement EndKW { AbsGrammar.BlockOnlyExecution $1 $2 $3 $4 }
ListStatement :: { [Statement] }
ListStatement : {- empty -} { [] }
              | Statement { (:[]) $1 }
              | Statement ';' ListStatement { (:) $1 $3 }
NonMandatoryTerminator :: { NonMandatoryTerminator }
NonMandatoryTerminator : {- empty -} { AbsGrammar.NonMandatoryTerminator1 }
                       | ';' { AbsGrammar.NonMandatoryTerminator2 }
Declaration :: { Declaration }
Declaration : CostantsBlock { AbsGrammar.DeclarationCostantsBlock $1 }
            | VariablesBlock { AbsGrammar.DeclarationVariablesBlock $1 }
            | FunctionForw { AbsGrammar.DeclarationFunctionForw $1 }
            | ProcedureForw { AbsGrammar.DeclarationProcedureForw $1 }
            | FunctionDecl { AbsGrammar.DeclarationFunctionDecl $1 }
            | ProcedureDecl { AbsGrammar.DeclarationProcedureDecl $1 }
CostantsBlock :: { CostantsBlock }
CostantsBlock : ConstKW ListConstantDecl { AbsGrammar.CostantsBlock1 $1 $2 }
ListConstantDecl :: { [ConstantDecl] }
ListConstantDecl : ConstantDecl ';' { (:[]) $1 }
                 | ConstantDecl ';' ListConstantDecl { (:) $1 $3 }
ConstantDecl :: { ConstantDecl }
ConstantDecl : Ident '=' RightExp { AbsGrammar.ConstantDeclaration $1 $3 }
VariablesBlock :: { VariablesBlock }
VariablesBlock : VarKW ListVariableDeclBlock { AbsGrammar.VariablesBlock1 $1 $2 }
ListVariableDeclBlock :: { [VariableDeclBlock] }
ListVariableDeclBlock : VariableDeclBlock ';' { (:[]) $1 }
                      | VariableDeclBlock ';' ListVariableDeclBlock { (:) $1 $3 }
ListVariableDeclFunc :: { [VariableDeclFunc] }
ListVariableDeclFunc : {- empty -} { [] }
                     | VariableDeclFunc { (:[]) $1 }
                     | VariableDeclFunc ';' ListVariableDeclFunc { (:) $1 $3 }
VariableDeclBlock :: { VariableDeclBlock }
VariableDeclBlock : ListIdent ':' Type InitAssign { AbsGrammar.VariableDeclarationInsideBlock $1 $3 $4 }
InitAssign :: { InitAssign }
InitAssign : {- empty -} { AbsGrammar.InitAssign1 }
           | '=' RightExp { AbsGrammar.InitAssign2 $2 }
DeclarationFunc :: { DeclarationFunc }
DeclarationFunc : {- empty -} { AbsGrammar.DeclarationFunc1 }
                | '(' ListVariableDeclFunc ')' { AbsGrammar.DeclarationFunc2 $2 }
VariableDeclFunc :: { VariableDeclFunc }
VariableDeclFunc : ListIdent ':' Type { AbsGrammar.VariableDeclarationInsideF $1 $3 }
ListIdent :: { [Ident] }
ListIdent : Ident { (:[]) $1 } | Ident ',' ListIdent { (:) $1 $3 }
FunctionSign :: { FunctionSign }
FunctionSign : FunctionKW Ident DeclarationFunc ':' Type ';' { AbsGrammar.FunctionSignature $1 $2 $3 $5 }
ProcedureSign :: { ProcedureSign }
ProcedureSign : ProcedureKW Ident DeclarationFunc ';' { AbsGrammar.ProcedureSignature $1 $2 $3 }
FunctionDecl :: { FunctionDecl }
FunctionDecl : FunctionSign BlockWithDecl ';' { AbsGrammar.FunctionDeclaration $1 $2 }
ProcedureDecl :: { ProcedureDecl }
ProcedureDecl : ProcedureSign BlockWithDecl ';' { AbsGrammar.ProcedureDeclaration $1 $2 }
FunctionForw :: { FunctionForw }
FunctionForw : FunctionSign ForwardKW ';' { AbsGrammar.FunctionForward $1 $2 }
ProcedureForw :: { ProcedureForw }
ProcedureForw : ProcedureSign ForwardKW ';' { AbsGrammar.ProcedureForward $1 $2 }
FunctionCall :: { FunctionCall }
FunctionCall : Ident '(' ListRightExp ')' { AbsGrammar.FunctionCall $1 $3 }
ProcedureCall :: { ProcedureCall }
ProcedureCall : Ident '(' ListRightExp ')' { AbsGrammar.ProcedureCall $1 $3 }
ListRightExp :: { [RightExp] }
ListRightExp : {- empty -} { [] }
             | RightExp { (:[]) $1 }
             | RightExp ',' ListRightExp { (:) $1 $3 }
Statement :: { Statement }
Statement : BlockExec { AbsGrammar.StatementBlock $1 }
          | IfKW RightExp ThenKW Statement ElseBlock { AbsGrammar.StatementIf $1 $2 $3 $4 $5 }
          | ForKW Assign ToKW RightExp DoKW Statement { AbsGrammar.StatementFor $1 $2 $3 $4 $5 $6 }
          | WhileKW RightExp DoKW Statement { AbsGrammar.StatementWhile $1 $2 $3 $4 }
          | RepeatKW ListStatement UntilKW RightExp { AbsGrammar.StatementRepeatUntil $1 $2 $3 $4 }
          | Assign { AbsGrammar.StatementAssign $1 }
          | FunctionCall { AbsGrammar.StatementFunctionCall $1 }
          | ProcedureCall { AbsGrammar.StatementProcedureCall $1 }
          | WritePrimitive { AbsGrammar.StatementWrite $1 }
          | ReadPrimitive { AbsGrammar.StatementRead $1 }
ElseBlock :: { ElseBlock }
ElseBlock : {- empty -} { AbsGrammar.ElseBlock1 }
          | ElseKW Statement { AbsGrammar.ElseBlock2 $1 $2 }
Assign :: { Assign }
Assign : LeftExp ':=' RightExp { AbsGrammar.VariableAssignment $1 $3 }
RightExp :: { RightExp }
RightExp : RightExp1 { $1 }
         | RightExp 'or' RightExp1 { AbsGrammar.RightExpOr $1 $3 }
RightExp1 :: { RightExp }
RightExp1 : RightExp2 { $1 }
          | RightExp1 'and' RightExp2 { AbsGrammar.RightExpAnd $1 $3 }
RightExp2 :: { RightExp }
RightExp2 : RightExp3 { $1 }
          | RightExp2 '>' RightExp3 { AbsGrammar.RightExpGreater $1 $3 }
          | RightExp2 '<' RightExp3 { AbsGrammar.RightExpLess $1 $3 }
          | RightExp2 '>=' RightExp3 { AbsGrammar.RightExpGreaterEqual $1 $3 }
          | RightExp2 '<=' RightExp3 { AbsGrammar.RightExpLessEqual $1 $3 }
          | RightExp2 '=' RightExp3 { AbsGrammar.RightExpEqual $1 $3 }
RightExp3 :: { RightExp }
RightExp3 : RightExp4 { $1 }
          | RightExp3 '+' RightExp4 { AbsGrammar.RightExpPlus $1 $3 }
          | RightExp3 '-' RightExp4 { AbsGrammar.RightExpMinus $1 $3 }
RightExp4 :: { RightExp }
RightExp4 : RightExp5 { $1 }
          | RightExp4 '*' RightExp5 { AbsGrammar.RightExpTimes $1 $3 }
          | RightExp4 '/' RightExp5 { AbsGrammar.RightExpDivide $1 $3 }
          | RightExp4 'mod' RightExp5 { AbsGrammar.RightExpMod $1 $3 }
          | RightExp4 'div' RightExp5 { AbsGrammar.RightExpDiv $1 $3 }
RightExp5 :: { RightExp }
RightExp5 : RightExp6 { $1 }
          | RightExp5 '**' RightExp6 { AbsGrammar.RightExpPower $1 $3 }
RightExp6 :: { RightExp }
RightExp6 : RightExp7 { $1 }
          | 'not' RightExp7 { AbsGrammar.RightExpNot $2 }
          | '-' RightExp7 { AbsGrammar.RightExpMinusUnary $2 }
          | '+' RightExp7 { AbsGrammar.RightExpPlusUnary $2 }
RightExp7 :: { RightExp }
RightExp7 : '(' RightExp ')' { $2 }
          | Integer { AbsGrammar.RightExpInteger $1 }
          | Double { AbsGrammar.RightExpReal $1 }
          | Boolean { AbsGrammar.RightExpBoolean $1 }
          | Char { AbsGrammar.RightExpChar $1 }
          | String { AbsGrammar.RightExpString $1 }
          | FunctionCall { AbsGrammar.RightExpFunctionCall $1 }
          | LeftExp { AbsGrammar.RightExpCopy $1 }
LeftExp :: { LeftExp }
LeftExp : Ident { AbsGrammar.LeftExpIdent $1 }
        | LeftExp '[' ListRightExp ']' { AbsGrammar.LeftExpArrayAccess $1 $3 }
        | LeftExp '^' { AbsGrammar.LeftExpPointerValue $1 }
        | LeftExp '@' { AbsGrammar.LeftExpPointerAddress $1 }
Type :: { Type }
Type : BaseType { AbsGrammar.TypeBaseType $1 }
     | CompositeType { AbsGrammar.TypeCompositeType $1 }
BaseType :: { BaseType }
BaseType : IntKW { AbsGrammar.BaseTypeIntKW $1 }
         | RealKW { AbsGrammar.BaseTypeRealKW $1 }
         | CharKW { AbsGrammar.BaseTypeCharKW $1 }
         | BoolKW { AbsGrammar.BaseTypeBoolKW $1 }
         | StringKW { AbsGrammar.BaseTypeStringKW $1 }
Boolean :: { Boolean }
Boolean : 'true' { AbsGrammar.Boolean_true }
        | 'false' { AbsGrammar.Boolean_false }
CompositeType :: { CompositeType }
CompositeType : ArrayKW '[' ListArrayDeclarationDim ']' 'of' BaseType { AbsGrammar.CompTypeArray $1 $3 $6 }
              | '^' Type { AbsGrammar.CompTypePointer $2 }
ListArrayDeclarationDim :: { [ArrayDeclarationDim] }
ListArrayDeclarationDim : {- empty -} { [] }
                        | ArrayDeclarationDim { (:[]) $1 }
                        | ArrayDeclarationDim ',' ListArrayDeclarationDim { (:) $1 $3 }
ArrayDeclarationDim :: { ArrayDeclarationDim }
ArrayDeclarationDim : RightExp '..' RightExp { AbsGrammar.ArrayDeclarationDim $1 $3 }
WritePrimitive :: { WritePrimitive }
WritePrimitive : 'writeInt' '(' RightExp ')' { AbsGrammar.WriteInt $3 }
               | 'writeReal' '(' RightExp ')' { AbsGrammar.WriteReal $3 }
               | 'writeChar' '(' RightExp ')' { AbsGrammar.WriteChar $3 }
               | 'writeString' '(' RightExp ')' { AbsGrammar.WriteString $3 }
ReadPrimitive :: { ReadPrimitive }
ReadPrimitive : 'readInt' '(' LeftExp ')' { AbsGrammar.ReadInt $3 }
              | 'readReal' '(' LeftExp ')' { AbsGrammar.ReadReal $3 }
              | 'readChar' '(' LeftExp ')' { AbsGrammar.ReadChar $3 }
              | 'readString' '(' LeftExp ')' { AbsGrammar.ReadString $3 }
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

