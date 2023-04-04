-- Haskell data types for the abstract syntax.
-- File: AbsGrammar.hs. Generate by the BNF converter and modified by hand.

-- to compile this file, use the command:
-- ghci AbstractSyntax.hs 

module AbstractSyntax where

-- ___________ TYPE FOR ABSTRACT SYNTAX  ___________

-- identifier for variables, functions, procedures, ...
data Ident = Ident                          { id_name :: String, ident_pos :: (Int, Int) }
    deriving (Show)

-- starting of the program
data Program = ProgramStart                 { program_name :: Ident, program_block_decl :: BlockWithDecl, program_pos :: (Int, Int) }
    deriving (Show)

-- Block structures
data BlockWithDecl = BlockWithDeclaration   { block_declarations :: [Declaration], block_exec :: BlockExec, block_with_decl_pos :: (Int, Int) }
    deriving (Show)

data BlockExec = BlockOnlyExecution         { statements :: [Statement], block_exec_pos :: (Int, Int) }
    deriving (Show)

-- Declarations
data Declaration
    = DeclarationCostantsBlock              { costant_declarations :: [ConstantDecl], declaration_pos :: (Int, Int) }
    | DeclarationVariablesBlock             { variable_declarations :: [VariableDecl], declaration_pos :: (Int, Int) }
    -- NB v_value in params must be all nothing !               (OR we can acept default values)
    -- NB (f_body == Nothing) <==> is forward
    | DeclarationFunction                   { declaration_name :: Ident, declaration_params :: [VariableDecl], function_type :: Type, declaration_body_maybe :: Maybe BlockWithDecl, declaration_pos :: (Int, Int) }
    -- NB v_value in params must be all nothing !               (OR we can acept default values)
    -- NB (f_body == Nothing) <==> is forward
    | DeclarationProcedure                  { declaration_name :: Ident, declaration_params :: [VariableDecl], declaration_body_maybe :: Maybe BlockWithDecl, declaration_pos :: (Int, Int) }
    deriving (Show)

data ConstantDecl 
    = ConstantDeclaration                   { costant_name :: Ident, costant_type :: BaseType, costant_value :: RightExp, constant_decl_pos :: (Int, Int) }
    deriving (Show)

data VariableDecl 
    = VariableDecl                          { variable_name :: Ident, variable_type :: Type, variable_value_maybe :: Maybe RightExp, variable_decl_pos :: (Int, Int) }
    deriving (Show)

-- Statements
data Statement
    = StatementBlock                        { block :: BlockExec, statement_pos :: (Int, Int) }
    | StatementIf                           { condition :: RightExp, then_body :: Statement, else_body_maybe :: Maybe ElseBlock, statement_pos :: (Int, Int) }
    | StatementFor                          { condition :: RightExp, then_body :: Statement, for_var :: Assign, statement_pos :: (Int, Int) }
    | StatementWhile                        { condition :: RightExp, then_body :: Statement, statement_pos :: (Int, Int) }
    | StatementRepeatUntil                  { condition :: RightExp, then_body :: Statement, statement_pos :: (Int, Int) }
    | StatementAssign                       { assign :: Assign, statement_pos :: (Int, Int) }
    | StatementFunctionCall                 { call_name :: Ident, call_params :: [RightExp], statement_pos :: (Int, Int) }
    | StatementProcedureCalll               { call_name :: Ident, call_params :: [RightExp], statement_pos :: (Int, Int) }
    | StatementWrite                        { writePrimitive :: WritePrimitive, statement_pos :: (Int, Int) }
    | StatementRead                         { readPrimitive :: ReadPrimitive, statement_pos :: (Int, Int)}
    deriving (Show)

data ElseBlock 
    = ElseBlock                             { else_body :: Statement, else_block_pos :: (Int, Int) }
    deriving (Show)

data RightExp
    = RightExpOr                            { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpAnd                           { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpGreater                       { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpLess                          { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpGreaterEqual                  { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpLessEqual                     { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpEqual                         { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpPlus                          { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpMinus                         { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpTimes                         { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpDivide                        { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpMod                           { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpDiv                           { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpPower                         { sx, dx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpNot                           { sx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpMinusUnary                    { sx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpPlusUnary                     { sx :: RightExp , right_exp_pos  :: (Int, Int) }
    | RightExpInteger                       { value :: BaseType, right_exp_pos  :: (Int, Int) }
    | RightExpReal                          { value :: BaseType, right_exp_pos  :: (Int, Int) }
    | RightExpBoolean                       { value :: BaseType, right_exp_pos  :: (Int, Int) }
    | RightExpChar                          { value :: BaseType, right_exp_pos  :: (Int, Int) }
    | RightExpString                        { value :: BaseType, right_exp_pos  :: (Int, Int) }
    | RightExpFunctionCall                  { call_name_right_exp :: Ident, call_params_right_exp :: [RightExp], right_exp_pos  :: (Int, Int) }
    | RightExpCopy                          { left_exp_right_exp :: LeftExp, right_exp_pos  :: (Int, Int) }
    deriving (Show)

data LeftExp
    = LeftExpIdent                          { left_exp_name :: Ident , left_exp_pos :: (Int, Int) }
    | LeftExpArrayAccess                    { array_name :: LeftExp, array_pos :: [RightExp], left_exp_pos  :: (Int, Int) }
    | LeftExpPointerValue                   { pointer_value :: LeftExp, left_exp_pos  :: (Int, Int) }
    | LeftExpPointerAddress                 { pointer_address :: LeftExp, left_exp_pos  :: (Int, Int) }
    deriving (Show)

data Assign = VariableAssignment            { left_exp_assignment :: LeftExp, right_exp_assignment :: RightExp, assign_pos :: (Int, Int) }
    deriving (Show)

-- Types
data Type 
    = TypeBaseType                          { base_type :: BaseType, type_pos :: (Int, Int) }
    | TypeCompositeType                     { composite_type :: CompositeType, type_pos :: (Int, Int) }
    deriving (Show)

data BaseType
    = BaseType_integer                      { type_int :: Int, base_type_pos  :: (Int, Int) }
    | BaseType_real                         { type_real :: Double, base_type_pos  :: (Int, Int) }
    | BaseType_char                         { type_char :: Char, base_type_pos  :: (Int, Int) }
    | BaseType_boolean                      { type_bool :: Bool, base_type_pos  :: (Int, Int) }
    | BaseType_string                       { type_string :: String, base_type_pos  :: (Int, Int) }
    deriving (Show)

data CompositeType
    = CompTypeArray                         { declaration_dim :: [ArrayDeclarationDim], arrayType :: BaseType, composite_type_pos :: (Int, Int) }
    | CompTypePointer                       { pointer_type :: Type, composite_type_pos :: (Int, Int) }
    deriving (Show)

data ArrayDeclarationDim 
    = ArrayDeclarationDim                   { array_declaration_dim :: (RightExp, RightExp), array_declaration_dim_pos :: (Int, Int) }
    deriving (Show)

data WritePrimitive
    = WriteInt                              { write_exp :: RightExp, writePrimitive_pos :: (Int, Int) }
    | WriteReal                             { write_exp :: RightExp, writePrimitive_pos :: (Int, Int) }
    | WriteChar                             { write_exp :: RightExp, writePrimitive_pos :: (Int, Int) }
    | WriteString                           { write_exp :: RightExp, writePrimitive_pos :: (Int, Int) }
    deriving (Show)

data ReadPrimitive
    = ReadInt                               { read_exp :: LeftExp, readPrimitive_pos :: (Int, Int) }
    | ReadReal                              { read_exp :: LeftExp, readPrimitive_pos :: (Int, Int) }
    | ReadChar                              { read_exp :: LeftExp, readPrimitive_pos :: (Int, Int) }
    | ReadString                            { read_exp :: LeftExp, readPrimitive_pos :: (Int, Int) }
    deriving (Show)

