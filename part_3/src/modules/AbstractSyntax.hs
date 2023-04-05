-- Haskell data types for the abstract syntax.
-- File: AbsGrammar.hs. Generate by the BNF converter and modified by hand.

-- to compile this file, use the command:
-- ghci AbstractSyntax.hs 


module AbstractSyntax where
import qualified Types as Tipi

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
    = DeclarationCostant                    { costant_name :: Ident, costant_type_maybe :: Maybe Tipi.Type, costant_value :: RightExp, declaration_pos :: (Int, Int) }
    | DeclarationVariable                   { variable_name :: Ident, variable_type :: Tipi.Type, variable_value_maybe :: Maybe RightExp, declaration_pos :: (Int, Int) }
    -- NB v_value in params must be all nothing !               (OR we can acept default values)
    -- NB (f_body == Nothing) <==> is forward
    | DeclarationFunction                   { declaration_name :: Ident, declaration_params :: [Declaration], function_type :: Tipi.Type, declaration_body_maybe :: Maybe BlockWithDecl, declaration_pos :: (Int, Int) }
    -- NB v_value in params must be all nothing !               (OR we can acept default values)
    -- NB (f_body == Nothing) <==> is forward
    | DeclarationProcedure                  { declaration_name :: Ident, declaration_params :: [Declaration], declaration_body_maybe :: Maybe BlockWithDecl, declaration_pos :: (Int, Int) }
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
    | StatementProcedureCall                { call_name :: Ident, call_params :: [RightExp], statement_pos :: (Int, Int) }
    | StatementWrite                        { write_primitive :: WritePrimitive, statement_pos :: (Int, Int) }
    | StatementRead                         { read_primitive :: ReadPrimitive, statement_pos :: (Int, Int)}
    deriving (Show)

data ElseBlock = ElseBlock                  { else_body :: Statement, else_block_pos :: (Int, Int) }
    deriving (Show)

data RightExp
    = RightExpOr                            { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpAnd                           { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpGreater                       { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpLess                          { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpGreaterEqual                  { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpLessEqual                     { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpEqual                         { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpPlus                          { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpMinus                         { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpTimes                         { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpDivide                        { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpMod                           { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpDiv                           { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpPower                         { sx, dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpNot                           { dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpMinusUnary                    { dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpPlusUnary                     { dx :: RightExp , right_exp_pos :: (Int, Int) }
    | RightExpInteger                       { right_exp_int :: Int, right_exp_pos :: (Int, Int) }
    | RightExpReal                          { right_exp_double :: Double, right_exp_pos :: (Int, Int) }
    | RightExpBoolean                       { right_exp_bool :: Bool, right_exp_pos :: (Int, Int) }
    | RightExpChar                          { right_exp_char :: Char, right_exp_pos :: (Int, Int) }
    | RightExpString                        { right_exp_string :: String, right_exp_pos :: (Int, Int) }
    | RightExpFunctionCall                  { call_name_right_exp :: Ident, call_params_right_exp :: [RightExp], right_exp_pos :: (Int, Int) }
    | RightExpCopy                          { left_exp_right_exp :: LeftExp, right_exp_pos :: (Int, Int) }
    deriving (Show)

data LeftExp
    = LeftExpIdent                          { left_exp_name :: Ident , left_exp_pos :: (Int, Int) }
    | LeftExpArrayAccess                    { array_name :: LeftExp, array_pos :: [RightExp], left_exp_pos  :: (Int, Int) }
    | LeftExpPointerValue                   { pointer_value :: LeftExp, left_exp_pos  :: (Int, Int) }
    | LeftExpPointerAddress                 { pointer_address :: LeftExp, left_exp_pos  :: (Int, Int) }
    deriving (Show)

data Assign = VariableAssignment            { left_exp_assignment :: LeftExp, right_exp_assignment :: RightExp, assign_pos :: (Int, Int) }
    deriving (Show)

data WritePrimitive
    = WriteInt                              { write_exp :: RightExp, write_primitive_pos :: (Int, Int) }
    | WriteReal                             { write_exp :: RightExp, write_primitive_pos :: (Int, Int) }
    | WriteChar                             { write_exp :: RightExp, write_primitive_pos :: (Int, Int) }
    | WriteString                           { write_exp :: RightExp, write_primitive_pos :: (Int, Int) }
    deriving (Show)

data ReadPrimitive
    = ReadInt                               { read_exp :: LeftExp, read_primitive_pos :: (Int, Int) }
    | ReadReal                              { read_exp :: LeftExp, read_primitive_pos :: (Int, Int) }
    | ReadChar                              { read_exp :: LeftExp, read_primitive_pos :: (Int, Int) }
    | ReadString                            { read_exp :: LeftExp, read_primitive_pos :: (Int, Int) }
    deriving (Show)
