-- Haskell data types for the abstract syntax.
-- File: AbsGrammar.hs. Generate by the BNF converter and modified by hand.

-- to compile this file, use the command:
-- ghci AbstractSyntax.hs 


module AbstractSyntax where
import qualified Types as T
import qualified Env as E

-- ___________ TYPE FOR ABSTRACT SYNTAX  ___________

-- identifier for variables, functions, procedures, ...
data Ident = Ident                          { id_name :: String, ident_pos :: (Int, Int), ident_env :: E.Env, ident_errors :: [String] }
    deriving (Show)

-- starting of the program
data Program = ProgramStart                 { program_name :: Ident, program_block :: Block, program_pos :: (Int, Int), program_env :: E.Env, program_errors :: [String] }
    deriving (Show)

-- Block structures
data Block = Block                          { block_declarations :: [Declaration], statements :: [Statement], block_pos :: (Int, Int), block_env :: E.Env, block_errors :: [String] }
    deriving (Show)

-- Declarations
data Declaration
    = DeclarationCostant                    { costant_name :: Ident, costant_type_maybe :: Maybe T.Type, costant_value :: RightExp, declaration_pos :: (Int, Int), declaration_env :: E.Env, declaration_errors :: [String] }
    | DeclarationVariable                   { variable_name :: Ident, variable_type :: T.Type, variable_value_maybe :: Maybe RightExp, declaration_pos :: (Int, Int), declaration_env :: E.Env, declaration_errors :: [String] }
    -- NB v_value in params must be all nothing !               (OR we can accept default values)
    -- NB (f_body == Nothing) <==> is forward
    | DeclarationFunction                   { declaration_name :: Ident, declaration_params :: [Declaration], function_type :: T.Type, declaration_body_maybe :: Maybe Block, declaration_pos :: (Int, Int), declaration_env :: E.Env, declaration_errors :: [String] }
    -- NB v_value in params must be all nothing !               (OR we can accept default values)
    -- NB (f_body == Nothing) <==> is forward
    | DeclarationProcedure                  { declaration_name :: Ident, declaration_params :: [Declaration], declaration_body_maybe :: Maybe Block, declaration_pos :: (Int, Int), declaration_env :: E.Env, declaration_errors :: [String] }
    deriving (Show)

-- Statements
data Statement
    = StatementBlock                        { block :: Block, statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
    | StatementIf                           { condition :: RightExp, then_body :: Statement, else_body_maybe :: Maybe ElseBlock, statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
    | StatementFor                          { condition :: RightExp, then_body :: Statement, for_var :: Assign, statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
    | StatementWhile                        { condition :: RightExp, then_body :: Statement, statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
    | StatementRepeatUntil                  { condition :: RightExp, then_body :: Statement, statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
    | StatementAssign                       { assign :: Assign, statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
    | StatementFuncProc                     { call_name :: Ident, call_params :: [RightExp], statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
    | StatementWrite                        { write_primitive :: WritePrimitive, statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
    | StatementBreak                        { statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
    | StatementContinue                     { statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
    | StatementRead                         { read_primitive :: ReadPrimitive, statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
    deriving (Show)

data ElseBlock = ElseBlock                  { else_body :: Statement, else_block_pos :: (Int, Int), else_block_env :: E.Env, else_block_errors :: [String] }
    deriving (Show)

data RightExp
    = RightExpOr                            { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpAnd                           { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpGreater                       { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpLess                          { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpGreaterEqual                  { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpLessEqual                     { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpEqual                         { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpPlus                          { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpMinus                         { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpTimes                         { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpDivide                        { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpMod                           { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpDiv                           { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpPower                         { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpNot                           { dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpMinusUnary                    { dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpPlusUnary                     { dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpInteger                       { right_exp_int :: Int, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpReal                          { right_exp_double :: Double, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpBoolean                       { right_exp_bool :: Bool, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpChar                          { right_exp_char :: Char, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpString                        { right_exp_string :: String, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpFuncProcCall                  { call_name_right_exp :: Ident, call_params_right_exp :: [RightExp], right_exp_pos :: (Int, Int), right_exp_env :: E.Env }
    | RightExpCopy                          { left_exp_right_exp :: LeftExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
    | RightExpCoercion                      { right_exp_coercion :: RightExp, right_exp_from_type :: T.Type, right_exp_to_type :: T.Type, right_exp_pos :: (Int, Int), right_exp_env :: E.Env, right_exp_errors :: [String] }
    deriving (Show)

data LeftExp
    = LeftExpIdent                          { left_exp_name :: Ident , left_exp_pos :: (Int, Int), left_exp_type :: T.Type, left_exp_env :: E.Env, left_exp_errors :: [String] }
    | LeftExpArrayAccess                    { array_name :: LeftExp, array_pos :: [RightExp], left_exp_type :: T.Type, left_exp_pos  :: (Int, Int), left_exp_env :: E.Env, left_exp_errors :: [String] }
    | LeftExpPointerValue                   { pointer_value :: LeftExp, left_exp_pos  :: (Int, Int), left_exp_type :: T.Type, left_exp_env :: E.Env, left_exp_errors :: [String] }
    | LeftExpPointerAddress                 { pointer_address :: LeftExp, left_exp_pos  :: (Int, Int), left_exp_type :: T.Type, left_exp_env :: E.Env, left_exp_errors :: [String] }
    deriving (Show)

data Assign = VariableAssignment            { left_exp_assignment :: LeftExp, right_exp_assignment :: RightExp, assign_pos :: (Int, Int), assign_env :: E.Env, assign_errors :: [String] }
    deriving (Show)

data WritePrimitive
    = WriteInt                              { write_exp :: RightExp, write_primitive_pos :: (Int, Int), write_primitive_env :: E.Env, write_primitive_errors :: [String] }
    | WriteReal                             { write_exp :: RightExp, write_primitive_pos :: (Int, Int), write_primitive_env :: E.Env, write_primitive_errors :: [String] }
    | WriteChar                             { write_exp :: RightExp, write_primitive_pos :: (Int, Int), write_primitive_env :: E.Env, write_primitive_errors :: [String] }
    | WriteString                           { write_exp :: RightExp, write_primitive_pos :: (Int, Int), write_primitive_env :: E.Env, write_primitive_errors :: [String] }
    deriving (Show)

data ReadPrimitive
    = ReadInt                               { read_exp :: LeftExp, read_primitive_pos :: (Int, Int), read_primitive_env :: E.Env, read_primitive_errors :: [String] }
    | ReadReal                              { read_exp :: LeftExp, read_primitive_pos :: (Int, Int), read_primitive_env :: E.Env, read_primitive_errors :: [String] }
    | ReadChar                              { read_exp :: LeftExp, read_primitive_pos :: (Int, Int), read_primitive_env :: E.Env, read_primitive_errors :: [String] }
    | ReadString                            { read_exp :: LeftExp, read_primitive_pos :: (Int, Int), read_primitive_env :: E.Env, read_primitive_errors :: [String] }
    deriving (Show)

