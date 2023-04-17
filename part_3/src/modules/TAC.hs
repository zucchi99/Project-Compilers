
-- ghci -outputdir src/bin src/modules/TAC.hs src/modules/Types.hs src/modules/ErrorMessage.hs src/modules/AbstractSyntax.hs

module TAC where

import qualified AbstractSyntax as AS
import qualified Types as T
import qualified Env as E

-- ___________ PRIMITIVE TYPE ___________

-- primitive types
data PrimType =     
    TypeInt
    | TypeReal
    | TypeChar
    | TypeBool
    | TypeAddr
    deriving (Show)

-- ___________ PRIMITIVE ADDRESS TYPE ___________

--                                    AddrRight
--            AddrLeft                                        Address
-- AddrProgramVar  AddrTempVar              AddressInt AddressReal AddressChar AddressBool

{-

-- works but shitty

-- variable defined in source code
newtype AddrProgramVar = AddrProgramVar String
    deriving (Show)

-- temporary variable defined by compiler
newtype AddrTempVar = AddrTempVar String
    deriving (Show)

-- primitive types of TAC
data Address = 
    AddressInt Int
    | AddressReal Double
    | AddressChar Char
    | AddressBool Bool
    deriving (Show)

-- left expression in TAC code
data AddrLeft = 
    AddrProgramVarSuper AddrProgramVar
    | AddrTempVarSuper  AddrTempVar
    deriving (Show)

-- right expression in TAC code
data AddrRight = 
    AddrLeftSuper  AddrLeft
    | AddressSuper Address
    deriving (Show)

data Instruction = 
    -- l = r1 bin_op r2
    BinaryAssignment        { l :: AddrLeft, r1,r2 :: AddrRight, assign_type :: PrimType, bin_op :: BinaryOp }
    -- l = un_op r
    | UnaryAssignment       { l :: AddrLeft,     r :: AddrRight, assign_type :: PrimType, un_op :: UnaryOp }
    -- l = r
    | NullAssignment        { l :: AddrLeft,     r :: AddrRight, assign_type :: PrimType }
    -- goto label
    | Jump                  { goto :: String }
    -- if r goto label
    | JumpIfTrue            { goto :: String, cond :: AddrRight }
    -- ifFalse r goto label
    | JumpIfFalse           { goto :: String, cond :: AddrRight }
    -- if r1 rel r2 goto label
    | JumpConditional       { goto :: String, r1,r2 :: AddrRight, rel_op :: RelationalOp }
    -- l = array[i]
    | ReadFromArray         { array :: AddrProgramVar, i,r :: AddrRight, assign_type :: PrimType }
    -- array[i] = r
    | WriteToArray          { array :: AddrProgramVar, i,r :: AddrRight, assign_type :: PrimType }
    -- l = @id (&id in C)
    | ReadPointerAddress    { l      :: AddrLeft, pointer :: AddrProgramVar }
    -- l = ^l (*id in C)
    | ReadPointerValue      { l1, l2 :: AddrLeft }
    -- ^l = r (*id in C)
    | WritePointerValue     { l      :: AddrLeft, r :: AddrRight }
    -- param r
    | Parameter             { param :: AddrRight, param_type :: PrimType }
    -- pcall id, num_params
    | ProcCall              { p_name :: String, num_params :: Int }
    -- l = fcall id, num_params
    | FunCall               { f_name :: String, num_params :: Int, l :: AddrLeft,  assign_type :: PrimType }
    -- return (for procedures)
    | Return
    -- return r (for functions)
    | RetVal                { value :: AddrRight, return_type :: PrimType }
    deriving (Show)

-}

data Address = 
    AddressInt          Int
    | AddressReal       Double
    | AddressChar       Char
    | AddressBool       Bool
    | AddressProgramVar String
    | AddressTempVar    String
    deriving (Show)

instance Eq Address where
    AddressInt        _   == AddressInt        _   = True
    AddressReal       _   == AddressReal       _   = True
    AddressChar       _   == AddressChar       _   = True
    AddressBool       _   == AddressBool       _   = True
    AddressProgramVar _   == AddressProgramVar _   = True
    AddressTempVar    _   == AddressTempVar    _   = True
    _                     == _                     = False

is_address_left :: Address -> Bool
is_address_left addr = ((addr == (AddressProgramVar "")) || (addr == (AddressTempVar "")))

is_address_program :: Address -> Bool
is_address_program addr = (addr == (AddressProgramVar ""))


-- ___________ SET OF INSTRUCTIONS ___________

-- NB:
-- l, l1, l2      ==> is_address_left
-- array, pointer ==> is_address_program

data Instruction = 
    -- l = r1 bin_op r2
    BinaryAssignment        { l :: Address, r1,r2 :: Address, assign_type :: PrimType, bin_op :: BinaryOp }
    -- l = un_op r
    | UnaryAssignment       { l :: Address,     r :: Address, assign_type :: PrimType, un_op :: UnaryOp }
    -- l = r
    | NullAssignment        { l :: Address,     r :: Address, assign_type :: PrimType }
    -- goto label
    | Jump                  { goto :: String }
    -- if r goto label
    | JumpIfTrue            { goto :: String, cond :: Address }
    -- ifFalse r goto label
    | JumpIfFalse           { goto :: String, cond :: Address }
    -- if r1 rel r2 goto label
    | JumpConditional       { goto :: String, r1,r2 :: Address, rel_op :: RelationalOp }
    -- l = array[i]
    | ReadFromArray         { array :: Address, i,r :: Address, assign_type :: PrimType }
    -- array[i] = r
    | WriteToArray          { array :: Address, i,r :: Address, assign_type :: PrimType }
    -- l = @id (&id in C)
    | ReadPointerAddress    { l      :: Address, pointer :: Address }
    -- l = ^l (*id in C)
    | ReadPointerValue      { l1, l2 :: Address }
    -- ^l = r (*id in C)
    | WritePointerValue     { l      :: Address, r :: Address }
    -- param r
    | Parameter             { param :: Address, param_type :: PrimType }
    -- pcall id, num_params
    | ProcCall              { p_name :: String, num_params :: Int }
    -- l = fcall id, num_params
    | FunCall               { f_name :: String, num_params :: Int, l :: Address,  assign_type :: PrimType }
    -- return (for procedures)
    | Return
    -- return r (for functions)
    | RetVal                { value :: Address, return_type :: PrimType }
    deriving (Show)

-- ___________ SET OF OPERATIONS ___________

data UnaryOp =
    Coerce          { type_from, type_to    :: PrimType }
    | Negate        { negation_type         :: PrimType }
    | Not 
    deriving (Show)

data BinaryOp = 
    Sum             { binary_type           :: PrimType }
    | Subtract      { binary_type           :: PrimType }
    | Multiply      { binary_type           :: PrimType }
    | Divide        { binary_type           :: PrimType }
    | Remainder     { binary_type           :: PrimType }
    | Power         { binary_type           :: PrimType }
    | AddressSum
    deriving (Show)

data RelationalOp = 
    GreaterThan 
    | GreaterEqual
    | LessThan    
    | LessEqual   
    | Equal       
    | NotEqual    
    deriving (Show)

-- ___________ TAC CODE TYPE ___________

data BlockType = 
    StartBlockType                                                              -- start computation (invoke main)
    | MainBlockType { pos :: (Int, Int), is_start :: Bool }                     -- block for main code declaration
    | FuncBlockType { pos :: (Int, Int), is_start :: Bool, fun_name :: String } -- block for function declaration
    | ProcBlockType { pos :: (Int, Int), is_start :: Bool, prc_name :: String } -- block for procedure declaration
    | TempBlockType { b_idx :: Int }                                            -- block defined by compiler
    | StringBlockType { b_idx :: Int }                                          -- block for storage a string literal

data Block = Block {
    block_name  :: String,
    code        :: [ Instruction ]
} deriving (Show)

data State = State { 
    tac                          :: [ Block ],
    strings                      :: [ String ],
    temp_idx, block_idx, str_idx :: Int
} deriving (Show)

-- ___________ FUNCTIONS ___________

{-
opposite_relational_operator :: RelationalOp -> RelationalOp
opposite_relational_operator GreaterThan  = LessEqual
opposite_relational_operator LessEqual    = GreaterThan
opposite_relational_operator GreaterEqual = LessThan 
opposite_relational_operator LessThan     = GreaterEqual    
opposite_relational_operator Equal        = NotEqual 
opposite_relational_operator NotEqual     = Equal    
-}

empty_block :: Block
empty_block = (Block "" [])

empty_state :: State
empty_state = (State [] [] 0 0 0)

initialize_state :: (Int, Int) -> (String, State)
initialize_state main_pos = 
    let start_name = make_block_label StartBlockType
        main_type  = (MainBlockType main_pos True)
        main_name  = make_block_label main_type
        s          = add_block (State [] [] 0 0 0) StartBlockType []
        s1         = add_block s main_type []
        s2         = out s1 start_name (Jump main_name)
    in  (main_name, s2)
   
to_primitive_relational_operator :: AS.RightExp -> RelationalOp
to_primitive_relational_operator (AS.RightExpLess {}) = LessThan
to_primitive_relational_operator (AS.RightExpGreater {}) = GreaterThan
to_primitive_relational_operator (AS.RightExpLessEqual {}) = LessEqual
to_primitive_relational_operator (AS.RightExpGreaterEqual {}) = GreaterEqual
to_primitive_relational_operator (AS.RightExpEqual {}) = Equal
-- to_primitive_relational_operator (AS.RightExpNotEqual {}) = NotEqual

to_primitive_math_binary_operator :: AS.RightExp -> BinaryOp
to_primitive_math_binary_operator r_exp = 
    let t = to_primitive_type (AS.right_exp_type r_exp)
    in  case r_exp of
        (AS.RightExpPlus   {}) -> Sum t
        (AS.RightExpMinus  {}) -> Subtract t
        (AS.RightExpTimes  {}) -> Multiply t
        (AS.RightExpDivide {}) -> Divide t
        (AS.RightExpMod    {}) -> Remainder t
        (AS.RightExpPower  {}) -> Power t

to_primitive_type :: T.Type -> PrimType
to_primitive_type T.BooleanType = TypeBool
to_primitive_type T.IntegerType = TypeInt
to_primitive_type T.RealType    = TypeReal
to_primitive_type T.CharType    = TypeChar
to_primitive_type _             = TypeAddr

-- reverse list of blocks and reverse each block list of instructions
reverse_TAC :: State -> State
reverse_TAC (State t str tmp_i bck_i str_i) = (State (reverse $ map (\ (Block n c) -> (Block n (reverse c))) t) (reverse str) tmp_i bck_i str_i)

-- check if block is already present in TAC
lookup :: State -> String -> Maybe Block
lookup (State [] _ _ _ _) _ = Nothing
lookup (State (x:xs) str tmp_i bck_i str_i) b_name = 
    let (Block cur_name _) = x in
        if cur_name == b_name
        then Just x
        else TAC.lookup (State xs str tmp_i bck_i str_i) b_name

-- add instruction to tac inside given block, create new temp block if block was not present
out :: State -> String -> Instruction -> State
out s b_name instr = 
    case (add_instruction_to_block s b_name instr) of
        (True,  b) -> add_temp_block s [instr] -- by default we create a tempblock
        (False, b) -> update_block s b b_name

-- add instruction to block, create new block if block was not present
add_instruction_to_block :: State -> String -> Instruction -> (Bool, Block)
add_instruction_to_block s b_name instr = case (TAC.lookup s b_name) of
    Nothing          -> (True,  (Block b_name [ instr ])) -- create new block ==> is new block? true
    Just (Block n c) -> (False, (Block n (instr:c)))      -- add goto block   ==> is new block? false

add_new_temp_var :: State -> (String, State)
add_new_temp_var state = 
    let name      = make_temp_var_label (temp_idx state)
        temp_idx' = (temp_idx state) + 1
        s'         = state { temp_idx = temp_idx' }
    in (name, s')

-- add temp block to state
add_temp_block :: State -> [Instruction] -> State
add_temp_block state b_code = add_block state (TempBlockType (block_idx state)) b_code

-- add a new block to tac (increase block counter)
add_block :: State -> BlockType -> [Instruction] -> State
add_block state b_type b_code =
    let tac'   = (make_new_block b_type b_code) : (tac state)
        bck_i' = (block_idx state) + 1
    in state { tac = tac', block_idx = bck_i' }

make_new_block :: BlockType -> [Instruction] -> Block
make_new_block b_type b_code = (Block (make_block_label b_type) b_code)

get_name_of_last_temp_block :: State -> String
get_name_of_last_temp_block state = get_name_of_last_ith_temp_block state 0

get_name_of_last_ith_temp_block :: State -> Int -> String
get_name_of_last_ith_temp_block state i = make_block_label (TempBlockType ((block_idx state)-i-1))

-- update a block inside tac, inefficient: O(|blocks|)
update_block :: State -> Block -> String -> State
update_block (State t str tmp_i bck_i str_i) b b_name = (State (update_block_aux t b b_name) str tmp_i bck_i str_i) where
    update_block_aux []     _ b_name = [ (Block ("Internal error: block " ++ b_name ++ " not found") []) ]
    update_block_aux (x:xs) b b_name = 
        let (Block cur_name _) = x in
            if cur_name == b_name
            then b : xs                                 -- block found    ==> add new block and rest of tac
            else x : (update_block_aux xs b b_name) --block not found ==> keep searching

add_string :: State -> String -> State
add_string state str = 
    let strings' = (str:(strings state))
        str_idx' = ((str_idx state)+1)
    in state { strings = strings', str_idx = str_idx' }

make_temp_var_label :: Int -> String
make_temp_var_label i = "tmp?" ++ (show i)

make_block_label :: BlockType -> String
make_block_label StartBlockType                      = "start->program"
make_block_label (MainBlockType pos is_start)        = (make_start_end_label is_start)           ++ "->" ++ "main" ++ "?" ++ (print_row_col pos)
make_block_label (FuncBlockType pos is_start f_name) = (make_start_end_label is_start) ++ "_fun" ++ "->" ++ f_name ++ "?" ++ (print_row_col pos)
make_block_label (ProcBlockType pos is_start p_name) = (make_start_end_label is_start) ++ "_prc" ++ "->" ++ p_name ++ "?" ++ (print_row_col pos)
make_block_label (TempBlockType   idx)               = "block?"  ++ (show idx)
make_block_label (StringBlockType idx)               = "string?" ++ (show idx) -- actually not used to create a block but only by the pretty printer

print_row_col ::(Int, Int) -> String
print_row_col (r,c) = (show r) ++ "_" ++ (show c)

make_start_end_label :: Bool -> String
make_start_end_label is_start = if (is_start) then "start" else "end"

---------------------------------------------------------------------------------------------------------------------------

-- ____________________________ TAC GENERATOR ________________________________________

generate_tac :: AS.Program -> State
generate_tac (AS.ProgramStart _ code pos _ _) = 
    let (main_name, state) = initialize_state pos 
    in gen_tac_of_Block state main_name code

--{ block_declarations :: [Declaration], statements :: [Statement], block_pos :: (Int, Int), block_env :: E.Env, block_errors :: [String] }
gen_tac_of_Block state cur_block_name (AS.Block decls stmts pos _ _) = state --todo

--________________________________ Statement __________________________________________

-- Statement wrapper
gen_tac_of_Statement :: State -> String -> AS.Statement -> State
gen_tac_of_Statement state cur_block_name stmt = gen_tac_fun state cur_block_name stmt
    where gen_tac_fun = case stmt of
            (AS.StatementBlock {})          -> gen_tac_of_StatementBlock
            (AS.StatementIf {})             -> gen_tac_of_StatementIf
            (AS.StatementFor {})            -> gen_tac_of_StatementFor
            (AS.StatementWhile {})          -> gen_tac_of_StatementWhile
            (AS.StatementRepeatUntil {})    -> gen_tac_of_StatementRepeatUntil
            (AS.StatementFuncProcCall {})   -> gen_tac_of_StatementFuncProcCall
            (AS.StatementWrite {})          -> gen_tac_of_StatementWrite
            (AS.StatementBreak {})          -> gen_tac_of_StatementBreak
            (AS.StatementContinue {})       -> gen_tac_of_StatementContinue
            (AS.StatementRead {})           -> gen_tac_of_StatementRead

gen_tac_of_StatementBlock :: State -> String -> AS.Statement -> State
gen_tac_of_StatementBlock state cur_block_name stmt = state

-- StatementIf { condition :: RightExp, then_body :: Statement, else_body_maybe :: Maybe ElseBlock, statement_pos :: (Int, Int), statement_ :: , statement_errors :: [String] }
gen_tac_of_StatementIf :: State -> String -> AS.Statement -> State
gen_tac_of_StatementIf state  cur_block_name stmt =
    case (AS.else_body_maybe stmt) of
        -- if-then
        Nothing ->  let (s1, cond_addr) = gen_tac_of_RightExp  state cur_block_name (AS.condition stmt)
                        s2              = gen_tac_of_Statement s1    cur_block_name (AS.then_body stmt)
                    in  s2
        -- if-then-else
        Just else_body -> state

gen_tac_of_StatementFor :: State -> String -> AS.Statement -> State
gen_tac_of_StatementFor state cur_block_name stmt = state

gen_tac_of_StatementWhile :: State -> String -> AS.Statement -> State
gen_tac_of_StatementWhile state cur_block_name stmt = state

gen_tac_of_StatementRepeatUntil :: State -> String -> AS.Statement -> State
gen_tac_of_StatementRepeatUntil state cur_block_name stmt = state

gen_tac_of_StatementAssign :: State -> String -> AS.Statement -> State
gen_tac_of_StatementAssign state cur_block_name stmt = state

gen_tac_of_StatementFuncProcCall :: State -> String -> AS.Statement -> State
gen_tac_of_StatementFuncProcCall state cur_block_name stmt = state

gen_tac_of_StatementWrite :: State -> String -> AS.Statement -> State
gen_tac_of_StatementWrite state cur_block_name stmt = state

gen_tac_of_StatementBreak :: State -> String -> AS.Statement -> State
gen_tac_of_StatementBreak state cur_block_name stmt = state

gen_tac_of_StatementContinue :: State -> String -> AS.Statement -> State
gen_tac_of_StatementContinue state cur_block_name stmt = state

gen_tac_of_StatementRead :: State -> String -> AS.Statement -> State
gen_tac_of_StatementRead state cur_block_name stmt = state

--________________________________ Right Expression __________________________________________

-- Right Expression wrapper
gen_tac_of_RightExp :: State -> String -> AS.RightExp -> (State, Maybe Address)
gen_tac_of_RightExp state cur_block_name r_exp =
    case r_exp of
        -- base case : literals
        (AS.RightExpInteger {}) -> (state, Just $ AddressInt  $ AS.right_exp_int    r_exp)
        (AS.RightExpReal {})    -> (state, Just $ AddressReal $ AS.right_exp_double r_exp)
        (AS.RightExpBoolean {}) -> (state, Just $ AddressBool $ AS.right_exp_bool   r_exp)
        (AS.RightExpChar {})    -> (state, Just $ AddressChar $ AS.right_exp_char   r_exp)
        (AS.RightExpString {} ) -> ((add_string state (AS.right_exp_string r_exp)), Just $ AddressTempVar $ make_block_label $ StringBlockType (str_idx state))
        -- boolean operators
        (AS.RightExpOr {})      -> 
            let s1 = add_temp_block (add_temp_block state []) [] -- add temp blocks: block-if-true, block-if-false
            in  gen_tac_of_RightExpOr s1 cur_block_name (AS.sx r_exp) (AS.dx r_exp) 
        -- binary math operators   
        -- RightExpPlus { sx, dx :: RightExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
        (AS.RightExpLess {})    -> gen_tac_of_binary_math_operators state cur_block_name r_exp

gen_tac_of_binary_math_operators :: State -> String -> AS.RightExp -> (State, Maybe Address)
gen_tac_of_binary_math_operators state cur_block_name op = 
    let (s1, Just r1) = gen_tac_of_RightExp state cur_block_name (AS.sx op)
        (s2, Just r2) = gen_tac_of_RightExp s1 cur_block_name (AS.dx op)
        (tmp_name, s3) = add_new_temp_var s2
        tmp_var = (AddressTempVar tmp_name)
        ass_type = to_primitive_type (AS.right_exp_type op)
        bin_op = to_primitive_math_binary_operator op
        instr = (BinaryAssignment tmp_var r1 r2 ass_type bin_op)
        s4 = out s3 cur_block_name instr
    in (s4, Nothing)

gen_tac_of_RightExpOr :: State -> String -> AS.RightExp -> AS.RightExp -> (State, Maybe Address)
gen_tac_of_RightExpOr state cur_block_name sx dx =
    let block_if_true       = get_name_of_last_ith_temp_block state 1
        block_if_false      = get_name_of_last_ith_temp_block state 0        
        (s1, Just addr_sx)  = gen_tac_of_RightExp state cur_block_name sx
        left_instr          = JumpIfTrue { goto = block_if_true, cond = addr_sx }
        s2                  = out s1 cur_block_name left_instr
        (s3, Just addr_dx)  = gen_tac_of_RightExp s2 cur_block_name dx
        rght_instr          = JumpIfFalse { goto = block_if_false, cond = addr_dx }
        s4                  = out s3 cur_block_name rght_instr
    in (s4, Nothing)



----------------------------------------------------------------------------------------------------------------------------

pretty_printer_tac :: State -> String
pretty_printer_tac (State t str _ _ _) = "# TAC START\n" ++ "# FUNCTIONS AND PROCEDURE\n" ++ (pretty_printer_tac_aux t str) ++ "#TAC END\n" where
    pretty_printer_tac_aux []     str = "# STRINGS\n" ++ pretty_printer_string str
    pretty_printer_tac_aux (x:xs) str = (pretty_printer_block x) ++ (pretty_printer_tac_aux xs str)

pretty_printer_block :: Block -> String
pretty_printer_block (Block b_name b_code) = b_name ++ ":\n" ++ (pretty_printer_block_aux b_code) where
    pretty_printer_block_aux []     = "\n"
    pretty_printer_block_aux (x:xs) = "   " ++ (show x) ++ "\n" ++ (pretty_printer_block_aux xs)
    
pretty_printer_string :: [ String ] -> String
pretty_printer_string str = (pretty_printer_string_aux str 0) where
    pretty_printer_string_aux []     _ = "\n"
    pretty_printer_string_aux (x:xs) i = (make_block_label $ StringBlockType i) ++ ":\n   " ++ (show x) ++ "\n" ++ (pretty_printer_string_aux xs (i+1))

----------------------------------------------------------------------------------------------------------------------------

get_tabs 0 = ""
get_tabs n = ' ' : (get_tabs (n-1))

pretty_print_any_text text = pretty_print_any_text_aux text 1
    where
        pretty_print_any_text_aux ""       n = ""
        pretty_print_any_text_aux (c:xs) n | (c == '{') = (c : '\n' : (get_tabs (n+1)))    ++ (pretty_print_any_text_aux xs (n+1))
        pretty_print_any_text_aux (c:xs) n | (c == '}') = ('\n' : (get_tabs (n-1)) ++ [c]) ++ (pretty_print_any_text_aux xs (n-1))
        --pretty_print_any_text_aux (',':xs) n = (",\n" ++ (get_tabs n))           ++ (pretty_print_any_text_aux xs n)
        pretty_print_any_text_aux (x:xs)   n = (x : (pretty_print_any_text_aux xs n))


main :: IO ()
main = do

    let or_stmt = AS.RightExpOr { 
        AS.sx = AS.RightExpBoolean {
            AS.right_exp_bool = True,
            AS.right_exp_pos  = (0, 0),
            AS.right_exp_type = T.BooleanType,
            AS.right_exp_env  = E.emptyEnv,
            AS.right_exp_errors = []
        },
        AS.dx = AS.RightExpBoolean {
            AS.right_exp_bool = True,
            AS.right_exp_pos  = (1, 1),
            AS.right_exp_type = T.BooleanType,
            AS.right_exp_env  = E.emptyEnv,
            AS.right_exp_errors = []
        },
        AS.right_exp_pos = (2,2), 
        AS.right_exp_type = T.BooleanType, 
        AS.right_exp_env = E.emptyEnv, 
        AS.right_exp_errors = [] 
    }

    let (name, s0) = initialize_state (0,0)
    let (s, a) = gen_tac_of_RightExp s0 name or_stmt
    putStr "\n\n---------------\n"
    putStrLn "Program Abstract syntax"
    putStr $ pretty_print_any_text $ show or_stmt
    putStr "\n\n---------------\n"
    putStrLn "TAC data structure"
    putStr $ pretty_print_any_text $ show s
    putStr "\n\n---------------\n"
    putStrLn "TAC output"
    putStr $ pretty_printer_tac $ reverse_TAC s
    {-
    let if_stmt = AS.StatementIf {
        AS.condition = AS.RightExpBoolean {
            AS.right_exp_bool = True,
            AS.right_exp_pos  = (0, 0),
            AS.right_exp_type = T.BooleanType,
            AS.right_exp_env  = E.emptyEnv,
            AS.right_exp_errors = []
        },
        AS.then_body = AS.StatementWrite {
            --{write_primitive  :: WritePrimitive, statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
            AS.write_primitive = AS.WriteInt {
                --AS.write_exp :: RightExp, write_primitive_pos :: (Int, Int), write_primitive_env :: E.Env, write_primitive_errors :: [String]
                AS.write_exp = AS.RightExpInteger {
                    -- { right_exp_int :: Int, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
                    AS.right_exp_int = 2,
                    AS.right_exp_pos = (1,1),
                    AS.right_exp_type = T.IntegerType,
                    AS.right_exp_env = E.emptyEnv,
                    AS.right_exp_errors = []
                },
                AS.write_primitive_pos = (0,0),
                AS.write_primitive_env = E.emptyEnv,
                AS.write_primitive_errors = []
            },
            AS.statement_pos = (2,2),
            AS.statement_env = E.emptyEnv,
            AS.statement_errors = []                  
        },
        AS.else_body_maybe = Nothing,
        AS.statement_pos = (3,3),
        AS.statement_env = E.emptyEnv,
        AS.statement_errors = []
    }
    
    putStrLn ""
    
    putStrLn $ show if_stmt
    --putStrLn $ pretty_print_any_text $ show if_stmt
    
    putStrLn ""

    let t = gen_tac_of_StatementIf (initialize_state) "" if_stmt
    putStrLn $ show t
    
    putStrLn ""
    -}
    {-
    let b0_name = "ciao"
    let b0 = (Block b0_name [])
    let t0 = (State [b0] 0 0)
    --putStrLn $ show $ TAC.lookup t0 b0_name
    --putStrLn $ show $ TAC.lookup t0 "rand"

    let b1_name = "bella"
    let b1 = (Block b1_name [])
    let t1 = add_block b1 t0
    putStr $ pretty_printer_tac t1

    let t2 = out t1 b0_name (Jump "cp")
    let t3 = out t2 b0_name (Jump "rm")
    let t4 = out t3 b1_name (Jump "x")
    let t5 = out t4 b1_name (Jump "y")
    putStr $ pretty_printer_tac t5

    let tn = reverse_TAC t5

    putStr $ pretty_printer_tac tn
    -}