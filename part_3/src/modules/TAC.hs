
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
    -- l = r1 bin_op r2 -----> where bin_op in +, -, *, / ecc
    BinaryArithmAssignment  { l :: Address, r1,r2 :: Address, assign_type :: PrimType, bin_arit_op :: BinaryArithmOp }
    -- l = r1 bin_op r2 -----> where bin_op in >, <, <=, >= ecc
    | BinaryRelatAssignment { l :: Address, r1,r2 :: Address, assign_type :: PrimType, bin_rel_op :: BinaryRelatOp }
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
    | JumpConditional       { goto :: String, r1,r2 :: Address, rel_op :: BinaryRelatOp }
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

data BinaryArithmOp = 
    Sum             { binary_type           :: PrimType }
    | Subtract      { binary_type           :: PrimType }
    | Multiply      { binary_type           :: PrimType }
    | Divide        { binary_type           :: PrimType }
    | Remainder     { binary_type           :: PrimType }
    | Power         { binary_type           :: PrimType }
    | AddressSum
    deriving (Show)

-- NB trivially binary_type = T.BooleanType, no attribute needed
data BinaryRelatOp = 
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

instance Eq BlockType where
    StartBlockType       == StartBlockType        = True
    (MainBlockType {})   == (MainBlockType {})    = True
    (FuncBlockType {})   == (FuncBlockType {})    = True
    (ProcBlockType {})   == (ProcBlockType {})    = True
    (TempBlockType {})   == (TempBlockType {})    = True
    (StringBlockType {}) == (StringBlockType {})  = True
    _                    == _                     = False      

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
opposite_relational_operator :: BinaryRelatOp -> BinaryRelatOp
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
   
to_primitive_relational_operator :: AS.RightExp -> BinaryRelatOp
to_primitive_relational_operator (AS.RightExpLess {})           = LessThan
to_primitive_relational_operator (AS.RightExpGreater {})        = GreaterThan
to_primitive_relational_operator (AS.RightExpLessEqual {})      = LessEqual
to_primitive_relational_operator (AS.RightExpGreaterEqual {})   = GreaterEqual
to_primitive_relational_operator (AS.RightExpEqual {})          = Equal
-- to_primitive_relational_operator (AS.RightExpNotEqual {}) = NotEqual

to_primitive_arithm_binary_operator :: AS.RightExp -> BinaryArithmOp
to_primitive_arithm_binary_operator r_exp = 
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
lookup (State (x:xs) str tmp_i bck_i str_i) cur_blck = 
    let (Block cur_name _) = x in
        if cur_name == cur_blck
        then Just x
        else TAC.lookup (State xs str tmp_i bck_i str_i) cur_blck

-- add instruction to tac inside given block, create new temp block if block was not present
out :: State -> String -> Instruction -> State
out s cur_blck instr = 
    case (add_instruction_to_block s cur_blck instr) of
        (True,  b) -> add_temp_block s [instr] -- by default we create a tempblock
        (False, b) -> update_block s b cur_blck

-- add instruction to block, create new block if block was not present
add_instruction_to_block :: State -> String -> Instruction -> (Bool, Block)
add_instruction_to_block s cur_blck instr = case (TAC.lookup s cur_blck) of
    Nothing          -> (True,  (Block cur_blck [ instr ])) -- create new block ==> is new block? true
    Just (Block n c) -> (False, (Block n (instr:c)))      -- add goto block   ==> is new block? false

add_temp_var :: State -> (String, State)
add_temp_var state = 
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
        bck_i  = (block_idx state)
        bck_i' = if b_type == (TempBlockType 0) then bck_i + 1 else bck_i
    in state { tac = tac', block_idx = bck_i' }

make_new_block :: BlockType -> [Instruction] -> Block
make_new_block b_type b_code = (Block (make_block_label b_type) b_code)

get_name_of_last_temp_block :: State -> String
get_name_of_last_temp_block state = get_name_of_last_ith_temp_block state 0

get_name_of_last_ith_temp_block :: State -> Int -> String
get_name_of_last_ith_temp_block state i = make_block_label (TempBlockType ((block_idx state)-i-1))

-- update a block inside tac, inefficient: O(|blocks|)
update_block :: State -> Block -> String -> State
update_block (State t str tmp_i bck_i str_i) b cur_blck = (State (update_block_aux t b cur_blck) str tmp_i bck_i str_i) where
    update_block_aux []     _ cur_blck = [ (Block ("Internal error: block " ++ cur_blck ++ " not found") []) ]
    update_block_aux (x:xs) b cur_blck = 
        let (Block cur_name _) = x in
            if cur_name == cur_blck
            then b : xs                                 -- block found    ==> add new block and rest of tac
            else x : (update_block_aux xs b cur_blck) --block not found ==> keep searching

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
    in reverse_TAC $ gen_tac_of_Block state main_name code

--{ block_declarations :: [Declaration], statements :: [Statement], block_pos :: (Int, Int), block_env :: E.Env, block_errors :: [String] }
gen_tac_of_Block state _ (AS.Block [] [] _ _ _) = state
gen_tac_of_Block state _ (AS.Block (x:decls) stmts pos env err) = state
gen_tac_of_Block state cur_blck (AS.Block [] (x:stmts) pos env err) = 
    let (s1, cur_blck1) = gen_tac_of_Statement state cur_blck x
    in  gen_tac_of_Block s1 cur_blck1 (AS.Block [] stmts pos env err) 

--________________________________ Statement __________________________________________

-- Statement wrapper
gen_tac_of_Statement state cur_blck stmt = gen_tac_fun state cur_blck stmt
    where gen_tac_fun = case stmt of
            (AS.StatementBlock {})          -> gen_tac_of_StatementBlock
            (AS.StatementIf {})             -> gen_tac_of_StatementIf
            (AS.StatementFor {})            -> gen_tac_of_StatementFor
            (AS.StatementWhile {})          -> gen_tac_of_StatementWhile
            (AS.StatementRepeatUntil {})    -> gen_tac_of_StatementRepeatUntil
            (AS.StatementAssign {})         -> gen_tac_of_StatementAssign
            (AS.StatementFuncProcCall {})   -> gen_tac_of_StatementFuncProcCall
            (AS.StatementWrite {})          -> gen_tac_of_StatementWrite
            (AS.StatementBreak {})          -> gen_tac_of_StatementBreak
            (AS.StatementContinue {})       -> gen_tac_of_StatementContinue
            (AS.StatementRead {})           -> gen_tac_of_StatementRead

gen_tac_of_StatementBlock state cur_blck stmt = (state, cur_blck)

-- StatementIf { condition :: RightExp, then_body :: Statement, else_body_maybe :: Maybe ElseBlock, statement_pos :: (Int, Int), statement_ :: , statement_errors :: [String] }
gen_tac_of_StatementIf state cur_blck stmt =
    let maybe_else_body = (AS.else_body_maybe stmt)
        -- create temp blocks: block-else (also if there is no else block, easier to code), block-next
        s1              = add_temp_block (add_temp_block state []) []
        block_else      = get_name_of_last_ith_temp_block s1 1
        block_next      = get_name_of_last_ith_temp_block s1 0
        -- generate code for condition 
        (s2, maybe_addr) = gen_tac_of_RightExp s1 cur_blck block_next (AS.condition stmt)
        -- if cond_addr is given, add instruction : if_false cond_addr then goto block_else
        -- else : no instruction needed (and / or is present) ==> already handled goto
        s3               = case maybe_addr of
                                Nothing      -> s2
                                Just cond_addr -> out s2 cur_blck (JumpIfFalse { goto = block_else, cond = cond_addr })
        -- add if body
        (s4, cur_blck1) = gen_tac_of_Statement s3 cur_blck (AS.then_body stmt)
        -- add goto next (skip else body)
        s5              = out s4 cur_blck1 (Jump { goto = block_next })
        -- add else statement and body
        (s6, _)    = gen_tac_of_ElseBlock s5 block_else maybe_else_body
    in (s6, block_next)

-- data ElseBlock = ElseBlock { else_body :: Statement, else_block_pos :: (Int, Int), else_block_env :: E.Env, else_block_errors :: [String] }}
gen_tac_of_ElseBlock state cur_blck Nothing          = (state, cur_blck)
gen_tac_of_ElseBlock state cur_blck (Just else_blck) = gen_tac_of_Statement state cur_blck (AS.else_body else_blck)

gen_tac_of_StatementFor state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementWhile state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementRepeatUntil state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementAssign state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementFuncProcCall state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementWrite state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementBreak state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementContinue state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementRead state cur_blck stmt = (state, cur_blck)

--________________________________ Right Expression __________________________________________

-- Right Expression wrapper
gen_tac_of_RightExp state cur_blck nxt_blck r_exp =
    case r_exp of
        -- base case : literals
        (AS.RightExpInteger {})         -> (state, Just $ AddressInt  $ AS.right_exp_int    r_exp)
        (AS.RightExpReal {})            -> (state, Just $ AddressReal $ AS.right_exp_double r_exp)
        (AS.RightExpBoolean {})         -> (state, Just $ AddressBool $ AS.right_exp_bool   r_exp)
        (AS.RightExpChar {})            -> (state, Just $ AddressChar $ AS.right_exp_char   r_exp)
        (AS.RightExpString {} )         -> ((add_string state (AS.right_exp_string r_exp)), Just $ AddressTempVar $ make_block_label $ StringBlockType (str_idx state))
        -- boolean operators
        (AS.RightExpOr {})              -> gen_tac_of_RightExpOr state cur_blck nxt_blck (AS.sx r_exp) (AS.dx r_exp) 
        -- binary arithmetic operators
        (AS.RightExpPlus {})            -> gen_tac_of_binary_arithm_operators state cur_blck nxt_blck r_exp
        (AS.RightExpMinus {})           -> gen_tac_of_binary_arithm_operators state cur_blck nxt_blck r_exp
        (AS.RightExpTimes {})           -> gen_tac_of_binary_arithm_operators state cur_blck nxt_blck r_exp
        (AS.RightExpDivide {})          -> gen_tac_of_binary_arithm_operators state cur_blck nxt_blck r_exp
        (AS.RightExpMod {})             -> gen_tac_of_binary_arithm_operators state cur_blck nxt_blck r_exp
        (AS.RightExpDiv {})             -> gen_tac_of_binary_arithm_operators state cur_blck nxt_blck r_exp
        (AS.RightExpPower {})           -> gen_tac_of_binary_arithm_operators state cur_blck nxt_blck r_exp
        -- binary relational operators
        (AS.RightExpGreater {})         -> gen_tac_of_binary_relational_operators state cur_blck nxt_blck r_exp
        (AS.RightExpLess {})            -> gen_tac_of_binary_relational_operators state cur_blck nxt_blck r_exp
        (AS.RightExpGreaterEqual {})    -> gen_tac_of_binary_relational_operators state cur_blck nxt_blck r_exp
        (AS.RightExpLessEqual {})       -> gen_tac_of_binary_relational_operators state cur_blck nxt_blck r_exp
        (AS.RightExpEqual {})           -> gen_tac_of_binary_relational_operators state cur_blck nxt_blck r_exp

    
-- both arithmetic and relational binary operators
gen_tac_of_binary_operators :: State -> String -> String -> AS.RightExp -> (AS.RightExp -> t) -> (Address -> Address -> Address -> PrimType -> t -> Instruction) -> (State, Maybe Address)
gen_tac_of_binary_operators state cur_blck nxt_blck op to_primitive_op constructor = 
    let (s1, Just r1) = gen_tac_of_RightExp state cur_blck nxt_blck (AS.sx op)
        (s2, Just r2) = gen_tac_of_RightExp s1 cur_blck nxt_blck (AS.dx op)
        (tmp_name, s3) = add_temp_var s2
        tmp_var = (AddressTempVar tmp_name)
        r_exp_type = (AS.right_exp_type op)
        ass_type = if r_exp_type == T.TBDType then TypeBool else to_primitive_type r_exp_type
        bin_op = to_primitive_op op
        instr = (constructor tmp_var r1 r2 ass_type bin_op)
        s4 = out s3 cur_blck instr
    in (s4, Just tmp_var)


-- binary relational operators: <, <=, >, >=, =, !=
gen_tac_of_binary_relational_operators :: State -> String -> String -> AS.RightExp -> (State, Maybe Address)
gen_tac_of_binary_relational_operators state cur_blck nxt_blck op = gen_tac_of_binary_operators state cur_blck nxt_blck op to_primitive_relational_operator BinaryRelatAssignment

-- binary math operators: +, -, *, /, //, %, ^
gen_tac_of_binary_arithm_operators :: State -> String -> String -> AS.RightExp -> (State, Maybe Address)
gen_tac_of_binary_arithm_operators state cur_blck nxt_blck op = gen_tac_of_binary_operators state cur_blck nxt_blck op to_primitive_arithm_binary_operator BinaryArithmAssignment

-- binary logical operator OR
gen_tac_of_RightExpOr :: State -> String -> String -> AS.RightExp -> AS.RightExp -> (State, Maybe Address)
gen_tac_of_RightExpOr state cur_blck nxt_blck sx dx =
    let s1                  = add_temp_block state [] -- add temp block to check dx if sx is false
        b_if_sx_is_false    = get_name_of_last_ith_temp_block s1 0
        (s2, sx_out)        = gen_tac_of_RightExp s1 cur_blck nxt_blck sx
        s3                  = case sx_out of
                                Nothing      -> s2
                                Just addr_sx -> out s2 cur_blck (JumpIfTrue { goto = b_if_sx_is_false, cond = addr_sx })
        (s4, Just addr_dx)  = gen_tac_of_RightExp s3 cur_blck nxt_blck dx
        rght_instr          = JumpIfFalse { goto = nxt_blck, cond = addr_dx }
        s5                  = out s4 cur_blck rght_instr
    in (s5, Nothing)
    
----------------------------------------------------------------------------------------------------------------------------

pretty_printer_tac :: State -> String
pretty_printer_tac (State t str _ _ _) = "# TAC START\n" ++ "# FUNCTIONS AND PROCEDURE\n" ++ (pretty_printer_tac_aux t str) ++ "#TAC END\n" where
    pretty_printer_tac_aux []     str = "# STRINGS\n" ++ pretty_printer_string str
    pretty_printer_tac_aux (x:xs) str = (pretty_printer_block x) ++ (pretty_printer_tac_aux xs str)

pretty_printer_block :: Block -> String
pretty_printer_block (Block cur_blck b_code) = (if (take 6 cur_blck) == "block?" then "" else "\n") ++ cur_blck ++ ":\n" ++ (pretty_printer_block_aux b_code) where
    pretty_printer_block_aux []     = ""
    pretty_printer_block_aux (x:xs) = "   " ++ (show x) ++ "\n" ++ (pretty_printer_block_aux xs)
    
pretty_printer_string :: [ String ] -> String
pretty_printer_string str = (pretty_printer_string_aux str 0) where
    pretty_printer_string_aux []     _ = "\n"
    pretty_printer_string_aux (x:xs) i = (make_block_label $ StringBlockType i) ++ ":\n   " ++ (show x) ++ "\n" ++ (pretty_printer_string_aux xs (i+1))

