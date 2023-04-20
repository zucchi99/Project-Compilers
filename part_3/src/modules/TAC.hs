
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
    -- comment (for the sake of debugging)
    | Comment               { comment :: String }
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
        s10         = add_block s main_type []
        s20         = out s10 start_name (Jump main_name)
    in  (main_name, s20)
   
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

add_many_empty_temp_block :: State -> Int -> State
add_many_empty_temp_block state 0 = state
add_many_empty_temp_block state n = add_many_empty_temp_block (add_temp_block state []) (n-1)

-- add a temp empty block in the middle of the tac, after b_before
-- NB less efficient

add_many_blocks_after_given_block :: State -> String -> Int -> State
add_many_blocks_after_given_block (State tac str tmp_i bck_i str_i) b_before n =
    let bck_i'  = bck_i + n
        tac'    = insert_in_list tac b_before bck_i n     
    in  (State tac' str tmp_i bck_i' str_i)

insert_in_list :: [Block] -> String -> Int -> Int -> [Block]
insert_in_list tac b_before bck_i n = insert_in_list_aux tac b_before (create_many_empty_temp_blocks n bck_i) where
    insert_in_list_aux []     b_before b_new = b_new
    insert_in_list_aux (x:xs) b_before b_new | (block_name x) == b_before = b_new ++ [ x ] ++ xs
                                             | otherwise                  = x : (insert_in_list_aux xs b_before b_new)

create_many_empty_temp_blocks :: Int -> Int -> [Block]
create_many_empty_temp_blocks n bck_i = 
    create_many_empty_temp_blocks_aux n (bck_i+n-1) where
        create_many_empty_temp_blocks_aux 0 bck_i = []
        create_many_empty_temp_blocks_aux n bck_i = (make_new_block (TempBlockType bck_i) []) : (create_many_empty_temp_blocks_aux (n-1) (bck_i-1))
            
-- add a new block to tac at top of the list of blocks (increase block counter)
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
    
-- ____________________________ LABELS ________________________________________

make_ident_var_label :: String -> (Int, Int) -> String
make_ident_var_label name pos = name ++ "?" ++ (print_row_col pos)

make_temp_var_label :: Int -> String
make_temp_var_label i = "tmp?" ++ (show i)

make_block_label :: BlockType -> String
make_block_label StartBlockType                      = "start->program"
make_block_label (MainBlockType pos is_start)        = (make_start_end_label is_start)           ++ "->" ++ "main" ++ "?" ++ (print_row_col pos)
make_block_label (FuncBlockType pos is_start f_name) = (make_start_end_label is_start) ++ "_fun" ++ "->" ++ f_name ++ "?" ++ (print_row_col pos)
make_block_label (ProcBlockType pos is_start p_name) = (make_start_end_label is_start) ++ "_prc" ++ "->" ++ p_name ++ "?" ++ (print_row_col pos)
make_block_label (TempBlockType   idx)               = "block?"  ++ (show idx)
make_block_label (StringBlockType idx)               = "string?" ++ (show idx) -- actually not used to create a block but only by the pretty printer

print_row_col :: (Int, Int) -> String
print_row_col (r,c) = (show r) ++ "_" ++ (show c)

make_start_end_label :: Bool -> String
make_start_end_label is_start = if (is_start) then "start" else "end"

---------------------------------------------------------------------------------------------------------------------------

-- ____________________________ TAC GENERATOR ________________________________________

generate_tac :: AS.Program -> State
generate_tac (AS.ProgramStart _ code pos _ _) = 
    let (main_name, state) = initialize_state pos
    in reverse_TAC $ fst $ gen_tac_of_Block state main_name code

--{ block_declarations :: [Declaration], statements :: [Statement], block_pos :: (Int, Int), block_env :: E.Env, block_errors :: [String] }
gen_tac_of_Block :: State -> String -> AS.Block -> (State, String)
gen_tac_of_Block state cur_blck (AS.Block []        []        pos   _   _  ) = 
    let s10 = out state cur_blck (Comment $ "end of block?" ++ (print_row_col pos))
    in  (s10, cur_blck)
gen_tac_of_Block state cur_blck (AS.Block (x:decls) stmts     pos env err) = 
    gen_tac_of_Block state cur_blck (AS.Block [] stmts pos env err) --todo
gen_tac_of_Block state cur_blck (AS.Block []        (x:stmts) pos env err) = 
    let (s10, cur_blck1) = gen_tac_of_Statement state cur_blck x
    in  gen_tac_of_Block s10 cur_blck1 (AS.Block [] stmts pos env err) 

--________________________________ Statement __________________________________________

-- Statement wrapper
gen_tac_of_Statement :: State -> String -> AS.Statement -> (State, String)
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

gen_tac_of_StatementBlock :: State -> String -> AS.Statement -> (State, String)
gen_tac_of_StatementBlock state cur_blck blck_stmt = gen_tac_of_Block state cur_blck (AS.block blck_stmt)

-- StatementIf { condition :: RightExp, then_body :: Statement, else_body_maybe :: Maybe ElseBlock, statement_pos :: (Int, Int), statement_ :: , statement_errors :: [String] }
gen_tac_of_StatementIf state cur_blck if_stmt =
    let show_stmt        = " if statement declared at " ++ (print_row_col $ AS.statement_pos if_stmt)
        s00             = out state cur_blck (Comment $ "start of" ++ show_stmt)
        maybe_else_body = (AS.else_body_maybe if_stmt)
        -- create temp blocks: block-then, block-else (also if there is no else block, easier to code), block-next
        s10             = add_many_blocks_after_given_block s00 cur_blck 3
        block_then      = get_name_of_last_ith_temp_block s10 2
        block_else      = get_name_of_last_ith_temp_block s10 1
        block_next      = get_name_of_last_ith_temp_block s10 0
        s14             = out s10 block_then (Comment $ "start of THEN " ++ block_then ++ " added by" ++ show_stmt)
        s15             = out s14 block_else (Comment $ "start of ELSE " ++ block_else ++ " added by" ++ show_stmt)
        s16             = out s15 block_next (Comment $ "start of NEXT " ++ block_next ++ " added by" ++ show_stmt)
        s17             = out s16 block_else (Comment $ "is ELSE block empty? " ++ (show $ isNothing maybe_else_body))
        -- generate code for condition 
        (s20, maybe_addr) = gen_tac_of_RightExp s17 cur_blck block_then block_else (AS.condition if_stmt)
        -- if cond_addr is given, add instruction : if_false cond_addr then goto block_else
        -- else : no instruction needed (and / or is present) ==> already handled goto
        s30              = out_jumpIf_if_address_is_given s20 cur_blck block_else maybe_addr JumpIfFalse
        -- add then body
        (s40, cur_blck_then) = gen_tac_of_Statement s30 block_then (AS.then_body if_stmt)
        -- add goto next (skip else body)
        s50              = out s40 cur_blck_then (Jump { goto = block_next })
        -- add else statement and body
        (s60, cur_blck_else) = gen_tac_of_ElseBlock s50 block_else maybe_else_body
        -- add goto next (necessary only if elseBlock has added blocks betweem else and next blocks )
        s70              = out s60 cur_blck_else (Jump { goto = block_next })
    in (s70, block_next)

-- data ElseBlock = ElseBlock { else_body :: Statement, else_block_pos :: (Int, Int), else_block_env :: E.Env, else_block_errors :: [String] }}
gen_tac_of_ElseBlock state cur_blck Nothing          = (state, cur_blck)
gen_tac_of_ElseBlock state cur_blck (Just else_blck) = gen_tac_of_Statement state cur_blck (AS.else_body else_blck)

gen_tac_of_StatementFor state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementWhile state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementRepeatUntil state cur_blck stmt = (state, cur_blck)

-- StatementAssign { assign :: Assign, statement_pos :: (Int, Int), statement_env :: E.Env, statement_errors :: [String] }
gen_tac_of_StatementAssign state cur_blck stmt = gen_tac_of_VariableAssignment state cur_blck (AS.assign stmt)

-- data Assign = VariableAssignment { left_exp_assignment :: LeftExp, right_exp_assignment :: RightExp, assign_pos :: (Int, Int), assign_env :: E.Env, assign_errors :: [String] }
gen_tac_of_VariableAssignment state cur_blck assgn_stmt = 
    let s00         = out state cur_blck (Comment $ "start of variable assignment declared at " ++ (print_row_col $ AS.assign_pos assgn_stmt))
        r_exp       = (AS.right_exp_assignment assgn_stmt)
        has_and_or  = r_exp_has_boolean_operators r_exp
    in gen_tac_of_VariableAssignment_aux s00 cur_blck assgn_stmt has_and_or

gen_tac_of_VariableAssignment_aux state cur_blck assgn_stmt True = 
    let show_stmt       = " assignment declared at " ++ (print_row_col $ AS.assign_pos assgn_stmt)
        s00             = out state cur_blck (Comment $ "start of" ++ show_stmt)
        -- BEFORE RIGHT EXPRESSION
        r_exp         = (AS.right_exp_assignment assgn_stmt)
        -- generate 2 temp block only if r_exp is boolean and contains and/or
        -- NB first create else blck after then block (remember the reverse tac ad the end of generation)
        s10             = add_many_blocks_after_given_block state cur_blck 3
        blck_true       = get_name_of_last_ith_temp_block s10 2
        blck_false      = get_name_of_last_ith_temp_block s10 1
        block_next      = get_name_of_last_ith_temp_block s10 0
        s14             = out s10 blck_true  (Comment $ "start of THEN " ++ blck_true  ++ " added by" ++ show_stmt)
        s15             = out s14 blck_false (Comment $ "start of ELSE " ++ blck_false ++ " added by" ++ show_stmt)
        s16             = out s15 block_next (Comment $ "start of NEXT " ++ block_next ++ " added by" ++ show_stmt)
        -- if has_and_or=True ==> gen_tac_of_RightExp will return (state, Nothing)
        (s20, Nothing) = gen_tac_of_RightExp s16 cur_blck blck_true blck_false r_exp
        -- AFTER LEFT EXPRESSION
        l_exp         = (AS.left_exp_assignment assgn_stmt)
        (s30, t, l_addr)    = gen_tac_of_LeftExp s20 cur_blck l_exp
        -- add assignment code
        s40            = out s30 blck_true  (NullAssignment { l = l_addr, r = (AddressBool True),  assign_type = t })
        s45            = out s40 blck_true (Jump { goto = block_next })
        s50            = out s45 blck_false (NullAssignment { l = l_addr, r = (AddressBool False), assign_type = t })
    in (s50, block_next)
gen_tac_of_VariableAssignment_aux state cur_blck assgn_stmt False = 
    let -- BEFORE RIGHT EXPRESSION
        r_exp           = (AS.right_exp_assignment assgn_stmt)
        (s10, Just r_addr) = gen_tac_of_RightExp state cur_blck "dummy" "dummy" r_exp
        -- AFTER LEFT EXPRESSION
        l_exp           = (AS.left_exp_assignment assgn_stmt)
        (s20, t, l_addr)      = gen_tac_of_LeftExp s10 cur_blck l_exp
        -- add assignment code
        s30              = out s20 cur_blck (NullAssignment { l = l_addr, r = r_addr, assign_type = t })
    in (s30, cur_blck)


gen_tac_of_StatementFuncProcCall state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementWrite state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementBreak state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementContinue state cur_blck stmt = (state, cur_blck)

gen_tac_of_StatementRead state cur_blck stmt = (state, cur_blck)

--________________________________ Left Expression __________________________________________

gen_tac_of_LeftExp state cur_blck l_exp = 
    let prim_type = to_primitive_type (AS.left_exp_type l_exp)
    in case l_exp of 
        (AS.LeftExpIdent {})           -> (state, prim_type, gen_tac_of_Ident (AS.left_exp_name l_exp))
        --array_name :: LeftExp, array_pos :: [RightExp], left_exp_type :: T.Type, left_exp_pos  :: (Int, Int), left_exp_env :: E.Env, left_exp_errors :: [String] 
        (AS.LeftExpArrayAccess {})     -> (state, prim_type, AddressTempVar "todo")
        (AS.LeftExpPointerValue {})    -> (state, prim_type, AddressTempVar "todo")
        (AS.LeftExpPointerAddress {})  -> (state, prim_type, AddressTempVar "todo")

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

--________________________________ Right Expression __________________________________________

-- Right Expression wrapper
gen_tac_of_RightExp state cur_blck blck_true blck_false r_exp =
    case r_exp of
        -- base case : literals
        (AS.RightExpInteger {})         -> (state, Just $ AddressInt  $ AS.right_exp_int    r_exp)
        (AS.RightExpReal {})            -> (state, Just $ AddressReal $ AS.right_exp_double r_exp)
        (AS.RightExpBoolean {})         -> (state, Just $ AddressBool $ AS.right_exp_bool   r_exp)
        (AS.RightExpChar {})            -> (state, Just $ AddressChar $ AS.right_exp_char   r_exp)
        (AS.RightExpString {} )         -> ((add_string state (AS.right_exp_string r_exp)), Just $ AddressTempVar $ make_block_label $ StringBlockType (str_idx state))
        -- unary boolean operators
        -- NOT todo
        -- binary boolean operators
        (AS.RightExpOr {})              -> gen_tac_of_RightExpOr state cur_blck blck_true blck_false (AS.sx r_exp) (AS.dx r_exp) 
        -- AND todo
        -- binary arithmetic operators
        (AS.RightExpPlus {})            -> gen_tac_of_binary_arithm_operators     state cur_blck blck_true blck_false r_exp
        (AS.RightExpMinus {})           -> gen_tac_of_binary_arithm_operators     state cur_blck blck_true blck_false r_exp
        (AS.RightExpTimes {})           -> gen_tac_of_binary_arithm_operators     state cur_blck blck_true blck_false r_exp
        (AS.RightExpDivide {})          -> gen_tac_of_binary_arithm_operators     state cur_blck blck_true blck_false r_exp
        (AS.RightExpMod {})             -> gen_tac_of_binary_arithm_operators     state cur_blck blck_true blck_false r_exp
        (AS.RightExpDiv {})             -> gen_tac_of_binary_arithm_operators     state cur_blck blck_true blck_false r_exp
        (AS.RightExpPower {})           -> gen_tac_of_binary_arithm_operators     state cur_blck blck_true blck_false r_exp
        -- binary relational operators
        (AS.RightExpGreater {})         -> gen_tac_of_binary_relational_operators state cur_blck blck_true blck_false r_exp
        (AS.RightExpLess {})            -> gen_tac_of_binary_relational_operators state cur_blck blck_true blck_false r_exp
        (AS.RightExpGreaterEqual {})    -> gen_tac_of_binary_relational_operators state cur_blck blck_true blck_false r_exp
        (AS.RightExpLessEqual {})       -> gen_tac_of_binary_relational_operators state cur_blck blck_true blck_false r_exp
        (AS.RightExpEqual {})           -> gen_tac_of_binary_relational_operators state cur_blck blck_true blck_false r_exp

    
-- both arithmetic and relational binary operators
gen_tac_of_binary_operators :: State -> String -> String -> String -> AS.RightExp -> (AS.RightExp -> t) -> (Address -> Address -> Address -> PrimType -> t -> Instruction) -> (State, Maybe Address)
gen_tac_of_binary_operators state cur_blck blck_true blck_false op to_primitive_op constructor = 
    let (s10, Just r1) = gen_tac_of_RightExp state cur_blck blck_true blck_false (AS.sx op)
        (s20, Just r2) = gen_tac_of_RightExp s10 cur_blck blck_true blck_false (AS.dx op)
        (tmp_name, s30) = add_temp_var s20
        tmp_var = (AddressTempVar tmp_name)
        r_exp_type = (AS.right_exp_type op)
        ass_type = if r_exp_type == T.TBDType then TypeBool else to_primitive_type r_exp_type
        bin_op = to_primitive_op op
        instr = (constructor tmp_var r1 r2 ass_type bin_op)
        s40 = out s30 cur_blck instr
    in (s40, Just tmp_var)

-- binary relational operators: <, <=, >, >=, =, !=
gen_tac_of_binary_relational_operators :: State -> String -> String -> String -> AS.RightExp -> (State, Maybe Address)
gen_tac_of_binary_relational_operators state cur_blck blck_true blck_false op = gen_tac_of_binary_operators state cur_blck blck_true blck_false op to_primitive_relational_operator BinaryRelatAssignment

-- binary math operators: +, -, *, /, //, %, ^
gen_tac_of_binary_arithm_operators :: State -> String -> String -> String -> AS.RightExp -> (State, Maybe Address)
gen_tac_of_binary_arithm_operators state cur_blck blck_true blck_false op = gen_tac_of_binary_operators state cur_blck blck_true blck_false op to_primitive_arithm_binary_operator BinaryArithmAssignment

-- binary logical operator OR
gen_tac_of_RightExpOr :: State -> String -> String -> String -> AS.RightExp -> AS.RightExp -> (State, Maybe Address)
gen_tac_of_RightExpOr state cur_blck blck_true blck_false sx dx =
    let -- generate code for left-exp
        (s20, may_sx_addr) = gen_tac_of_RightExp state cur_blck blck_true blck_false sx
        -- if left is true ==> jump if true to blck_true
        s30                = out_jumpIf_if_address_is_given s20 cur_blck blck_true may_sx_addr JumpIfTrue 
        -- generate code for right-exp
        (s40, may_dx_addr) = gen_tac_of_RightExp s30 cur_blck blck_true blck_false dx
        -- if right is false ==> jump if false to blck_false
        s50                = out_jumpIf_if_address_is_given s40 cur_blck blck_false may_dx_addr JumpIfFalse
    in (s50, Nothing)


--________________________________ Read / Write Primitive ________________________________________
    {-
gen_tac_of_WritePrimitive state cur_blck prim = 
    let (s10, maybe_addr) = gen_tac_of_RightExp state cur_blck blck_true blck_false r_exp
    in
    = WriteInt                              { write_exp :: RightExp, write_primitive_pos :: (Int, Int), write_primitive_env :: E.Env, write_primitive_errors :: [String] }
    | WriteReal                             { write_exp :: RightExp, write_primitive_pos :: (Int, Int), write_primitive_env :: E.Env, write_primitive_errors :: [String] }
    | WriteChar                             { write_exp :: RightExp, write_primitive_pos :: (Int, Int), write_primitive_env :: E.Env, write_primitive_errors :: [String] }
    | WriteString                           { write_exp :: RightExp, write_primitive_pos :: (Int, Int), write_primitive_env :: E.Env, write_primitive_errors :: [String] }
-}
--________________________________ Identifier ________________________________________

gen_tac_of_Ident ident = (AddressProgramVar $ make_ident_var_label (AS.id_name ident) (AS.ident_pos ident))

-----------------------------------------------------------------------------------------------------------------------------

-- ____________________________ AUX FUNCTIONS ________________________________________

r_exp_has_boolean_operators :: AS.RightExp -> Bool
r_exp_has_boolean_operators (AS.RightExpOr {})  = True 
r_exp_has_boolean_operators (AS.RightExpAnd {}) = True
r_exp_has_boolean_operators _                   = False

out_jumpIf_if_address_is_given :: State -> String -> String -> Maybe Address -> (String -> Address -> Instruction) -> State
out_jumpIf_if_address_is_given state cur_blck goto_blck Nothing          constructor = state
out_jumpIf_if_address_is_given state cur_blck goto_blck (Just cond_addr) constructor = out state cur_blck (constructor goto_blck cond_addr)

----------------------------------------------------------------------------------------------------------------------------

pretty_printer_tac :: State -> String
pretty_printer_tac (State t str _ _ _) = "# TAC START\n" ++ "# FUNCTIONS AND PROCEDURE\n" ++ (pretty_printer_tac_aux t str) ++ "#TAC END\n" where
    pretty_printer_tac_aux []     str = "\n# STRINGS\n" ++ pretty_printer_string str
    pretty_printer_tac_aux (x:xs) str = (pretty_printer_block x) ++ (pretty_printer_tac_aux xs str)

pretty_printer_block :: Block -> String
pretty_printer_block (Block cur_blck b_code) = (if (take 6 cur_blck) == "block?" then "" else "\n") ++ cur_blck ++ ":\n" ++ (pretty_printer_block_aux b_code) where
    pretty_printer_block_aux []     = ""
    pretty_printer_block_aux (x:xs) = "   " ++ (show x) ++ "\n" ++ (pretty_printer_block_aux xs)
    
pretty_printer_string :: [ String ] -> String
pretty_printer_string str = (pretty_printer_string_aux str 0) where
    pretty_printer_string_aux []     _ = "\n"
    pretty_printer_string_aux (x:xs) i = (make_block_label $ StringBlockType i) ++ ":\n   " ++ (show x) ++ "\n" ++ (pretty_printer_string_aux xs (i+1))

