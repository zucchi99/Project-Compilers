
-- ghci -outputdir src/bin src/modules/TAC.hs src/modules/Types.hs src/modules/ErrorMessage.hs src/modules/AbstractSyntax.hs

module TAC where

import qualified AbstractSyntax as AS
import qualified Types as T
import qualified Env as E
import ErrM

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
    string_constants             :: [ (String, Address) ], -- map : string constant identifier -> AddressTempVar of the string
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
empty_state = (State [] [] [] 0 0 0)

initialize_state :: (Int, Int) -> (String, State)
initialize_state main_pos = 
    let start_name = make_block_label StartBlockType
        main_type  = (MainBlockType main_pos True)
        main_name  = make_block_label main_type
        s          = add_block empty_state StartBlockType []
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
reverse_TAC (State t str const tmp_i bck_i str_i) = (State (reverse $ map (\ (Block n c) -> (Block n (reverse c))) t) (reverse str) const tmp_i bck_i str_i)

-- check if block is already present in TAC
lookup :: State -> String -> Maybe Block
lookup (State [] _ _ _ _ _) _ = Nothing
lookup (State (x:xs) str const tmp_i bck_i str_i) cur_blck = 
    let (Block cur_name _) = x in
        if cur_name == cur_blck
        then Just x
        else TAC.lookup (State xs str const tmp_i bck_i str_i) cur_blck

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

add_temp_var :: State -> (Address, State)
add_temp_var state = 
    let tmp_var   = (AddressTempVar $ make_temp_var_label (temp_idx state))
        temp_idx' = (temp_idx state) + 1
        s'        = state { temp_idx = temp_idx' }
    in (tmp_var, s')

-- add temp block to state
add_temp_block :: State -> [Instruction] -> State
add_temp_block state b_code = add_block state (TempBlockType (block_idx state)) b_code

add_many_empty_temp_block :: State -> Int -> State
add_many_empty_temp_block state 0 = state
add_many_empty_temp_block state n = add_many_empty_temp_block (add_temp_block state []) (n-1)

-- add 1+ temp empty block in the middle of the tac, after b_before
-- NB less efficient
add_many_blocks_after_given_block :: State -> String -> Int -> State
add_many_blocks_after_given_block (State tac str const tmp_i bck_i str_i) b_before n =
    let bck_i'  = bck_i + n
        tac'    = insert_in_list tac b_before bck_i n     
    in  (State tac' str const tmp_i bck_i' str_i)

add_blocks_then_else_next :: State -> String -> (State, String, String, String)
add_blocks_then_else_next state b_before =
    let s00 = add_many_blocks_after_given_block state b_before 3
        block_then      = get_name_of_last_ith_temp_block s00 2
        block_else      = get_name_of_last_ith_temp_block s00 1
        block_next      = get_name_of_last_ith_temp_block s00 0
    in (s00, block_then, block_else, block_next)

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
update_block (State t str const tmp_i bck_i str_i) b cur_blck = (State (update_block_aux t b cur_blck) str const tmp_i bck_i str_i) where
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

    
add_to_string_constants_list :: State -> String -> Address -> State
add_to_string_constants_list state string address = 
    let string_constants' = (string, address):(string_constants state)
        s' = state { string_constants = string_constants' }
    in  s'

get_address_from_string_constant :: State -> String -> Address
get_address_from_string_constant (State _ _ []                _ _ _) ident = (AddressTempVar "internal error identifier not found")
get_address_from_string_constant (State t str ((name, addr):xs) tmp_i bck_i str_i) ident =
    if name == ident 
        then addr
        else get_address_from_string_constant (State t str xs tmp_i bck_i str_i) ident   

    
-- ____________________________ LABELS ________________________________________

{-
block_TBD :: String
block_TBD = "BLOCK_TO_BE_DEFINED"

is_block_TBD :: String -> Bool
is_block_TBD b_name = b_name == block_TBD
-}

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

gen_tac_of_Block :: State -> String -> AS.Block -> (State, String)
gen_tac_of_Block state cur_blck (AS.Block []        []        pos   _   _  ) = (state, cur_blck)
-- DECLARATIONS
gen_tac_of_Block state cur_blck (AS.Block (d:decls) stmts     pos env err) = 
    let (s10, cur_blck1) = gen_tac_of_Declaration state cur_blck d
    in gen_tac_of_Block s10 cur_blck1 (AS.Block decls stmts pos env err)
-- STATEMENTS
gen_tac_of_Block state cur_blck (AS.Block []        (s:stmts) pos env err) = 
    -- after statements
    let (s10, cur_blck1) = gen_tac_of_Statement state cur_blck s
    in  gen_tac_of_Block s10 cur_blck1 (AS.Block [] stmts pos env err) 

--________________________________ Declaration __________________________________________

gen_tac_of_Declaration state cur_blck declaration = 
    case declaration of 
            -- only string to be actually treated
            (AS.DeclarationCostant {})      -> gen_tac_of_DeclarationCostant   state cur_blck declaration
            (AS.DeclarationVariable {})     -> gen_tac_of_DeclarationVariable  state cur_blck declaration
            (AS.DeclarationFunction {})     -> gen_tac_of_DeclarationFunction  state cur_blck declaration
            (AS.DeclarationProcedure {})    -> gen_tac_of_DeclarationProcedure state cur_blck declaration

gen_tac_of_DeclarationCostant :: State -> String -> AS.Declaration -> (State, String)
gen_tac_of_DeclarationCostant state cur_blck decl_const = 
    let value = (AS.costant_value decl_const)
    in  case value of
        (AS.RightExpString{}) -> 
            let (s10, cur_blck1, address) = gen_tac_of_string state cur_blck value
                ident_name                = (AS.id_name (AS.costant_name decl_const))
                s20                       = add_to_string_constants_list s10 ident_name address
            in  (s20, cur_blck1)
        _   -> (state, cur_blck)

gen_tac_of_DeclarationVariable :: State -> String -> AS.Declaration -> (State, String)
gen_tac_of_DeclarationVariable state cur_blck decl_var =
    case (AS.variable_value_maybe decl_var) of
        Nothing         -> (state, cur_blck)
        Just assgn_stmt ->
            let t                         = to_primitive_type (AS.variable_type decl_var)
                l_addr                    = gen_tac_of_Ident (AS.variable_name decl_var)
                (s10, cur_blck10, r_addr) = gen_tac_of_RightExp state cur_blck assgn_stmt
                s20                       = out s10 cur_blck10 (NullAssignment { l = l_addr, r = r_addr, assign_type = t })
            in (s20, cur_blck10)

gen_tac_of_DeclarationFunction :: State -> String -> AS.Declaration -> (State, String)
gen_tac_of_DeclarationFunction  state cur_blck decl_fun = (state, cur_blck)

gen_tac_of_DeclarationProcedure :: State -> String -> AS.Declaration -> (State, String)
gen_tac_of_DeclarationProcedure state cur_blck decl_prc = (state, cur_blck)

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
    let --show_stmt        = " if statement declared at " ++ (print_row_col $ AS.statement_pos if_stmt)
        --s00             = out state cur_blck (Comment $ "start of" ++ show_stmt)
        maybe_else_body = (AS.else_body_maybe if_stmt)
        -- create temp blocks: block-then, block-else (also if there is no else block, easier to code), block-next
        (s10, block_then, block_else, block_next) = add_blocks_then_else_next state cur_blck
        --s14             = out s10 block_then (Comment $ "start of THEN " ++ block_then ++ " added by" ++ show_stmt)
        --s15             = out s14 block_else (Comment $ "start of ELSE " ++ block_else ++ " added by" ++ show_stmt)
        --s16             = out s15 block_next (Comment $ "start of NEXT " ++ block_next ++ " added by" ++ show_stmt)
        --s17             = out s16 block_else (Comment $ "is ELSE block empty? " ++ (show $ isNothing maybe_else_body))
        -- generate code for condition 
        (s20, cur_block10, cond_addr) = gen_tac_of_RightExp s10 cur_blck (AS.condition if_stmt)
        -- add instruction : if_false cond_addr then goto block_else
        s30              = out s20 cur_block10 (JumpIfFalse block_else cond_addr)
        -- if some blocks has been added between cur_block and block_then ==> add jump to block_then
        --s40              = if block_else == cur_block10 then s30 else out s30 cur_block10 (Jump { goto = block_then })
        -- add then body
        (s50, cur_blck_then) = gen_tac_of_Statement s30 block_then (AS.then_body if_stmt)
        -- add goto next (skip else body)
        s60              = out s50 cur_blck_then (Jump { goto = block_next })
        -- add else statement and body<
        (s70, cur_blck_else) = gen_tac_of_ElseBlock s60 block_else maybe_else_body
        -- add goto next (necessary only if elseBlock has added blocks between else and next blocks )
        --s65              = out s60 block_else (Comment $ "block_else: " ++ block_else ++ ", cur_blck_else: " ++ cur_blck_else ++ " equal?" ++ (show $ block_else == cur_blck_else))
        s80              = if block_else == cur_blck_else then s70 else out s70 block_else (Jump { goto = block_next })
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
    let -- s00                      = out state cur_blck (Comment $ "start of variable assignment declared at " ++ (print_row_col $ AS.assign_pos assgn_stmt))
        -- BEFORE LEFT EXPRESSION
        (s10, t, l_addr)            = gen_tac_of_LeftExp  state   cur_blck10 (AS.left_exp_assignment assgn_stmt)
        -- AFTER RIGHT EXPRESSION
        (s20, cur_blck10, r_addr)   = gen_tac_of_RightExp s10 cur_blck   (AS.right_exp_assignment assgn_stmt)
        -- add assignment code
        s30                         = out s20 cur_blck10 (NullAssignment { l = l_addr, r = r_addr, assign_type = t })
    in (s30, cur_blck10)

--TODO
gen_tac_of_StatementFuncProcCall state cur_blck stmt = (state, cur_blck)

--TODO
gen_tac_of_StatementRead state cur_blck stmt = (state, cur_blck)

-- OK
gen_tac_of_StatementWrite state cur_blck wrt_stmt = gen_tac_of_WritePrimitive state cur_blck (AS.write_primitive wrt_stmt)

--TODO
gen_tac_of_StatementBreak state cur_blck stmt = (state, cur_blck)

--TODO
gen_tac_of_StatementContinue state cur_blck stmt = (state, cur_blck)


--________________________________ Left Expression __________________________________________

gen_tac_of_LeftExp :: State -> String -> AS.LeftExp -> (State, PrimType, Address)
gen_tac_of_LeftExp state cur_blck l_exp = 
    let prim_type = to_primitive_type (AS.left_exp_type l_exp)
        --s10       = out state cur_blck (Comment $ show l_exp)
    in case l_exp of 
        -- variable
        (AS.LeftExpIdent {})           -> (state, prim_type, gen_tac_of_Ident (AS.left_exp_name l_exp))
        -- only string-constants (non-string constants has been already substituted by static semantic)
        (AS.LeftExpConst {})           -> (state, prim_type, get_address_from_string_constant state (AS.id_name (AS.left_exp_name l_exp)))
        (AS.LeftExpArrayAccess {})     -> (state, prim_type, AddressTempVar "todo")
        (AS.LeftExpPointerValue {})    -> (state, prim_type, AddressTempVar "todo")
        (AS.LeftExpPointerAddress {})  -> (state, prim_type, AddressTempVar "todo")
        
--________________________________ Identifier ________________________________________

gen_tac_of_Ident :: AS.Ident -> Address
gen_tac_of_Ident ident = (AddressProgramVar $ make_ident_var_label (AS.id_name ident) (AS.ident_pos ident))

--________________________________ Right Expression __________________________________________

-- Right Expression wrapper
--gen_tac_of_RightExp :: State -> String -> String -> String -> AS.RightExp -> (State, String, Address)
gen_tac_of_RightExp state cur_blck r_exp =
    case r_exp of
        -- base case : literals
        (AS.RightExpInteger {})         -> gen_tac_of_literal                     state cur_blck r_exp AddressInt  AS.right_exp_int
        (AS.RightExpReal {})            -> gen_tac_of_literal                     state cur_blck r_exp AddressReal AS.right_exp_double
        (AS.RightExpBoolean {})         -> gen_tac_of_literal                     state cur_blck r_exp AddressBool AS.right_exp_bool
        (AS.RightExpChar {})            -> gen_tac_of_literal                     state cur_blck r_exp AddressChar AS.right_exp_char
        (AS.RightExpString {} )         -> gen_tac_of_string                      state cur_blck r_exp
        -- unary boolean operator
        (AS.RightExpNot {})             -> gen_tac_of_RightExpNot                 state cur_blck r_exp
        -- binary boolean operators
        (AS.RightExpOr {})              -> gen_tac_of_binary_logical_operators    state cur_blck r_exp
        (AS.RightExpAnd {})             -> gen_tac_of_binary_logical_operators    state cur_blck r_exp
        -- unary arithmetic operators TODO
        (AS.RightExpMinusUnary {})      -> gen_tac_of_RightExpMinusUnary          state cur_blck r_exp
        (AS.RightExpPlusUnary {})       -> gen_tac_of_RightExpPlusUnary           state cur_blck r_exp
        -- binary arithmetic operators
        (AS.RightExpPlus {})            -> gen_tac_of_binary_arithm_operators     state cur_blck r_exp
        (AS.RightExpMinus {})           -> gen_tac_of_binary_arithm_operators     state cur_blck r_exp
        (AS.RightExpTimes {})           -> gen_tac_of_binary_arithm_operators     state cur_blck r_exp
        (AS.RightExpDivide {})          -> gen_tac_of_binary_arithm_operators     state cur_blck r_exp
        (AS.RightExpMod {})             -> gen_tac_of_binary_arithm_operators     state cur_blck r_exp
        (AS.RightExpDiv {})             -> gen_tac_of_binary_arithm_operators     state cur_blck r_exp
        (AS.RightExpPower {})           -> gen_tac_of_binary_arithm_operators     state cur_blck r_exp
        -- binary relational operators
        (AS.RightExpGreater {})         -> gen_tac_of_binary_relational_operators state cur_blck r_exp
        (AS.RightExpLess {})            -> gen_tac_of_binary_relational_operators state cur_blck r_exp
        (AS.RightExpGreaterEqual {})    -> gen_tac_of_binary_relational_operators state cur_blck r_exp
        (AS.RightExpLessEqual {})       -> gen_tac_of_binary_relational_operators state cur_blck r_exp
        (AS.RightExpEqual {})           -> gen_tac_of_binary_relational_operators state cur_blck r_exp
        -- function / procedure call TODO
        (AS.RightExpFuncProcCall {})    -> gen_tac_of_RightExpFuncProcCall        state cur_blck r_exp
        -- use a LeftExp as RightExp
        (AS.RightExpLeftExp {})         -> gen_tac_of_RightExpLeftExp             state cur_blck r_exp
        -- coerce a rightexp to make it compatible with expected type TODO
        (AS.RightExpCoercion {})        -> gen_tac_of_RightExpCoercion            state cur_blck r_exp


--TODO
-- { call_name_right_exp :: Ident, call_params_right_exp :: [RightExp], right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
gen_tac_of_RightExpFuncProcCall state cur_blck r_exp = (state, cur_blck, (AddressTempVar "TODO"))

--TODO
-- RightExpLeftExp { left_exp_right_exp :: LeftExp, right_exp_pos :: (Int, Int), right_exp_type :: T.Type, right_exp_env :: E.Env, right_exp_errors :: [String] }
gen_tac_of_RightExpLeftExp state cur_blck r_exp = 
    let (s10, prim_type, address) = gen_tac_of_LeftExp state cur_blck (AS.left_exp_right_exp r_exp)
    in (s10, cur_blck, address)

--TODO
-- RightExpCoercion { right_exp_coercion :: RightExp, right_exp_from_type :: T.Type, right_exp_to_type :: T.Type, right_exp_pos :: (Int, Int), right_exp_env :: E.Env, right_exp_errors :: [String] }
gen_tac_of_RightExpCoercion state cur_blck r_exp = (state, cur_blck, (AddressTempVar "TODO"))

gen_tac_of_RightExpPlusUnary :: State -> String -> AS.RightExp -> (State, String, Address)
gen_tac_of_RightExpPlusUnary state cur_blck r_exp = gen_tac_of_RightExp state cur_blck (AS.dx r_exp)

gen_tac_of_RightExpMinusUnary :: State -> String -> AS.RightExp -> (State, String, Address)
gen_tac_of_RightExpMinusUnary state cur_blck r_exp = 
    let (s10, cur_blck1, address) = gen_tac_of_RightExp state cur_blck (AS.dx r_exp)
        (tmp_var, s20)            = add_temp_var s10
        prim_type                 = to_primitive_type (AS.right_exp_type r_exp)
        un_op_neg                 = (Negate { negation_type = prim_type } )      
        s30                       = out s20 cur_blck1 (UnaryAssignment { l = tmp_var, r = address, assign_type = prim_type, un_op = un_op_neg })
        in (s30, cur_blck1, tmp_var)

gen_tac_of_literal :: State -> String -> t1 -> (t2 -> Address) -> (t1 -> t2) -> (State, String, Address)
gen_tac_of_literal state cur_blck r_exp constructor attribute = (state, cur_blck, constructor $ attribute r_exp)

gen_tac_of_string :: State -> String -> AS.RightExp -> (State, String, Address)
gen_tac_of_string state cur_blck r_exp = 
    let s10 = add_string state (AS.right_exp_string r_exp)
    in (s10, cur_blck, AddressTempVar $ make_block_label $ StringBlockType (str_idx state))

--gen_tac_of_RightExpNot :: State -> String -> AS.RightExp -> (State, String, Address)
gen_tac_of_RightExpNot state cur_blck not_exp = 
    let --s00         = out state cur_blck (Comment $ "start of NOT assignment declared at " ++ (print_row_col $ AS.right_exp_pos not_exp))
        r_exp       = (AS.dx not_exp)
        (s10, cur_blck10, addr) = gen_tac_of_RightExp state cur_blck r_exp
        -- create temp variable to store not result
        -- NB actually it is needed only for the cases: (not true), (not false)
        -- in all other cases the addr returned is already a left address
        (tmp_var, s20)  = add_temp_var s10
        s30              = out s20 cur_blck10 (UnaryAssignment { l = tmp_var, r = addr, assign_type = TypeBool, un_op = Not })
    in  (s30, cur_blck10, tmp_var)

-- binary relational operators: <, <=, >, >=, =, !=
gen_tac_of_binary_relational_operators :: State -> String -> AS.RightExp -> (State, String, Address)
gen_tac_of_binary_relational_operators state cur_blck op = gen_tac_of_binary_operators state cur_blck op to_primitive_relational_operator    BinaryRelatAssignment

-- binary math operators: +, -, *, /, //, %, ^
gen_tac_of_binary_arithm_operators :: State -> String -> AS.RightExp -> (State, String, Address)
gen_tac_of_binary_arithm_operators     state cur_blck op = gen_tac_of_binary_operators state cur_blck op to_primitive_arithm_binary_operator BinaryArithmAssignment

-- both arithmetic and relational binary operators
gen_tac_of_binary_operators :: State -> String -> AS.RightExp -> (AS.RightExp -> t) -> (Address -> Address -> Address -> PrimType -> t -> Instruction) -> (State, String, Address)
gen_tac_of_binary_operators state cur_blck r_exp to_primitive_op constructor = 
    let bin_op          = to_primitive_op r_exp
        --s00             = out state cur_blck (Comment $ "start of show bin_op declared at " ++ (print_row_col $ AS.right_exp_pos r_exp))
        (s10, _, r1)    = gen_tac_of_RightExp state cur_blck (AS.sx r_exp)
        (s20, _, r2)    = gen_tac_of_RightExp s10   cur_blck (AS.dx r_exp)
        (tmp_var, s30)  = add_temp_var s20
        r_exp_type      = (AS.right_exp_type r_exp)
        ass_type        = if r_exp_type == T.TBDType then TypeBool else to_primitive_type r_exp_type
        instr           = (constructor tmp_var r1 r2 ass_type bin_op)
        s40             = out s30 cur_blck instr
    in (s40, cur_blck, tmp_var)

-- binary logical operator: AND, OR
--gen_tac_of_binary_logical_operators :: State -> String -> AS.RightExp -> (State, String, Address)
gen_tac_of_binary_logical_operators state cur_blck r_exp =
    let -- case AND/OR ==> create 3 blocks and a temp var to store output
        (s00, blck_true, blck_false, blck_next) = add_blocks_then_else_next state cur_blck
        -- define first instruction jumpif goto parameters
        (fstJumpIf, fstGoToBlck)    = case r_exp of
            (AS.RightExpOr  {}) -> (JumpIfTrue,  blck_true)
            (AS.RightExpAnd {}) -> (JumpIfFalse, blck_false)
        -- 1) generate code for right-exp sx
        (s10, cur_blck10, cond_sx)  = gen_tac_of_RightExp s00 cur_blck   (AS.sx r_exp)
        -- 2) add goto: JumpIfFalse to blck_true (for OR) / JumpIfTrue to blck_false (for AND) 
        s20                         = out s10 cur_blck10  (fstJumpIf fstGoToBlck cond_sx)
        -- 3) generate code for right-exp dx
        (s30, cur_blck20, cond_dx)  = gen_tac_of_RightExp s20 cur_blck10 (AS.dx r_exp)
        -- 4) create a temp variable to store output result and return address
        (tmp_var, s40)              = add_temp_var s30
        -- BLOCK TRUE
        -- 5) assign true to output tempvar in blck_true
        s50                         = out s40 blck_true (NullAssignment { l = tmp_var, r = (AddressBool True),  assign_type = TypeBool })
        -- 6) add goto: Jump from blck_true to blck_next
        s60                         = out s50 blck_true  (Jump { goto = blck_next })
        -- BLOCK FALSE
        -- 7) add goto: JumpIfFalse to blck_false (for both AND/OR)
        s70                         = out s60 cur_blck20  (JumpIfFalse blck_false cond_dx)
        -- 8) assign false to output tempvar
        s80                         = out s70 blck_false (NullAssignment { l = tmp_var, r = (AddressBool False), assign_type = TypeBool })
        -- 9) add goto: Jump from blck_false to blck_next (necessary only if r_exp dx has added blocks between blck_false and blck_next
        --s90                         = if blck_false == cur_blck20 then s80 else out s80 blck_false (Jump { goto = blck_next })
        -- DEBUG COMMENT
        --s100                        = out s90 cur_blck20 (Comment $ cur_blck ++ " " ++ blck_true ++ " " ++ blck_false ++ " " ++ blck_next  ++ " " ++ cur_blck10  ++ " " ++ cur_blck20)
    in (s80, blck_next, tmp_var)


--________________________________ Read / Write Primitive ________________________________________

gen_tac_of_WritePrimitive :: State -> String -> AS.WritePrimitive -> (State, String)
gen_tac_of_WritePrimitive state cur_blck prim_write = 
    let r_exp            = (AS.write_exp prim_write)
        prim_write_name  = AS.make_label_WritePrimitive prim_write
        (s10, cur_blck10, address)   = gen_tac_of_RightExp state cur_blck r_exp
        prim_type        = to_primitive_type (AS.right_exp_type r_exp)
        s20              = out s10 cur_blck10 (Parameter { param = address, param_type = prim_type })
        s30              = out s20 cur_blck10 (ProcCall { p_name = prim_write_name, num_params = 1 })
    in  (s30, cur_blck10)

-----------------------------------------------------------------------------------------------------------------------------

-- ____________________________ AUX FUNCTIONS ________________________________________

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

----------------------------------------------------------------------------------------------------------------------------

-- ____________________________ PRETTY PRINTER ________________________________________

pretty_printer_tac :: State -> String
pretty_printer_tac (State t str _ _ _ _) = "# TAC START\n" ++ "# FUNCTIONS AND PROCEDURE\n" ++ (pretty_printer_tac_aux t str) ++ "#TAC END\n" where
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
