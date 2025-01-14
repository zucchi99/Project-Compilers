
-- ghci -outputdir src/bin src/modules/TAC.hs src/modules/Types.hs src/modules/ErrorMessage.hs src/modules/AbstractSyntax.hs

module TAC where

import qualified AbstractSyntax as AS
import qualified Types as T
import qualified Env as E
import ErrM
import Data.List
import qualified Data.Map as Map

-- ___________ PRIMITIVE TYPE ___________

-- primitive types
data PrimType =     
    TypeInt
    | TypeReal
    | TypeChar
    | TypeBool
    | TypeAddr

size_of_PrimType :: PrimType -> Int
size_of_PrimType TypeInt  = 4
size_of_PrimType TypeReal = 8
size_of_PrimType TypeChar = 1
size_of_PrimType TypeBool = 1
size_of_PrimType TypeAddr = 8

-- ___________ PRIMITIVE ADDRESS TYPE ___________

data Address = 
    AddressInt          Int
    | AddressReal       Double
    | AddressChar       Char
    | AddressBool       Bool
    | AddressProgramVar String
    | AddressTempVar    String

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
    -- if false r goto label
    | JumpIfFalse           { goto :: String, cond :: Address }
    -- if r1 rel r2 goto label
    | JumpConditional       { goto :: String, r1,r2 :: Address, rel_op :: BinaryRelatOp }
    -- l = array[i]
    | ReadFromArray         { array :: Address, i,l :: Address, assign_type :: PrimType }
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

-- ___________ SET OF OPERATIONS ___________

data UnaryOp =
    Coerce          { type_from, type_to    :: PrimType }
    | Negate        { negation_type         :: PrimType }
    | Not 

data BinaryArithmOp = 
    Sum             { binary_arithm_type    :: PrimType }
    | Subtract      { binary_arithm_type    :: PrimType }
    | Multiply      { binary_arithm_type    :: PrimType }
    | Divide        { binary_arithm_type    :: PrimType }
    | Remainder     { binary_arithm_type    :: PrimType }
    | Power         { binary_arithm_type    :: PrimType }
    | AddressSum

-- binary_relat_type indicates the types of the operand ==> 2 >:int 1, 3.0 >:real 1.0
data BinaryRelatOp = 
    GreaterThan      { binary_relat_type    :: PrimType } 
    | GreaterEqual   { binary_relat_type    :: PrimType } 
    | LessThan       { binary_relat_type    :: PrimType } 
    | LessEqual      { binary_relat_type    :: PrimType } 
    | Equal          { binary_relat_type    :: PrimType } 
    | NotEqual       { binary_relat_type    :: PrimType } 

-- ___________ TAC CODE TYPE ___________

data BlockType
    --StartBlockType                                                              -- start computation (invoke main)
    = ExcepHandlerBlockType                                                     -- block for handle exceptions
    | MainBlockType { pos :: (Int, Int), is_start :: Bool }                     -- block for main code declaration
    | FuncBlockType { pos :: (Int, Int), is_start :: Bool, fun_name :: String } -- block for function declaration
    | ProcBlockType { pos :: (Int, Int), is_start :: Bool, prc_name :: String } -- block for procedure declaration
    | TempBlockType { b_idx :: Int }                                            -- block defined by compiler
    | StringBlockType { b_idx :: Int }                                          -- block for storage a string literal

instance Eq BlockType where
    --StartBlockType        == StartBlockType         = True
    (MainBlockType {})    == (MainBlockType {})     = True
    (FuncBlockType {})    == (FuncBlockType {})     = True
    (ProcBlockType {})    == (ProcBlockType {})     = True
    (TempBlockType {})    == (TempBlockType {})     = True
    (StringBlockType {})  == (StringBlockType {})   = True
    ExcepHandlerBlockType == ExcepHandlerBlockType  = True
    _                     == _                      = False      

data Block = Block {
    block_name  :: String,
    code        :: [ Instruction ]
} deriving (Show)

data State = State { 
    tac                          :: [ Block ],
    strings                      :: [ String ],
    string_constants             :: [ (String, Address) ], -- map : string constant identifier -> AddressTempVar of the string TODO CHANGE TO MAP
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

initialize_state :: (Int, Int) -> (String, String, State)
initialize_state main_pos = 
    let main_type_start = (MainBlockType main_pos True)
        main_type_end   = (MainBlockType main_pos False)
        main_name_start = make_block_label main_type_start
        main_name_end   = make_block_label main_type_end
        --s10             = add_block empty_state StartBlockType [ (Jump main_name_start) ]
        s20             = add_block empty_state main_type_start [ ]
        s30             = add_block s20 main_type_end   [ Return ]
        s40             = add_block s30 ExcepHandlerBlockType [ Comment "Defined by back-end" ]
    in  (main_name_start, main_name_end, s40)
   
to_primitive_relational_operator :: AS.RightExp -> BinaryRelatOp
to_primitive_relational_operator r_exp = 
    let t = to_primitive_type (AS.right_exp_type (AS.dx r_exp))
    in  case r_exp of
        (AS.RightExpLess {})           -> LessThan t
        (AS.RightExpGreater {})        -> GreaterThan t
        (AS.RightExpLessEqual {})      -> LessEqual t
        (AS.RightExpGreaterEqual {})   -> GreaterEqual t
        (AS.RightExpEqual {})          -> Equal t
        (AS.RightExpNotEqual {})       -> NotEqual t

to_primitive_arithm_binary_operator :: AS.RightExp -> BinaryArithmOp
to_primitive_arithm_binary_operator r_exp = 
    let t = to_primitive_type (AS.right_exp_type r_exp)
    in  case r_exp of
        (AS.RightExpPlus   {}) -> Sum t
        (AS.RightExpMinus  {}) -> Subtract t
        (AS.RightExpTimes  {}) -> Multiply t
        (AS.RightExpDivide {}) -> Divide t -- division real 
        (AS.RightExpDiv    {}) -> Divide t -- division integer
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

make_ident_array_label :: String -> (Int, Int) -> Address -> [Char]
make_ident_array_label name pos idx = (make_ident_var_label name pos) ++ "[" ++ (show idx) ++ "]"

make_ident_var_label :: String -> (Int, Int) -> String
make_ident_var_label name pos = name ++ "?" ++ (print_row_col pos)

make_temp_var_label :: Int -> String
make_temp_var_label i = "tmp?" ++ (show i)

make_block_label :: BlockType -> String
make_block_label ExcepHandlerBlockType               = "excep_handler"
--make_block_label StartBlockType                      = "start->program"
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
    let (main_name_start, main_name_end, state) = initialize_state pos
        (s10, _) = gen_tac_of_Block state main_name_start code "" ""
    in reverse_TAC $ s10

gen_tac_of_Block :: State -> String -> AS.Block -> String -> String -> (State, String)
gen_tac_of_Block state cur_blck (AS.Block []        []        pos   _   _  ) blck_next blck_guard = (state, cur_blck)
-- DECLARATIONS
gen_tac_of_Block state cur_blck (AS.Block (d:decls) stmts     pos env err) blck_next blck_guard = 
    let (s10, cur_blck1) = gen_tac_of_Declaration state cur_blck d
    in gen_tac_of_Block s10 cur_blck1 (AS.Block decls stmts pos env err) blck_next blck_guard
-- STATEMENTS
gen_tac_of_Block state cur_blck (AS.Block []        (s:stmts) pos env err) blck_next blck_guard = 
    -- after statements
    let (s10, cur_blck1) = gen_tac_of_Statement state cur_blck s blck_next blck_guard
    in  gen_tac_of_Block s10 cur_blck1 (AS.Block [] stmts pos env err) blck_next blck_guard

--________________________________ Declaration __________________________________________

gen_tac_of_Declaration :: State -> String -> AS.Declaration -> (State, String)
gen_tac_of_Declaration state cur_blck declaration = 
    case declaration of 
            -- only string to be actually treated
            (AS.DeclarationCostant {})      -> gen_tac_of_DeclarationCostant   state cur_blck declaration
            (AS.DeclarationVariable {})     -> gen_tac_of_DeclarationVariable  state cur_blck declaration
            (AS.DeclarationFunction {})     -> (gen_tac_of_DeclarationFunction  state declaration, cur_blck)
            (AS.DeclarationProcedure {})    -> (gen_tac_of_DeclarationProcedure state declaration, cur_blck)

gen_tac_of_DeclarationCostant :: State -> String -> AS.Declaration -> (State, String)
gen_tac_of_DeclarationCostant state cur_blck decl_const = 
    let value = (AS.constant_value decl_const)
    in  case value of
        (AS.RightExpString{}) -> 
            let (s10, cur_blck1, address) = gen_tac_of_string state cur_blck value
                ident_name                = (AS.id_name (AS.constant_name decl_const))
                s20                       = add_to_string_constants_list s10 ident_name address
            in  (s20, cur_blck1)
        _   -> (state, cur_blck)

gen_tac_of_DeclarationVariable :: State -> String -> AS.Declaration -> (State, String)
gen_tac_of_DeclarationVariable state cur_blck decl_var =
    if (T.is_array (AS.variable_type decl_var)) || (isNothing (AS.variable_value_maybe decl_var))
    -- if no initialization do nothing (array can never be initialized)
    then (state, cur_blck) 
    -- initialize
    else    let Just assgn_stmt           = (AS.variable_value_maybe decl_var)
                t                         = to_primitive_type (AS.variable_type decl_var)
                l_addr                    = gen_tac_of_Ident (AS.variable_name decl_var)
                (s10, cur_blck10, r_addr) = gen_tac_of_RightExp state cur_blck assgn_stmt
                s20                       = out s10 cur_blck10 (NullAssignment { l = l_addr, r = r_addr, assign_type = t })
            in (s20, cur_blck10)

gen_tac_of_DeclarationFunction :: State -> AS.Declaration -> State
gen_tac_of_DeclarationFunction  state decl_fun = gen_tac_of_declaration_fun_proc state decl_fun FuncBlockType True

gen_tac_of_DeclarationProcedure :: State -> AS.Declaration -> State
gen_tac_of_DeclarationProcedure state decl_prc = gen_tac_of_declaration_fun_proc state decl_prc ProcBlockType False

gen_tac_of_declaration_fun_proc :: State -> AS.Declaration -> ((Int, Int) -> Bool -> String -> BlockType) -> Bool -> State
gen_tac_of_declaration_fun_proc state decl constructor is_fun = 
    let fun_prc_pos         = (AS.declaration_pos decl)
        fun_prc_name        = (AS.id_name (AS.declaration_name decl))
        block_type_start    = (constructor fun_prc_pos True fun_prc_name)
        block_name_start    = make_block_label block_type_start
        block_type_end      = (block_type_start { is_start = False })
        block_name_end      = make_block_label block_type_end
        s10                 = add_block state block_type_start []
        s20                 = add_block s10 block_type_end []
        (s30, cur_blck10)   = (s20, block_name_start) --get_tac_of_parameters s20 block_name_start (AS.declaration_params decl)
        (s40, cur_blck20)   = case AS.declaration_body_maybe decl of
                                (Just b) -> gen_tac_of_Block s30 cur_blck10 b "" ""
                                Nothing  -> (s30, cur_blck10)
        return_addr         = (AddressProgramVar (make_ident_var_label fun_prc_name fun_prc_pos))
        s50                 = if is_fun 
                                then out s40 block_name_end (RetVal { value = return_addr, return_type = to_primitive_type (AS.function_type decl) })
                                else out s40 block_name_end Return
    in  s50

{-
get_tac_of_parameters :: State -> String -> [AS.Declaration] -> (State, String)
get_tac_of_parameters state cur_blck [] = (state, cur_blck)
get_tac_of_parameters state cur_blck (d:decls) = 
    case (AS.param_type_maybe d) of
        Just ValueResult -> 
            let 
            in  get_tac_of_parameters state cur_blck decls
        -- if param is not given by ValueResult do nothing (cannot be a func/proc neither we have default values for params) 
        _                -> get_tac_of_parameters state cur_blck decls
-}

--________________________________ Statement __________________________________________

-- Statement wrapper
gen_tac_of_Statement :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_Statement state cur_blck stmt blck_next blck_guard = gen_tac_fun state cur_blck stmt blck_next blck_guard
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

gen_tac_of_StatementBlock :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_StatementBlock state cur_blck blck_stmt blck_next blck_guard = gen_tac_of_Block state cur_blck (AS.block blck_stmt) blck_next blck_guard

gen_tac_of_StatementIf :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_StatementIf state cur_blck if_stmt blck_next blck_guard =
    let show_stmt        = " statement declared at " ++ (print_row_col $ AS.statement_pos if_stmt)
        --s00             = out state cur_blck (Comment $ "start of" ++ show_stmt)
        maybe_else_body = (AS.else_body_maybe if_stmt)
        -- create temp blocks: block-then, block-else (also if there is no else block, easier to code), block-next
        (s10, block_then, block_else, block_next) = add_blocks_then_else_next state cur_blck
        s14             = out s10 block_then (Comment $ "start of IF THEN " ++ block_then ++ " added by" ++ show_stmt)
        s15             = out s14 block_else (Comment $ "start of IF ELSE " ++ block_else ++ " added by" ++ show_stmt)
        s16             = out s15 block_next (Comment $ "start of IF NEXT " ++ block_next ++ " added by" ++ show_stmt)
        --s17             = out s16 block_else (Comment $ "is ELSE block empty? " ++ (show $ isNothing maybe_else_body))
        -- generate code for condition 
        (s20, cur_block10, cond_addr) = gen_tac_of_RightExp s16 cur_blck (AS.condition if_stmt)
        -- add instruction : if_false cond_addr then goto block_else
        s30              = out s20 cur_block10 (JumpIfFalse block_else cond_addr)
        -- if some blocks has been added between cur_block and block_then ==> add jump to block_then
        --s40              = if block_else == cur_block10 then s30 else out s30 cur_block10 (Jump { goto = block_then })
        -- add then body
        (s50, cur_blck_then) = gen_tac_of_Statement s30 block_then (AS.then_body if_stmt) blck_next blck_guard
        -- add goto next (skip else body)
        s60              = out s50 cur_blck_then (Jump { goto = block_next })
        -- add else statement and body<
        (s70, cur_blck_else) = gen_tac_of_ElseBlock s60 block_else maybe_else_body blck_next blck_guard
        -- add goto next (necessary only if elseBlock has added blocks between else and next blocks )
        --s65              = out s60 block_else (Comment $ "block_else: " ++ block_else ++ ", cur_blck_else: " ++ cur_blck_else ++ " equal?" ++ (show $ block_else == cur_blck_else))
        s80              = if block_else == cur_blck_else then s70 else out s70 block_else (Jump { goto = block_next })
    in (s70, block_next)

gen_tac_of_ElseBlock :: State -> String -> Maybe AS.ElseBlock -> String -> String -> (State, String)
gen_tac_of_ElseBlock state cur_blck Nothing          _         _          = (state, cur_blck)
gen_tac_of_ElseBlock state cur_blck (Just else_blck) blck_next blck_guard = gen_tac_of_Statement state cur_blck (AS.else_body else_blck) blck_next blck_guard

gen_tac_of_StatementFor :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_StatementFor state cur_blck for_stmt blck_next blck_guard = 
    let
        (s11, cur_blck10) = gen_tac_of_VariableAssignment state cur_blck (AS.for_var for_stmt) -- blck_next blck_guard
        (s12, blck_then, blck_guard, blck_next) = add_blocks_then_else_next s11 cur_blck10
        show_stmt       = " statement declared at " ++ (print_row_col $ AS.statement_pos for_stmt)
        name_stmt       = "FOR "
        for_index       = gen_tac_of_for_var for_stmt
        s14             = out s12 blck_then  (Comment $ "start of " ++ name_stmt ++ "THEN  " ++ blck_then  ++ " added by" ++ show_stmt)
        s15             = out s14 blck_guard (Comment $ "start of " ++ name_stmt ++ "GUARD " ++ blck_guard ++ " added by" ++ show_stmt)
        s16             = out s15 blck_next  (Comment $ "start of " ++ name_stmt ++ "NEXT  " ++ blck_next  ++ " added by" ++ show_stmt)
        s20                          = out s16 cur_blck (Jump { goto = blck_guard })
        (s30, cur_blck11, cond_addr) = gen_tac_of_RightExp s20 blck_guard (AS.condition for_stmt)
        s40                          = out s30 cur_blck11 (JumpIfTrue { goto = blck_then, cond = cond_addr })
        (s50, _)                     = gen_tac_of_Statement s40 blck_then (AS.then_body for_stmt) blck_next blck_guard
        s60                          = out s50 blck_then (BinaryArithmAssignment { l = for_index, r1 = for_index, r2 = (AddressInt 1), assign_type = TypeInt, bin_arit_op = get_op_for (AS.condition for_stmt)})
    in  (s60, blck_next)

gen_tac_of_for_var :: AS.Statement -> Address
gen_tac_of_for_var for_stmt = gen_tac_of_Ident (AS.left_exp_name (AS.left_exp_assignment (AS.for_var for_stmt)))

get_op_for :: AS.RightExp -> BinaryArithmOp
get_op_for (AS.RightExpLessEqual {}) = Sum      { binary_arithm_type = TypeInt }
get_op_for _                         = Subtract { binary_arithm_type = TypeInt }

gen_tac_of_StatementWhile :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_StatementWhile state cur_blck while_stmt _ _ = gen_tac_of_unbounded_repetition state cur_blck while_stmt False 

gen_tac_of_StatementRepeatUntil :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_StatementRepeatUntil state cur_blck rep_stmt _ _ = gen_tac_of_unbounded_repetition state cur_blck rep_stmt True

gen_tac_of_unbounded_repetition :: State -> String -> AS.Statement -> Bool -> (State, String)
gen_tac_of_unbounded_repetition state cur_blck unb_stmt is_repeat =
    let (s10, blck_then, blck_guard, blck_next) = add_blocks_then_else_next state cur_blck
        show_stmt       = " statement declared at " ++ (print_row_col $ AS.statement_pos unb_stmt)
        name_stmt       = if is_repeat then "REPEAT UNTIL " else "WHILE DO "
        s14             = out s10 blck_then  (Comment $ "start of " ++ name_stmt ++ "THEN  " ++ blck_then  ++ " added by" ++ show_stmt)
        s15             = out s14 blck_guard (Comment $ "start of " ++ name_stmt ++ "GUARD " ++ blck_guard ++ " added by" ++ show_stmt)
        s16             = out s15 blck_next  (Comment $ "start of " ++ name_stmt ++ "NEXT  " ++ blck_next  ++ " added by" ++ show_stmt)
        s20                          = if is_repeat then s16 else out s16 cur_blck (Jump { goto = blck_guard })
        (s30, cur_blck10, cond_addr) = gen_tac_of_RightExp s20 blck_guard (AS.condition unb_stmt)
        s40                          = out s30 cur_blck10 (JumpIfTrue { goto = blck_then, cond = cond_addr })
        (s50, _)                     = gen_tac_of_Statement s40 blck_then (AS.then_body unb_stmt) blck_next blck_guard
    in  (s50, blck_next)

gen_tac_of_StatementAssign :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_StatementAssign state cur_blck stmt _ _ = gen_tac_of_VariableAssignment state cur_blck (AS.assign stmt)

gen_tac_of_VariableAssignment :: State -> String -> AS.Assign -> (State, String)
gen_tac_of_VariableAssignment state cur_blck assgn_stmt = 
    let -- s00                      = out state cur_blck (Comment $ "start of variable assignment declared at " ++ (print_row_col $ AS.assign_pos assgn_stmt))
        l_exp = (AS.left_exp_assignment assgn_stmt)
        r_exp = (AS.right_exp_assignment assgn_stmt)
        -- BEFORE LEFT EXPRESSION
        (s10, prim_type, l_addr, is_pnt)  = gen_tac_of_LeftExp state cur_blck10 l_exp True
        -- AFTER RIGHT EXPRESSION
        (s20, cur_blck10, r_addr) = gen_tac_of_RightExp s10 cur_blck r_exp
        -- add assignment code
        --s25                       = out s20 cur_blck10 (Comment $ (show l_addr) ++ " = " ++ (show r_addr) ++ " : " ++ (show prim_type))
        --s25                       = out s20 cur_blck10 (Comment $ (show l_exp))
        --s26                       = out s25 cur_blck10 (Comment $ (show (AS.left_exp_type l_exp)))
        s30                       = add_assignment_instruction s20 cur_blck10 l_exp prim_type l_addr r_addr is_pnt
    in (s30, cur_blck10)

add_assignment_instruction :: State -> String -> AS.LeftExp -> PrimType -> Address -> Address -> Bool -> State
add_assignment_instruction state cur_blck l_exp prim_type l_addr r_addr is_pnt = 
    if   is_pnt 
    then out state cur_blck (WritePointerValue { l = l_addr, r = r_addr })
    else out state cur_blck (NullAssignment    { l = l_addr, r = r_addr, assign_type = prim_type })

gen_tac_of_StatementFuncProcCall :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_StatementFuncProcCall state cur_blck stmt _ _ = 
    let params = AS.call_params stmt
        funproc_name = AS.id_name (AS.call_name stmt)
        env = AS.statement_env stmt
        (s40, cur_blck10, _) = gen_tac_FuncProcCall state cur_blck params funproc_name env
    in (s40, cur_blck10)

gen_tac_of_list_RightExp :: State -> String -> [AS.RightExp] -> [Address] -> (State, String, [Address])
gen_tac_of_list_RightExp state cur_blck [] addrs = (state, cur_blck, addrs)
gen_tac_of_list_RightExp state cur_blck (x:xs) addrs = gen_tac_of_list_RightExp new_state cur_blck1 xs (addr:addrs)
    where (new_state, cur_blck1, addr) = gen_tac_of_RightExp state cur_blck x

out_params :: State -> [Char] -> [Address] -> [PrimType] -> State
out_params state cur_blck10 []           []             = state
out_params state cur_blck10 (addr:addrs) (taddr:taddrs) = out_params new_state cur_blck10 addrs taddrs
    where new_state = out state cur_blck10 (Parameter { param = addr, param_type = taddr })

gen_tac_of_StatementRead :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_StatementRead state cur_blck read_stmt _ _ = gen_tac_of_ReadPrimitive state cur_blck (AS.read_primitive read_stmt)

gen_tac_of_StatementWrite :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_StatementWrite state cur_blck wrt_stmt _ _ = gen_tac_of_WritePrimitive state cur_blck (AS.write_primitive wrt_stmt)

gen_tac_of_StatementBreak :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_StatementBreak state cur_blck stmt blck_next _ = (out state cur_blck (Jump { goto = blck_next }), cur_blck)

gen_tac_of_StatementContinue :: State -> String -> AS.Statement -> String -> String -> (State, String)
gen_tac_of_StatementContinue state cur_blck stmt _ blck_guard = (out state cur_blck (Jump { goto = blck_guard }), cur_blck)

--________________________________ Identifier ________________________________________

gen_tac_of_Ident :: AS.Ident -> Address
gen_tac_of_Ident ident = 
    let id_name = (AS.id_name ident)
        decl_pos = (get_declaration_position (AS.ident_env ident) id_name)
    in (AddressProgramVar $ make_ident_var_label id_name decl_pos)

--________________________________ Left Expression __________________________________________

gen_tac_of_LeftExp :: State -> String -> AS.LeftExp -> Bool -> (State, PrimType, Address, Bool)
gen_tac_of_LeftExp state cur_blck l_exp is_lexp = 
    let prim_type = to_primitive_type (AS.left_exp_type l_exp)
        s10       = state --out state cur_blck (Comment $ ( (show prim_type) ++ " -> " ++ (show l_exp)))
    in case l_exp of 
        -- variable
        (AS.LeftExpIdent {})           -> (s10, prim_type, gen_tac_of_Ident (AS.left_exp_name l_exp), False)
        (AS.LeftExpForIterator {})     -> (s10, prim_type, gen_tac_of_Ident (AS.left_exp_name l_exp), False)
        -- only string-constants (non-string constants has been already substituted by static semantic)
        (AS.LeftExpConst {})           -> (s10, prim_type, get_address_from_string_constant s10 (AS.id_name (AS.left_exp_name l_exp)), False)
        -- pointers
        (AS.LeftExpArrayAccess {})     -> gen_tac_of_ArrayAccess           s10 cur_blck prim_type l_exp False
        (AS.LeftExpPointerValue {})    -> gen_tac_of_LeftExpPointerValue   s10 cur_blck prim_type l_exp
        (AS.LeftExpPointerAddress {})  -> gen_tac_of_LeftExpPointerAddress s10 cur_blck prim_type l_exp

gen_tac_of_LeftExpPointerValue :: State -> String -> PrimType -> AS.LeftExp -> (State, PrimType, Address, Bool)
gen_tac_of_LeftExpPointerValue state cur_blck prim_type ptr_val =
    let (s10, ident_addr, _) = get_pointer_ident state cur_blck prim_type ptr_val
    in  (s10, prim_type, ident_addr, True)

gen_tac_of_LeftExpPointerAddress :: State -> String -> PrimType -> AS.LeftExp -> (State, PrimType, Address, Bool)
gen_tac_of_LeftExpPointerAddress state cur_blck prim_type ptr_val =
    let (s10, ident_addr, is_array) = get_pointer_ident state cur_blck prim_type ptr_val
        (tmp_addr, s20)             = add_temp_var s10
        s30                         = out s20 cur_blck (ReadPointerAddress { l = tmp_addr, pointer = ident_addr })
        (s40, out_addr)             = if is_array then (s10, ident_addr) else (s30, tmp_addr)
    in  (s40, TypeAddr, tmp_addr, False)

get_pointer_ident :: State -> String -> PrimType -> AS.LeftExp -> (State, Address, Bool)
get_pointer_ident state cur_blck prim_type l_exp = 
    case l_exp of
        (AS.LeftExpPointerValue {})   -> get_pointer_ident state cur_blck prim_type (AS.pointer_value   l_exp)
        (AS.LeftExpPointerAddress {}) -> get_pointer_ident state cur_blck prim_type (AS.pointer_address l_exp)
        (AS.LeftExpArrayAccess {})    -> 
            let (s10, _, addr, _) = gen_tac_of_ArrayAccess state cur_blck prim_type l_exp True
            in (s10, addr, True)
        _                             -> (state, gen_tac_of_Ident (AS.left_exp_name l_exp), False)
    
gen_tac_of_ArrayAccess :: State -> String -> PrimType -> AS.LeftExp -> Bool -> (State, PrimType, Address, Bool)
gen_tac_of_ArrayAccess state cur_blck primitive_type array is_address = 
    let decl_pos             = (get_declaration_position (AS.left_exp_env array) id_name)
        (id_name, arr_sizes) = get_multi_array_name_and_length_from_lexp array
        (s10, addr_idx)      = linearize_multi_array state cur_blck arr_sizes array
        (s20, addr, is_pnt)  = make_array_access_address s10 cur_blck array id_name decl_pos addr_idx is_address
    in  (s20, primitive_type, addr, is_pnt)

make_array_access_address :: State -> String -> AS.LeftExp -> String -> (Int, Int) -> Address -> Bool -> (State, Address, Bool)
make_array_access_address state cur_blck l_exp id_name decl_pos addr_idx is_address = 
    case l_exp of
        (AS.LeftExpArrayAccess  {}) -> make_array_access_address state cur_blck (AS.array_name l_exp) id_name decl_pos addr_idx is_address
        (AS.LeftExpPointerValue {}) -> array_to_pointer_value state cur_blck id_name decl_pos addr_idx       
        _                           ->  if is_address
                                        then array_to_pointer_address  state cur_blck id_name decl_pos addr_idx
                                        else array_to_array            state cur_blck id_name decl_pos addr_idx

array_to_array :: State -> String -> String -> (Int, Int) -> Address -> (State, Address, Bool)
array_to_array state cur_blck id_name decl_pos addr_idx =
    let s10             = out state cur_blck (Comment $ "Make address of array with index")
        array_address   = (AddressProgramVar $ make_ident_array_label id_name decl_pos addr_idx)
    in (s10, array_address, False)

array_to_pointer_value :: State -> String -> String -> (Int, Int) -> Address -> (State, Address, Bool)
array_to_pointer_value state cur_blck id_name decl_pos addr_idx = 
    let pointer_address = AddressProgramVar $ make_ident_var_label id_name decl_pos
        (tmp_addr, s10) = add_temp_var state
        s20             = out s10 cur_blck (Comment $ "Sum address pointer with index value")
        s30             = out s20 cur_blck (BinaryArithmAssignment { l = tmp_addr, r1 = pointer_address, r2 = addr_idx, assign_type = TypeInt, bin_arit_op = (Sum TypeInt) }) 
    in (s30, tmp_addr, True)
    
array_to_pointer_address :: State -> String -> String -> (Int, Int) -> Address -> (State, Address, Bool)
array_to_pointer_address state cur_blck id_name decl_pos addr_idx = 
    let pointer_address = AddressProgramVar $ make_ident_var_label id_name decl_pos
        (tmp_addr, s10) = add_temp_var state
        s20             = out s10 cur_blck (Comment $ "Read array pointer ans then sum with index value")
        s30             = out s20 cur_blck (ReadPointerAddress     { l = tmp_addr, pointer = pointer_address } ) 
        s40             = out s30 cur_blck (BinaryArithmAssignment { l = tmp_addr, r1 = tmp_addr, r2 = addr_idx, assign_type = TypeInt, bin_arit_op = (Sum TypeInt) }) 
    in (s40, tmp_addr, True)

-- matrix sizes : [ N ][ M ][ P ]
-- matrix[ i ][ j ][ k ] = array[ i*(M*P) + j*P + k ]
linearize_multi_array :: State -> String -> [(Int,Int,Int)] -> AS.LeftExp -> (State, Address)
linearize_multi_array state cur_blck arr_sizes l_exp =
    let (s10, addr_idxs)     = get_array_indexes_raw l_exp state cur_blck
        s14                  = out s10 cur_blck (Comment $ "check array bounds: " ++ (show addr_idxs) ++ " in " ++ (show arr_sizes))
        s15                  = check_array_bounds s14 cur_blck addr_idxs ( map ( \ (x,y,_) -> (x,y) ) arr_sizes)
        offsets              = get_offsets_from_array_lengths $ tail $ map ( \ (_,_,x) -> x ) arr_sizes
        (s20, offset_by_idx) = get_array_indexes_with_offset s15 cur_blck addr_idxs offsets
        (addr_idx, s30)      = add_temp_var s20
        s40                  = out s30 cur_blck (NullAssignment { l = addr_idx, r = (AddressInt 0),  assign_type = TypeInt })
        s50                  = sum_all_offsets_by_dim s40 cur_blck addr_idx offset_by_idx
        prim_type            = to_primitive_type (T.get_basic_type (AS.left_exp_type l_exp))
        sizeof_prim_type     = size_of_PrimType prim_type 
        s60                  = out s50 cur_blck (Comment $ "Multiply index by sizeof(" ++ (show prim_type) ++ ") = " ++ (show sizeof_prim_type) )
        s70                  = out s60 cur_blck (BinaryArithmAssignment { l = addr_idx, r1 = addr_idx, r2 = (AddressInt sizeof_prim_type), assign_type = TypeInt, bin_arit_op = (Multiply TypeInt) })
    in  (s70, addr_idx)

check_array_bounds :: State -> String -> [Address] -> [(Int,Int)] -> State
check_array_bounds state cur_blck []       _             = state
check_array_bounds state cur_blck (i:idxs) ((l,r):sizes) = 
    let block_excep_handler = make_block_label ExcepHandlerBlockType
        s10 = out state cur_blck (JumpConditional { goto = block_excep_handler, r1 = i, r2 = (AddressInt l), rel_op = (LessThan    TypeInt) } )
        s20 = out s10   cur_blck (JumpConditional { goto = block_excep_handler, r1 = i, r2 = (AddressInt r), rel_op = (GreaterThan TypeInt) } )
    in  check_array_bounds s20 cur_blck idxs sizes

get_array_indexes_raw :: AS.LeftExp -> State -> String -> (State, [Address])
get_array_indexes_raw l_exp state cur_blck = 
    let (s, l) = get_array_indexes_aux state (get_array_indexes_as_rexp l_exp) []
    in  (s, reverse l)
        where
            get_array_indexes_aux state []         acc = (state, acc)
            get_array_indexes_aux state (r_exp:xs) acc = 
                let (s10, _, addr) = gen_tac_of_RightExp state cur_blck r_exp
                in  get_array_indexes_aux s10 xs (addr:acc)

get_array_indexes_as_rexp :: AS.LeftExp -> [AS.RightExp]
get_array_indexes_as_rexp l_exp = 
    case l_exp of
        (AS.LeftExpArrayAccess {}) -> (get_array_indexes_as_rexp (AS.array_name l_exp)) ++ (AS.array_pos l_exp)
        _                          -> []

get_multi_array_name_and_length_from_lexp :: AS.LeftExp -> (String, [(Int,Int,Int)])
get_multi_array_name_and_length_from_lexp l_exp = 
    case l_exp of
        (AS.LeftExpArrayAccess {})  -> get_multi_array_name_and_length_from_lexp (AS.array_name l_exp)
        (AS.LeftExpPointerValue {}) -> get_multi_array_name_and_length_from_lexp (AS.pointer_value l_exp)
        _                           -> ((AS.id_name (AS.left_exp_name l_exp)), T.get_multi_array_length (AS.left_exp_type l_exp))

get_offsets_from_array_lengths :: [Int] -> [Int]
get_offsets_from_array_lengths []     = [1]
get_offsets_from_array_lengths (x:xs) = (foldr (*) 1 (x:xs)) : (get_offsets_from_array_lengths xs)

get_array_indexes_with_offset :: State -> String -> [Address] -> [Int] -> (State, [Address])
get_array_indexes_with_offset state cur_blck addresses offsets = 
    get_array_indexes_with_offset_aux state cur_blck addresses offsets [] where
        get_array_indexes_with_offset_aux state cur_blck []               _             acc = (state, acc)
        get_array_indexes_with_offset_aux state cur_blck _                []            acc = (state, acc)
        get_array_indexes_with_offset_aux state cur_blck (addr:addresses) (off:offsets) acc = 
            let (tmp_var, s10) = add_temp_var state
                -- calculate offset for each index
                s20            = out s10 cur_blck (BinaryArithmAssignment { l = tmp_var, r1 = addr, r2 = (AddressInt off), assign_type = TypeInt, bin_arit_op = (Multiply TypeInt) })
            in  get_array_indexes_with_offset_aux s20 cur_blck addresses offsets (tmp_var:acc)

sum_all_offsets_by_dim :: State -> String -> Address -> [Address] -> State
sum_all_offsets_by_dim state cur_blck addr_arr offset_by_idx =
    sum_all_offsets_by_dim_aux state cur_blck addr_arr offset_by_idx 
        where
            sum_all_offsets_by_dim_aux state cur_blck addr_arr []          = state
            sum_all_offsets_by_dim_aux state cur_blck addr_arr (offset:xs) = 
                let s20 = out state cur_blck (BinaryArithmAssignment { l = addr_arr, r1 = addr_arr, r2 = offset, assign_type = TypeInt, bin_arit_op = (Sum TypeInt) })
                in  sum_all_offsets_by_dim_aux s20 cur_blck addr_arr xs


--________________________________ Right Expression __________________________________________

-- Right Expression wrapper
gen_tac_of_RightExp :: State -> String -> AS.RightExp -> (State, String, Address)
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
        -- unary arithmetic operators
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
        (AS.RightExpNotEqual {})        -> gen_tac_of_binary_relational_operators state cur_blck r_exp
        -- function / procedure call TODO
        (AS.RightExpFuncProcCall {})    -> gen_tac_of_RightExpFuncProcCall        state cur_blck r_exp
        -- use a LeftExp as RightExp
        (AS.RightExpLeftExp {})         -> gen_tac_of_RightExpLeftExp             state cur_blck r_exp
        -- coerce a rightexp to make it compatible with expected type TODO
        (AS.RightExpCoercion {})        -> gen_tac_of_RightExpCoercion            state cur_blck r_exp

gen_tac_of_RightExpFuncProcCall :: State -> String -> AS.RightExp -> (State, String, Address)
gen_tac_of_RightExpFuncProcCall state cur_blck r_exp = 
    let params = AS.call_params_right_exp r_exp
        funproc_name = AS.id_name (AS.call_name_right_exp r_exp)
        env = AS.right_exp_env r_exp
        (s40, cur_blck10, tmp_addr) = gen_tac_FuncProcCall state cur_blck params funproc_name env
    in (s40, cur_blck10, tmp_addr)

-- This function is used to generate the TAC for a function or procedure call in both statements and right expressions
--gen_tac_FuncProcCall :: State -> String -> AS.RightExp -> String -> E.Env -> (State, String, Address)
gen_tac_FuncProcCall state cur_blck params funproc_name env =
    let types                       = map (\x -> to_primitive_type (AS.right_exp_type x)) params
        (s10, cur_blck10, addrs)    = gen_tac_of_list_RightExp state cur_blck params []
        s20                         = out_params s10 cur_blck10 addrs types
        (tmp_addr, s30)             = add_temp_var s20
        funproc_entry               = E.lookup env funproc_name
        funproc_call                = case funproc_entry of
                                        Just (E.FunEntry _ ret _ _ _ _ _) -> FunCall  { f_name = funproc_name, num_params = length params, l = tmp_addr, assign_type = to_primitive_type ret }
                                        Just (E.ProcEntry {})           -> ProcCall { p_name = funproc_name, num_params = length params }
        s40                         = out s30 cur_blck10 funproc_call
    in (s40, cur_blck10, tmp_addr)


gen_tac_of_RightExpLeftExp :: State -> String -> AS.RightExp -> (State, String, Address)
gen_tac_of_RightExpLeftExp state cur_blck r_exp = 
    let (s10, prim_type, left_addr, is_pnt) = gen_tac_of_LeftExp state cur_blck (AS.left_exp_right_exp r_exp) False
        (tmp_addr, s20) = add_temp_var s10
        s30             = out s20 cur_blck (ReadPointerValue { l1 = tmp_addr, l2 = left_addr })
        (s40, out_addr) = if not is_pnt then (s10, left_addr) else (s30, tmp_addr)
    in (s40, cur_blck, out_addr)

gen_tac_of_RightExpCoercion :: State -> String -> AS.RightExp -> (State, String, Address)
gen_tac_of_RightExpCoercion state cur_blck r_exp =
    let (s10, cur_blck1, address) = gen_tac_of_RightExp state cur_blck (AS.right_exp_coercion r_exp)
        (tmp_var, s20)            = add_temp_var s10
        from_type                 = to_primitive_type (AS.right_exp_from_type r_exp)
        to_type                   = to_primitive_type (AS.right_exp_type r_exp)
        un_op_coere               = (Coerce { type_from = from_type, type_to = to_type })      
        s30                       = out s20 cur_blck1 (UnaryAssignment { l = tmp_var, r = address, assign_type = to_type, un_op = un_op_coere })
        in (s30, cur_blck1, tmp_var)

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

gen_tac_of_RightExpNot :: State -> String -> AS.RightExp -> (State, String, Address)
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
gen_tac_of_binary_logical_operators :: State -> String -> AS.RightExp -> (State, String, Address)
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

gen_tac_of_ReadPrimitive :: State -> String -> AS.ReadPrimitive -> (State, String)
gen_tac_of_ReadPrimitive state cur_blck prim_read = 
    let l_exp            = (AS.read_exp prim_read)
        prim_read_name      = AS.make_label_ReadPrimitive prim_read
        (s10, prim_type, address, _)   = gen_tac_of_LeftExp state cur_blck l_exp False
        s20              = out s10 cur_blck (Parameter { param = address, param_type = prim_type })
        s30              = out s20 cur_blck (ProcCall { p_name = prim_read_name, num_params = 1 })
    in  (s30, cur_blck)

-----------------------------------------------------------------------------------------------------------------------------

-- ____________________________ AUX FUNCTIONS ________________________________________

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

get_declaration_position :: E.Env -> String -> (Int, Int)
get_declaration_position env id_name = 
    case (E.lookup env id_name) of
        Nothing -> (-1,-1) --internal error
        Just x  -> (E.pos_decl x)


----------------------------------------------------------------------------------------------------------------------------

-- ____________________________ PRETTY PRINTER ________________________________________

pretty_printer_tac :: State -> String
pretty_printer_tac (State t str _ _ _ _) = "# TAC START\n" ++ "# FUNCTIONS AND PROCEDURE\n" ++ (pretty_printer_tac_aux t str) ++ "#TAC END\n" where
    pretty_printer_tac_aux []     str = "\n# STRINGS\n" ++ pretty_printer_string str
    pretty_printer_tac_aux (x:xs) str = (pretty_printer_block x) ++ (pretty_printer_tac_aux xs str)

pretty_printer_block :: Block -> String
pretty_printer_block (Block cur_blck b_code) = 
    (if (isPrefixOf "block?" cur_blck) || (isPrefixOf "end" cur_blck) then "" else "\n") ++ cur_blck ++ ":\n" ++ (pretty_printer_block_aux b_code) where
        pretty_printer_block_aux []     = ""
        pretty_printer_block_aux (x:xs) = "   " ++ (show x) ++ "\n" ++ (pretty_printer_block_aux xs)
    
pretty_printer_string :: [ String ] -> String
pretty_printer_string str = (pretty_printer_string_aux str 0) where
    pretty_printer_string_aux []     _ = "\n"
    pretty_printer_string_aux (x:xs) i = (make_block_label $ StringBlockType i) ++ ":\n   " ++ (show x) ++ "\n" ++ (pretty_printer_string_aux xs (i+1))

instance Show PrimType where
    show TypeInt    = "int"
    show TypeReal   = "real"
    show TypeChar   = "char"
    show TypeBool   = "bool"
    show TypeAddr   = "addr"

instance Show Address where 
    show (AddressInt i)         = show i
    show (AddressReal r)        = show r
    show (AddressChar c)        = show c
    show (AddressBool b)        = if b then "true" else "false"
    show (AddressProgramVar v)  = v
    show (AddressTempVar t)     = t

instance Show Instruction where
    show (BinaryArithmAssignment l r1 r2 t op)    = (show l) ++ " =:" ++ (show t) ++ " " ++ (show r1) ++ " " ++ (show op) ++ " " ++ (show r2)  ++ " #BinaryArithmAssignment"
    show (BinaryRelatAssignment  l r1 r2 t op)    = (show l) ++ " =:" ++ (show t) ++ " " ++ (show r1) ++ " " ++ (show op) ++ " " ++ (show r2)  ++ " #BinaryRelatAssignment"
    show (UnaryAssignment l r t op)               = (show l) ++ " =:" ++ (show t) ++ " " ++ (show op) ++ " " ++ (show r)   ++ " #UnaryAssignment"
    show (NullAssignment l r t)                   = (show l) ++ " =:" ++ (show t) ++ " " ++ (show r)  ++ " #NullAssignment"
    show (Jump goto)                              = "goto " ++ goto
    show (JumpIfTrue goto cond)                   = "if " ++ (show cond) ++ " goto " ++ goto
    show (JumpIfFalse goto cond)                  = "ifFalse " ++ (show cond) ++ " goto " ++ goto
    show (JumpConditional goto r1 r2 op)          = "ifCond " ++ (show r1) ++ " " ++ (show op) ++ " " ++ (show r2) ++ " goto " ++ goto
    show (ReadFromArray array i l t)              = (show l) ++ " =:" ++ (show t) ++ " " ++ (show array) ++ "[" ++ (show i) ++ "]"
    show (WriteToArray array i r t)               = (show array) ++ "[" ++ (show i) ++ "]" ++ " =:" ++ (show t) ++ " " ++ (show r)
    show (ReadPointerAddress l pointer)           = (show l) ++ " = " ++ (show pointer) ++ "@" ++ " #ReadPointerAddress"
    show (ReadPointerValue l1 l2)                 = (show l1) ++ " = " ++ (show l2) ++ "^" ++ " #ReadPointerValue"
    show (WritePointerValue l r)                  = (show l) ++ "^ = " ++ (show r) ++ " #WritePointerValue"
    show (ProcCall ptname numtparams)             = "pcall " ++ ptname ++ ", " ++ (show numtparams)
    show (FunCall ftname numtparams l _)          = (show l) ++ " = fcall " ++ ftname ++ ", " ++ (show numtparams)
    show (Parameter param paramttype)             = "param " ++ (show param)
    show (Return)                                 = "return void"
    show (RetVal value returnttype)               = "return " ++ (show value)
    show (Comment comment)                        = "# " ++ comment

instance Show BinaryArithmOp where
    show (Sum       t) = "+:"  ++ (show t)
    show (Subtract  t) = "-:"  ++ (show t)
    show (Multiply  t) = "*:"  ++ (show t)
    show (Divide    t) = "/:"  ++ (show t)
    show (Remainder t) = "%:"  ++ (show t)
    show (Power     t) = "**:" ++ (show t)

instance Show BinaryRelatOp where
    show (GreaterThan  t) = ">:"  ++ (show t)
    show (GreaterEqual t) = ">=:" ++ (show t)
    show (LessThan     t) = "<:"  ++ (show t)
    show (LessEqual    t) = "<=:" ++ (show t)
    show (Equal        t) = "==:" ++ (show t)
    show (NotEqual     t) = "<>:" ++ (show t)

instance Show UnaryOp where
    show (Coerce from to) = "coerce_" ++ (show from) ++ "_to_" ++ (show to)
    show (Negate t) = "-:" ++ (show t)
    show (Not) = "not"
