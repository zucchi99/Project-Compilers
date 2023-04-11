
-- ghci -outputdir src/bin src/modules/TAC.hs src/modules/Types.hs src/modules/ErrorMessage.hs

module TAC where

--import qualified AbstractSyntax as AS
import qualified Types as T

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

-- variable defined in source code
newtype AddrProgramVar = AddrProgramVar String
    deriving (Show)

-- temporary variable defined by compiler
newtype AddrTempVar = AddrTempVar String
    deriving (Show)

-- primitive types of TAC
data Address = 
    AddressInt Int
    -- | AddressReal Real
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

-- ___________ SET OF INSTRUCTIONS ___________

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

-- ___________ SET OF OPERATIONS ___________

data UnaryOp =
    Coerce          { type_from, type_to :: PrimType }
    | Negate        { negation_type      :: PrimType }
    | Not 
    deriving (Show)

data BinaryOp = 
    Sum             { binary_type :: PrimType }
    | Subtract      { binary_type :: PrimType }
    | Multiply      { binary_type :: PrimType }
    | Divide        { binary_type :: PrimType }
    | Remainder     { binary_type :: PrimType }
    | Power         { binary_type :: PrimType }
    | AddressSum
    deriving (Show)

data RelationalOp = 
    GreaterThan     { relation_type :: PrimType }
    | GreaterEqual  { relation_type :: PrimType }
    | LessThan      { relation_type :: PrimType }
    | LessEqual     { relation_type :: PrimType }
    | Equal         { relation_type :: PrimType }
    | NotEqual      { relation_type :: PrimType }
    deriving (Show)

-- ___________ TAC TYPE ___________

data Block = Block {
    b_name :: String,
    code :: [ Instruction ]
} deriving (Show)

data TAC = TAC { tac :: [ Block ] } 
    deriving (Show)

-- ___________ FUNCTIONS ___________

to_primitive_type T.BooleanType = TypeBool
to_primitive_type T.IntegerType = TypeInt
to_primitive_type T.RealType    = TypeReal
to_primitive_type T.CharType    = TypeChar
to_primitive_type _             = TypeAddr

-- reverse list of blocks and reverse each block list of instructions
reverse_TAC :: TAC -> TAC
reverse_TAC (TAC t) = (TAC $ reverse $ map (\ (Block n c) -> (Block n (reverse c))) t)

lookup :: TAC -> String -> Maybe Block
lookup (TAC (x:xs)) b_name = 
    let (Block cur_name _) = x in
        if cur_name == b_name
        then Just x
        else TAC.lookup (TAC xs) b_name
lookup (TAC []) _ = Nothing

add_instruction_to_block :: TAC -> String -> Instruction -> (Bool, Block)
add_instruction_to_block t b_name instr = case (TAC.lookup t b_name) of
    Nothing          -> (True,  (Block b_name [ instr ])) -- create new block ==> is new block? true
    Just (Block n c) -> (False, (Block n (instr:c)))      -- add goto block     ==> is new block? false

out :: TAC -> String -> Instruction -> TAC
out t b_name instr = 
    case (add_instruction_to_block t b_name instr) of
        (True,  b) -> add_block b t
        (False, b) -> substitute_block t b b_name

add_block :: Block -> TAC -> TAC
add_block b (TAC t) = (TAC $ b:t)

substitute_block :: TAC -> Block -> String -> TAC
substitute_block t b b_name = (TAC $ substitute_block_aux t b b_name)
    where
        substitute_block_aux (TAC (x:xs)) b b_name = 
            let (Block cur_name _) = x in
                if cur_name == b_name
                then b : xs -- block found ==> add new block and rest of tac
                else x : (substitute_block_aux (TAC xs) b b_name) --block not found ==> keep searching
        substitute_block_aux _ _ b_name = [ (Block ("Internal error: block " ++ b_name ++ " not found") []) ]

main :: IO ()
main = do
    let b0_name = "ciao"
    let b0 = (Block b0_name [])
    let t0 = (TAC [b0])
    putStrLn $ show $ TAC.lookup t0 b0_name
    putStrLn $ show $ TAC.lookup t0 "rand"

    let b1_name = "bella"
    let b1 = (Block b1_name [])
    let t1 = add_block b1 t0
    putStrLn $ show $ t1

    let t2 = out t1 b0_name (Jump "cp")
    let t3 = out t2 b0_name (Jump "rm")
    let t4 = out t3 b1_name (Jump "x")
    let t5 = out t4 b1_name (Jump "y")
    putStrLn $ show $ t5

    let tn = reverse_TAC t5
    putStrLn $ show $ tn