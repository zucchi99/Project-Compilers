-- Implementazione del typesystem
-- Nota: Teoria principale --> lezione-LC-2022-04-26.pdf

-- compile with
-- ghci TypeSys.hs -o TypeSys.o Errors.hs

module Types where

import qualified ErrorMessage as Err

-- any type definition
data Type = 
    BooleanType
    | IntegerType
    | RealType
    | CharType
    | StringType
    -- TODO handle array multidimensional
    | ArrayType { aType :: Type, dimensions :: [(Int,Int)] }
    | PointerType { pType :: Type }
    | ErrorType
    | TBDType -- to be defined (assigned as dummy type to any obj in the abs generated from the parser)

instance Eq Type where -- Eq for arrays does not care about checked/notchecked-ness
    ErrorType             == ErrorType             = True
    BooleanType           == BooleanType           = True
    IntegerType           == IntegerType           = True
    RealType              == RealType              = True
    CharType              == CharType              = True
    StringType            == StringType            = True
    TBDType               == TBDType               = True
    PointerType{pType=p1} == PointerType{pType=p2} = (p1 == p2) --same pointed address
    -- ArrayType{aType=t1, leftPoint=l1, rightPoint=r1} == ArrayType{aType=t2, leftPoint=l2, rightPoint=r2} = (t1 == t2) && (r2-l2 == r1-l1) --same type and length
    ArrayType{aType=t1, dimensions=d1} == ArrayType{aType=t2, dimensions=d2} = (t1 == t2) && (d1 == d2) --same type and length
    _                     == _                     = False

instance Show Type where
    show BooleanType           = "boolean"
    show IntegerType           = "integer"
    show RealType              = "real"
    show CharType              = "char"
    show StringType            = "string"
    -- show ArrayType{aType=t, leftPoint=l, rightPoint=r} = "array [" ++ (show l) ++ ".." ++ (show r) ++ "] of " ++ (show t)
    show ArrayType{aType=t, dimensions=d} = "array " ++ (show d) ++ " of " ++ (show t)
    show PointerType{pType=t}  = "^" ++ (show t)
    show ErrorType             = "error"
    show TBDType               = "TBD"

listToString :: Foldable t => t [Char] -> [Char] -> [Char]
listToString xs sep = foldr (\a b -> a ++ if b == "" then b else sep ++ b) "" xs

-- Definition of "mathType" and "sup"
-- "mathType" is used to make inference of unary expressions
-- In Pascal, char, string and boolean are not allowed in unary math expressions
-- f.e. -'a' -true are not allowed 
mathType :: Type -> Type
mathType IntegerType = IntegerType
mathType RealType    = RealType
mathType t           = ErrorType -- { messages = [Err.errMsgNotMathType t] }

-- "sup" is used to make inference of binary expressions
-- TODO:
-- 1. Posso sommare due string/char, ma non posso moltiplcarle. Come fare?

-- In Pascal, char, integer and boolean are not allowed in binary math expressions
-- f.e. 1 + true are not allowed
sup :: Type -> Type -> Type
sup IntegerType RealType = RealType
sup RealType IntegerType = RealType
sup t1 t2 = case t1 == t2 of
    True  -> t1 -- ok
    False -> ErrorType -- { messages = [Err.errMsgNotCompatible t1 t2] } -- error


areErrors t1 t2 = (t1 == ErrorType || t2 == ErrorType)


-- Function that handles the comparison of two types
rel :: Type -> Type -> Type
rel t1 t2 = case sup t1 t2 of
    ErrorType -> ErrorType -- { messages = (("Relation operation not permitted"):m) } -- error
    _         -> BooleanType -- ok

-- Helper function to compare all the types in a list and return true if they are all the same type
all_same_type :: [Type] -> Bool
all_same_type [] = False
all_same_type (x:xs) = all (==x) xs

{-
----------------- test
main :: IO ()
main = do
    putStrLn "Test TypeSys.hs"

    putStrLn "bool var"
    let bool = BooleanType
    putStrLn $ show bool

    putStrLn "int var"
    let int = IntegerType
    putStrLn $ show int

    putStrLn "real var"
    let real = RealType
    putStrLn $ show real

    let sup_ok = sup int real
    putStrLn "superior ok: int and real = real"
    putStrLn $ show sup_ok
    
    let sup_err1 = sup int bool
    putStrLn "superior err 1: int and bool = error"
    putStrLn $ show sup_err1

    let sup_err2 = sup real bool
    putStrLn "superior err 2: real and bool = error"
    putStrLn $ show sup_err2

    putStrLn "cmb err sup_ok sup_err1"
    putStrLn $ show $ combineTypeErrors sup_ok sup_err1
    
    putStrLn "cmb err sup_ok sup_err2"
    putStrLn $ show $ combineTypeErrors sup_ok sup_err2
    
    putStrLn "cmb err sup_err1 sup_err2"
    putStrLn $ show $ combineTypeErrors sup_err1 sup_err2


-}