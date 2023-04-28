module Types where

import qualified ErrorMessage as Err

data Type = 
    BooleanType
    | IntegerType
    | RealType
    | CharType
    | StringType
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
    ArrayType{aType=t1, dimensions=d1} == ArrayType{aType=t2, dimensions=d2} = (t1 == t2) && (d1 == d2) --same type and length
    _                     == _                     = False

instance Show Type where
    show BooleanType           = "boolean"
    show IntegerType           = "integer"
    show RealType              = "real"
    show CharType              = "char"
    show StringType            = "string"
    show ArrayType{aType=t, dimensions=d} = "array " ++ (show d) ++ " of " ++ (show t)
    show PointerType{pType=t}  = "^" ++ (show t)
    show ErrorType             = "error"
    show TBDType               = "TBD"

-- given two types, returns the superior type ( IntegerType < RealType )
sup :: Type -> Type -> Type
sup IntegerType RealType = RealType
sup RealType IntegerType = RealType
sup t1 t2 = case t1 == t2 of
    True  -> t1
    False -> ErrorType


get_multi_array_length (ArrayType t d) = (get_single_array_length d) ++ (get_multi_array_length t)
get_multi_array_length (PointerType t) = get_multi_array_length t
get_multi_array_length _               = []

get_single_array_length []         = []
get_single_array_length ((l,r):xs) = (r-l+1) : (get_single_array_length xs)


-- given two types, return True if them are comparable ( <, >, <=, >= )
rel :: Type -> Type -> Type
rel t1 t2 = case sup t1 t2 of
    ErrorType       -> ErrorType
    BooleanType     -> ErrorType
    ArrayType _ _   -> ErrorType
    PointerType _   -> ErrorType
    _               -> BooleanType  -- if not error, then boolean

-- given two types, return True if them are equal ( ==, <> )
comparable :: Type -> Type -> Bool
comparable t1 t2 = case sup t1 t2 of
    ArrayType _ _   -> False
    _               -> True  -- if not error, then boolean

-- given two types, return the type of the result of a math operation ( +, -, *, / )
math_op :: Type -> Type -> Type
math_op t1 t2 = case sup t1 t2 of
    RealType    -> RealType
    IntegerType -> IntegerType
    _           -> ErrorType    -- ErrorType -> if not the two types can not be used in math operations

-- ccompare all the types in a list and return true if they are all the same type
all_same_type :: [Type] -> Bool
all_same_type [] = False
all_same_type (x:xs) = all (==x) xs

-- given two types, return True if the one on the right can be coerced to the one on the left
need_coerc :: Type -> Type -> Bool
need_coerc RealType IntegerType = True
need_coerc _ _ = False

is_array :: Type -> Bool
is_array (ArrayType{}) = True
is_array _             = False
