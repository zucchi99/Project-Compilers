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

-- given two types, returns the superior type ( CharType < IntegerType < RealType )
sup :: Type -> Type -> Type
sup IntegerType RealType = RealType
sup RealType IntegerType = RealType
sup CharType RealType = RealType
sup RealType CharType = RealType
sup CharType IntegerType = IntegerType
sup IntegerType CharType = IntegerType
sup t1 t2 = case t1 == t2 of
    True  -> t1
    False -> ErrorType

get_multi_array_length (ArrayType t d) = (get_single_array_length d) ++ (get_multi_array_length t)
get_multi_array_length (PointerType t) = get_multi_array_length t
get_multi_array_length _               = []

get_single_array_length []         = []
get_single_array_length ((l,r):xs) = (r-l+1) : (get_single_array_length xs)


-- given two types, return True if them are comparable ( <, >, <=, >= )
comparison :: Type -> Type -> Type
comparison t1 t2 = case sup t1 t2 of
    RealType    -> RealType
    IntegerType -> IntegerType
    CharType    -> CharType
    _           -> ErrorType -- ErrorType -> if not possible to do a comparison operation

-- given two types, return True if them are equal ( ==, <> )
equality :: Type -> Type -> Type
equality t1 t2 = case sup t1 t2 of
    RealType    -> RealType
    IntegerType -> IntegerType
    CharType    -> CharType
    BooleanType -> BooleanType
    _           -> ErrorType -- ErrorType -> if not possible to do an equality operation

-- given two types, return the type of the result of an arithmetic operation ( +, -, *, / )
arithmetic :: Type -> Type -> Type
arithmetic t1 t2 = case sup t1 t2 of
    RealType    -> RealType
    IntegerType -> IntegerType
    CharType    -> CharType
    _           -> ErrorType -- ErrorType -> if not possible to do a comparison operation

-- ccompare all the types in a list and return true if they are all the same type
all_same_type :: [Type] -> Bool
all_same_type [] = False
all_same_type (x:xs) = all (==x) xs

-- given two types, return True if the one on the right can be coerced to the one on the left
need_coerc :: Type -> Type -> Bool
need_coerc RealType IntegerType = True
need_coerc RealType CharType = True
need_coerc IntegerType CharType = True
need_coerc _ _ = False

is_array :: Type -> Bool
is_array (ArrayType{}) = True
is_array _             = False
