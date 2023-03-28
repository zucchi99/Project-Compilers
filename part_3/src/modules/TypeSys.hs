-- Implementazione del typesystem
-- Nota: Teoria principale --> lezione-LC-2022-04-26.pdf

module Types where

-- any type definition
data Type = 
    BooleanType
    | IntegerType
    | RealType
    | CharType
    | StringType
    | ArrayType { aType :: Type, leftPoint :: Int, rightPoint :: Int }
    | PointerType { pType :: Type }
    | ErrorType { messages :: [String] }

instance Eq Type where -- Eq for arrays does not care about checked/notchecked-ness
    ErrorType{}           == ErrorType{}           = True
    BooleanType           == BooleanType           = True
    IntegerType           == IntegerType           = True
    RealType              == RealType              = True
    CharType              == CharType              = True
    StringType            == StringType            = True
    PointerType{pType=p1} == PointerType{pType=p2} = (p1 == p2) --same pointed address
    ArrayType{aType=t1, leftPoint=l1, rightPoint=r1} == ArrayType{aType=t2, leftPoint=l2, rightPoint=r2} = (t1 == t2) && (r2-l2 == r1-l1) --same type and length
    _                     == _                     = False

instance Show Type where
    show BooleanType           = "boolean"
    show IntegerType           = "integer"
    show RealType              = "real"
    show CharType              = "char"
    show StringType            = "string"
    show ArrayType{aType=t, leftPoint=l, rightPoint=r} = "array [" ++ (show l) ++ ".." ++ (show r) ++ "] of " ++ (show t)
    show PointerType{pType=t} = "^" ++ (show t)
    show ErrorType{messages=m} = listToString m ", "

listToString :: Foldable t => t [Char] -> [Char] -> [Char]
listToString xs sep = foldr (\a b -> a ++ if b == "" then b else sep ++ b) "" xs

-- Definition of "mathType" and "sup"
-- "mathType" is used to make inference of unary expressions
-- In Pascal, char, string and boolean are not allowed in unary math expressions
-- f.e. -'a' -true are not allowed 
mathType :: Type -> Type
mathType IntegerType = IntegerType
mathType RealType    = RealType
mathType _           = ErrorType { messages = ["Not a math type"] }

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
    False -> ErrorType { messages = [(show t1) ++ "is not compatible with type" ++ (show t2)] } -- error
