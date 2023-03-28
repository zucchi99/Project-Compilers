-- Implementazione del typesystem
-- Nota: Teoria principale --> lezione-LC-2022-04-26.pdf

module Types where

-- Custom Types definition
data Type = 
    | TBoolean
    | TInteger
    | TReal
    | TChar
    | TString
    -- Since array sizes are always declared beforehand, we can avoid
    -- to calculate it and just store it
    | TArray {aType: Type, aSize: Int}
    -- Pointer of Pointers seems to be considered as a user type ("Type" keyword)
    -- For this reason, we only allow BaseTypes
    | TPointer {pType: Type }
    -- "It is useful to add a type for errors that is 'bigger' than all other"
    | TError 

-- Definition of "mathType" and "sup"
-- "mathType" is used to make inference of unary expressions 
mathtype :: Type -> Type
mathtype TInteger = TInteger
mathType TReal = TReal
-- In Pascal, char, string and boolean are not allowed in unary math expressions
-- f.e. -'a' -true are not allowed
mathType _ = TError

-- "sup" is used to make inference of binary expressions
-- TODO:
-- 1. Posso sommare due string/char, ma non posso moltiplcarle. Come fare?

sup :: Type -> Type -> Type
sup type1 type1 = type1 -- If types are equal, the return type is the same
sup TInteger TReal = TReal
sup TReal TInteger = TReal
-- In Pascal, char, integer and boolean are not allowed in binary math expressions
-- f.e. 1 + true are not allowed
sup _ _ = TError


