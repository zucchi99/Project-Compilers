
-- compile with
-- ghci StaticSemantic.hs -o StaticSemantic.o Env.hs

module StaticSemantic where

import qualified Env as E
import qualified Types as T
import qualified Data.Map as Map
import qualified ErrorMessage as Err
import Data.Maybe

getErrorsFromMaybe :: Maybe T.Type -> [String]
getErrorsFromMaybe (Just (T.ErrorType m)) = m
getErrorsFromMaybe maybe                  = []

checkArrayType :: T.Type -> T.Type -> [String]
checkArrayType (T.ArrayType t _ _) idx = case T.sup idx T.IntegerType of
    T.IntegerType -> []
    _             -> getErrorsFromMaybe $ T.combineTypeErrors (T.ErrorType [ Err.errMsgUnexpectedType "The index of an array" T.IntegerType idx ]) idx
checkArrayType t _ = [ Err.errMsgTypeNotArray t ]

checkErrorOnAssignment :: T.Type -> T.Type -> [String]
checkErrorOnAssignment lhs rhs = 
    case (T.combineTypeErrors lhs rhs) of       -- lhs or rhs are errors?
        Just (T.ErrorType m) -> m               -- yes ==> return errors
        Nothing -> case T.sup lhs rhs == lhs of -- no  ==> rhs is compatible with lhs?
            True  -> []             -- yes ==> no errors
            False -> [ Err.errMsgAssign lhs rhs ] -- no ==> return assign error

checkErrorOnGuard :: T.Type -> [String]
checkErrorOnGuard guard = case guard of
    (T.ErrorType m) -> m
    (T.BooleanType) -> []
    t               -> [ (Err.errMsgUnexpectedType "Guard" T.BooleanType t) ]



main = do

    let arrayT = T.ArrayType T.RealType 1 3

    putStrLn "checkArrayType T.ArrayType T.IntegerType"    
    putStrLn $ show $ checkArrayType arrayT T.IntegerType
    putStrLn ""
    
    putStrLn "checkArrayType T.ArrayType T.RealType"    
    putStrLn $ show $ checkArrayType arrayT T.RealType
    putStrLn ""
    
    putStrLn "checkArrayType T.RealType T.RealType"    
    putStrLn $ show $ checkArrayType T.RealType T.RealType
    putStrLn ""

    putStrLn "checkErrorOnAssignment T.RealType T.IntegerType"    
    putStrLn $ show $ checkErrorOnAssignment T.RealType T.IntegerType
    putStrLn ""
    
    putStrLn "checkErrorOnAssignment T.IntegerType T.RealType"    
    putStrLn $ show $ checkErrorOnAssignment T.IntegerType T.RealType
    putStrLn ""


    