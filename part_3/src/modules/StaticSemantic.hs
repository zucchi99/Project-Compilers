
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


-- mkArrTy(E1.type, E2.type)
mkArrTy :: T.Type -> T.Type -> [String]
mkArrTy (T.ArrayType t _) idx = case T.sup idx T.IntegerType of
    T.IntegerType -> []
    _             -> getErrorsFromMaybe $ T.combineTypeErrors (T.ErrorType [ Err.errMsgUnexpectedType "The index of an array" T.IntegerType idx ]) idx
mkArrTy t _ = [ Err.errMsgTypeNotArray t ]


-- mkAssignErrs(E1.type, E2.type)
mkAssignErrs :: T.Type -> T.Type -> [String]
mkAssignErrs lhs rhs = 
    case (T.combineTypeErrors lhs rhs) of       -- lhs or rhs are errors?
        Just (T.ErrorType m) -> m               -- yes ==> return errors
        Nothing -> case T.sup lhs rhs == lhs of -- no  ==> rhs is compatible with lhs?
            True  -> []             -- yes ==> no errors
            False -> [ Err.errMsgAssign lhs rhs ] -- no ==> return assign error


-- mkIfErrs(E.type, S1.errs)
mkIfErrs :: T.Type -> [String]
mkIfErrs guard = case guard of
    (T.ErrorType m) -> m
    (T.BooleanType) -> []
    t               -> [ (Err.errMsgUnexpectedType "Guard" T.BooleanType t) ]


-- mkIdDeclErrs(id, E.type, T.type)
mkIdDeclErrs :: String -> T.Type -> T.Type -> [String]
mkIdDeclErrs id etype ttype = case T.sup etype ttype of
    (T.ErrorType m) -> getErrorsFromMaybe $ T.combineTypeErrors (T.ErrorType [ Err.errMsgUnexpectedType ("The variable named '" ++ id ++ "'") etype ttype ]) ttype
    _               -> []

-- mkFunEnv(id, F.types, T.type)
-- mkFunEnv(id, τ1 × . . . × τn, τ ) = {id : τ1 × . . . × τn → τ}
-- Nota: La stringa è il nome della funzione, la lista di tipi sono i tipi dei parametri e il tipo finale è il tipo di ritorno
--       Il tutto crea un enviroment con una sola entry (che equivale ad una funzione)
mkFunEnv :: String -> [T.Type] -> T.Type -> E.Env
mkFunEnv id types returnType = E.mkSingletonEnv id (T.FunType types returnType)

main = do

    let arrayT = T.ArrayType T.RealType [(1, 3)]

    putStrLn "mkArrTy T.ArrayType T.IntegerType"    
    putStrLn $ show $ mkArrTy arrayT T.IntegerType
    putStrLn ""
    
    putStrLn "mkArrTy T.ArrayType T.RealType"    
    putStrLn $ show $ mkArrTy arrayT T.RealType
    putStrLn ""
    
    putStrLn "mkArrTy T.RealType T.RealType"    
    putStrLn $ show $ mkArrTy T.RealType T.RealType
    putStrLn ""

    putStrLn "mkAssignErrs T.RealType T.IntegerType"    
    putStrLn $ show $ mkAssignErrs T.RealType T.IntegerType
    putStrLn ""
    
    putStrLn "mkAssignErrs T.IntegerType T.RealType"    
    putStrLn $ show $ mkAssignErrs T.IntegerType T.RealType
    putStrLn ""

    putStrLn "mkIfErrs T.BooleanType"
    putStrLn $ show $ mkIfErrs T.BooleanType
    putStrLn ""

    putStrLn "mkIfErrs T.IntegerType"
    putStrLn $ show $ mkIfErrs T.IntegerType
    putStrLn ""

    putStrLn "mkIdDeclErrs \"x\" T.RealType T.IntegerType"
    putStrLn $ show $ mkIdDeclErrs "x" T.RealType T.IntegerType
    putStrLn ""

    putStrLn "mkIdDeclErrs \"x\" T.IntegerType T.RealType"
    putStrLn $ show $ mkIdDeclErrs "x" T.IntegerType T.RealType
    putStrLn ""

    putStrLn "mkIdDeclErrs \"x\" T.IntegerType T.StringType"
    putStrLn $ show $ mkIdDeclErrs "x" T.IntegerType T.StringType
    putStrLn ""

    putStrLn "mkIdDeclErrs \"x\" T.StringType T.IntegerType"
    putStrLn $ show $ mkIdDeclErrs "x" T.StringType T.IntegerType
    putStrLn ""
