
-- compile with
-- ghci StaticSemantic.hs -o StaticSemantic.o Env.hs

module StaticSemantic where

import qualified Env as E
import qualified Types as T
import qualified Data.Map as Map
import qualified ErrorMessage as Err
import AbstractSyntax
import Data.Maybe
import Data.List
import ErrM

-- __________________________ AUXILIAR CLASSES AND FUNCTIONS

{-
getErrorsFromMaybe :: Maybe T.Type -> [String]
getErrorsFromMaybe (Just (T.ErrorType m)) = m
getErrorsFromMaybe maybe                  = []
-}

checkPresenceStmt id env decl pos = 
    case E.lookup env id of
        Just _  -> []
        Nothing -> [ Err.errMsgNotDeclared id pos ]


checkPresenceDecl id env decl pos = 
    case E.lookup env id of
        Just _  -> [ Err.errMsgAlreadyDeclared id pos]
        Nothing -> []

-- mkArrTy(E1.type, E2.type)
-- Controllo di indice dell'array (deve essere int)
mkArrTy :: T.Type -> T.Type -> (Int, Int) -> [String]
mkArrTy (T.ArrayType t _) idx pos = 
    case T.sup idx T.IntegerType of
        T.IntegerType -> []
        _             -> [ Err.errMsgUnexpectedType "The index of an array" T.IntegerType idx pos ]
mkArrTy t _ pos = [ Err.errMsgTypeNotArray t pos ]


-- mkAssignErrs(E1.type, E2.type)
-- Controllo durante l'assegnamento
mkAssignErrs :: T.Type -> T.Type -> (Int, Int) -> [String]
mkAssignErrs lhs rhs pos =  
    case (T.areErrors lhs rhs) of                    -- lhs or rhs are errors?
        True  -> []                                -- yes ==> error already found before, do not add new errors
        False -> case (T.sup lhs rhs) == lhs of    -- no  ==> check if rhs is compatible with lhs
            True  -> []                               -- yes ==> no errors
            False -> [ Err.errMsgAssign lhs rhs pos ] -- no ==> return assign error for incompatible types


-- mkIfErrs(E.type, S1.errs)
-- Controllo della guardia dell'if
mkIfErrs :: T.Type -> (Int, Int) -> [String]
mkIfErrs guard pos = 
    case guard of
        (T.ErrorType)   -> [] -- error already found before, do not add new errors
        (T.BooleanType) -> []
        t               -> [ (Err.errMsgUnexpectedType "Guard" T.BooleanType t pos) ]

-- mkIdDeclErrs(id, E.type, T.type)
-- Controllo durante inizializzazione
mkIdDeclErrs :: String -> T.Type -> T.Type -> (Int, Int) -> [String]
mkIdDeclErrs id etype ttype pos = 
    case (T.areErrors etype ttype) of             -- etype or ttype are errors?
        True  -> []                             -- error already found before, do not add new errors
        False -> case (T.sup etype ttype) == T.ErrorType of    -- no  ==> check if is compatible
            True  -> [ Err.errMsgUnexpectedType ("The variable named '" ++ id ++ "'") etype ttype pos ]
            False -> []

-- mkFunEnv(id, F.types, T.type)
-- mkFunEnv(id, τ1 × . . . × τn, τ ) = {id : τ1 × . . . × τn → τ}
-- Nota: La stringa è il nome della funzione, la lista di tipi sono i tipi dei parametri e il tipo finale è il tipo di ritorno
--       Il tutto crea un enviroment con una sola entry (che equivale ad una funzione)
mkFunEnv :: String -> [T.Type] -> T.Type -> E.Env
mkFunEnv id types returnType = E.mkSingletonEnv id E.FunEntry{E.params=types, E.ret=returnType}

-- D.errs = mkFunErrs(D1.errs, S.errs, F.loc, D1.loc)
-- D è la dichiarazione della funzione

mkFunErrs :: [String] -> [String] -> E.Env -> E.Env -> (Int, Int) -> [String]
mkFunErrs d1errs serrs fenv d1env pos = d1errs ++ serrs ++ E.getClashes fenv d1env pos 

-- Controllo del tipo di ritorno di una funzione
mkRet :: T.Type -> E.Env -> (Int, Int) -> [String]
mkRet t env pos
    | compatible t (E.lookup env "result") = []
    | otherwise                            = [ Err.errMsgReturnNotCompatible t pos ]

-- Compatibilità del tipo di ritorno con il tipo di ritorno dichiarato
compatible :: T.Type -> Maybe E.EnvEntry -> Bool
compatible t1 (Just (E.VarEntry t2))   = T.sup t1 t2 == t2
compatible t1 (Just (E.ConstEntry t2)) = T.sup t1 t2 == t2
compatible _  _                        = False


-- keyword can be "break" or "continue"
{- checkLoop :: String -> E.Env -> [String]
checkLoop keyword env
    | E.lookup env keyword == Nothing = []
    | otherwise = [ Err.errMsgWrongLoopControl keyword] -}

{-

-- __________________________ STATIC SEMANTIC ANAL-ISYS
staticsemanticcheck x = case x of
    -- parse successful
    (ErrM.Ok a)    -> intercalate "\n\n" $ snd $ staticsemanticAux E.emptyEnv a
    -- parse error
    (ErrM.Bad err) -> err


-- __________________________ STATIC SEMANTIC CLASSES
class StaticSemanticClass a where
    staticsemanticAux :: E.Env -> a -> (E.Env, [String])

instance StaticSemanticClass Program where
    staticsemanticAux env (ProgramStart name block pos) = staticsemanticAux env block

instance StaticSemanticClass Ident where
    staticsemanticAux env (Ident _ _) = (env, [])


--instance StaticSemanticClass [Ident] where
  --  staticsemanticAux env idents = (env, [])


instance StaticSemanticClass BlockWithDecl where
    staticsemanticAux env (BlockWithDeclStart decls block pos) = 
        let (env1, errs1) = staticsemanticAux E.emptyEnv decls
            (env2, errs2) = staticsemanticAux (E.merge env1 env) block
        in (env2, errs1 ++ errs2)

instance StaticSemanticClass Declaration where
    staticsemanticAux env (DeclarationCostant id type_maybe value pos) = 
        let err_presence = E.lookupEnv id env
            err_type = case type_maybe of
                Nothing -> []
                Just t  -> mkIdDeclErrs id (T.getType value) t
        in (E.insert env2 id (E.ConstEntry (T.getType value)), errs1 ++ errs2)


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

    putStrLn "mkFunEnv \"f\" [T.IntegerType, T.RealType] T.StringType"
    putStrLn $ show $ mkFunEnv "f" [T.IntegerType, T.RealType] T.StringType
    putStrLn ""

    putStrLn "mkFunErrs [] [] (mkFunEnv \"f\" [T.IntegerType, T.RealType] T.StringType) (mkFunEnv \"f\" [T.IntegerType, T.RealType] T.StringType)"
    putStrLn $ show $ mkFunErrs [] [] (mkFunEnv "f" [T.IntegerType, T.RealType] T.StringType) (mkFunEnv "f" [T.IntegerType, T.RealType] T.StringType)
    putStrLn ""

-}