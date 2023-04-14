
-- compile with
-- ghci Env.hs -o Env.o TypeSys.hs Errors.hs

module Env where

import qualified Types as T
import qualified Data.Map as Map
import qualified ErrorMessage as Err
import Data.Maybe

data Env = Env { env :: Map.Map String EnvEntry} deriving (Show)

data EnvEntry 
    = VarEntry { ty :: T.Type } 
    | ConstEntry { ty :: T.Type }
    | FunEntry { params :: [(String, T.Type)], ret :: T.Type, forward :: Bool }
    | ProcEntry { params :: [(String, T.Type)], forward :: Bool  }
    deriving (Show)

instance Eq EnvEntry where
    VarEntry { ty = t1 }                == VarEntry { ty = t2 }                 = t1 == t2
    ConstEntry { ty = t1 }              == ConstEntry { ty = t2 }               = t1 == t2
    FunEntry { params = p1, ret = r1 }  == FunEntry { params = p2, ret = r2 }   = p1 == p2 && r1 == r2
    ProcEntry { params = p1 }           == ProcEntry { params = p2 }            = p1 == p2
    _                                   == _                                    = False

-- | Create a new empty environment. It should be used as the root environment
emptyEnv :: Env
emptyEnv = Env { env = Map.empty }

-- | Create a new environment with only one binding
mkSingletonEnv :: String -> EnvEntry -> Env
mkSingletonEnv id ty = Env { env = Map.singleton id ty }


-- | Add a new variable to the environment
-- | If the variable is already present, it is overwritten
addVar :: Env -> String -> EnvEntry -> Env
addVar (Env e) id ty = Env { env = Map.insert id ty e }

-- | The function "lookupEnv" returns the value associated with a key in the environment
-- | If the key is not present, it recursively searches in the parent environment
lookup :: Env -> String -> Maybe EnvEntry
lookup e name = case Map.lookup name (env e) of
    Just t  -> Just t
    Nothing -> Nothing

-- | The function "merge" merges two environments
-- | If the two environments have a common key, the function keeps the value of the first environment
merge :: Env -> Env -> Env
merge (Env e1) (Env e2) = Env { env = Map.union e1 e2 }

-- The function gets the clashes in bindings between two environments
-- It returns a list of strings, each string is an error message
getClashes :: Env -> Env -> (Int, Int) -> [String]
getClashes (Env e1) (Env e2) pos = Map.foldrWithKey (\k v acc -> if Map.member k e2 then (Err.errMsgClash k pos) : acc else acc) [] e1

-- Function that checks if there are FunEntry or ProcEntry with forward = True and if there are returns their key
getForward :: Env -> [String]
getForward (Env e) = Map.foldrWithKey (\k v acc -> if (isForward v) then k : acc else acc) [] e where
    isForward (FunEntry { forward = True }) = True
    isForward (ProcEntry { forward = True }) = True
    isForward _ = False

mainEnv = do
    putStrLn "Test - Env.hs"

    putStrLn "Creazione env (empty)"
    let env1 = emptyEnv

    putStrLn "Creazione env (singleton)"
    let env2 = mkSingletonEnv "x" VarEntry { ty = T.RealType }
        env3 = addVar env2 "y"  VarEntry { ty = T.IntegerType }
        env4 = addVar env3 "z"  VarEntry { ty = T.BooleanType }
    putStrLn $ show env4

    putStrLn "Creazione env (singleton)"
    let env5 = mkSingletonEnv "a"  VarEntry { ty = T.RealType }
        env6 = addVar env5 "x"  VarEntry { ty = T.IntegerType }
    putStrLn $ show env6

    putStrLn "Merge envs"
    let env7 = merge env1 env4
    putStrLn $ show env7

    putStrLn "Merge envs"
    let env8 = merge env6 env7 
    putStrLn $ show env8

