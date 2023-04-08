
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
    | FunEntry { params :: [T.Type], ret :: T.Type }
    | ProcEntry { params :: [T.Type] }
    deriving (Show)

-- | Create a new empty environment. It should be used as the root environment
emptyEnv :: Env
emptyEnv = Env { env = Map.empty }

-- | Create a new environment with only one binding
mkSingletonEnv :: String -> EnvEntry -> Env
mkSingletonEnv id ty = Env { env = Map.singleton id ty }


-- | Add a new variable to the environment
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
getClashes :: Env -> Env -> [String]
getClashes (Env e1) (Env e2) = Map.foldrWithKey (\k v acc -> if Map.member k e2 then (Err.errMsgClash k) : acc else acc) [] e1

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

