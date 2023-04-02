
-- compile with
-- ghci Env.hs -o Env.o TypeSys.hs Errors.hs

module Env where

import qualified Types as T
import qualified Data.Map as Map
import qualified ErrorMessage as Err
import Data.Maybe

data Env = Env { env :: Map.Map String T.Type} deriving (Show)

-- | Create a new empty environment. It should be used as the root environment
emptyEnv :: Env
emptyEnv = Env { env = Map.empty }

-- | Create a new environment with only one binding
mkSingletonEnv :: String -> T.Type -> Env
mkSingletonEnv id ty = Env { env = Map.singleton id ty }


-- | Add a new variable to the environment
addVar :: Env -> String -> T.Type -> Env
addVar (Env e) id ty = Env { env = Map.insert id ty e }

-- | The function "lookupEnv" returns the value associated with a key in the environment
-- | If the key is not present, it recursively searches in the parent environment
lookup :: Env -> String -> Maybe T.Type
lookup e name = case Map.lookup name (env e) of
    Just t  -> Just t
    Nothing -> Nothing

-- | The function "merge" merges two environments
-- | If the two environments have a common key, the function keeps the value of the first environment
-- | TODO: forse aggiunge un errore/warning? Può essere?
merge :: Env -> Env -> Env
merge (Env e1) (Env e2) = Env { env = Map.union e1 e2 }

main = do
    putStrLn "Test - Env.hs"

    putStrLn "Creazione env (empty)"
    let env1 = emptyEnv

    putStrLn "Creazione env (singleton)"
    let env2 = mkSingletonEnv "x" T.RealType
        env3 = addVar env2 "y" T.IntegerType
        env4 = addVar env3 "z" T.BooleanType
    putStrLn $ show env4

    putStrLn "Creazione env (singleton)"
    let env5 = mkSingletonEnv "a" T.RealType
        env6 = addVar env5 "x" T.IntegerType
    putStrLn $ show env6

    putStrLn "Merge envs"
    let env7 = merge env1 env4
    putStrLn $ show env7

    putStrLn "Merge envs"
    let env8 = merge env6 env7 
    putStrLn $ show env8

