
-- compile with
-- ghci Env.hs -o Env.o TypeSys.hs Errors.hs

module Env where

import qualified Types as T
import qualified Data.Map as Map
import qualified ErrorMessage as Err
import Data.Maybe

data Env = Env { env :: Map.Map String EnvEntry} deriving (Show)

data ParameterType = Value | Reference
    deriving (Show)

instance Eq ParameterType where
    Value           == Value        = True
    Reference       == Reference    = True
    _               == _            = False

data ConstType = 
    IntConst        { value_int :: Int }
    | RealConst     { value_double :: Double }
    | BoolConst     { value_bool :: Bool }
    | StringConst   { value_string :: String }
    | CharConst     { value_char :: Char }
    deriving (Show)

data EnvEntry 
    = VarEntry          { ty :: T.Type, parent_env :: Bool, pos_decl :: (Int, Int) } 
    | ConstEntry        { ty :: T.Type, const_value :: ConstType, parent_env :: Bool, pos_decl :: (Int, Int) }
    | ForIteratorEntry  { ty :: T.Type, parent_env :: Bool, pos_decl :: (Int, Int) }
    | FunEntry          { params :: [(String, T.Type, ParameterType)], ret :: T.Type, forward :: Bool, permit_change :: Bool, changed :: Bool, parent_env :: Bool, pos_decl :: (Int, Int) }
    | ProcEntry         { params :: [(String, T.Type, ParameterType)], forward :: Bool, parent_env :: Bool, pos_decl :: (Int, Int) }
    deriving (Show)

instance Eq EnvEntry where
    VarEntry { ty = t1 }                == VarEntry { ty = t2 }                 = t1 == t2
    ConstEntry { ty = t1 }              == ConstEntry { ty = t2 }               = t1 == t2
    FunEntry { params = p1, ret = r1 }  == FunEntry { params = p2, ret = r2 }   = p1 == p2 && r1 == r2
    ProcEntry { params = p1 }           == ProcEntry { params = p2 }            = p1 == p2
    _                                   == _                                    = False

-- Create a new empty environment. It should be used as the root environment
emptyEnv :: Env
emptyEnv = Env { env = Map.empty }

-- Create a new environment with only one binding
mkSingletonEnv :: String -> EnvEntry -> Env
mkSingletonEnv id ty = Env { env = Map.singleton id ty }

-- Add a new variable to the environment
-- If the variable is already present, it is overwritten
addVar :: Env -> String -> EnvEntry -> Env
addVar (Env e) id ty = Env { env = Map.insert id ty e }

-- The function "lookupEnv" returns the value associated with a key in the environment
-- If the key is not present, it recursively searches in the parent environment
lookup_override :: Env -> String -> Maybe EnvEntry
lookup_override e name = case Map.lookup name (env e) of
            Just t  -> if (parent_env t) then Nothing else Just t
            Nothing -> Nothing

lookup :: Env -> String -> Maybe EnvEntry
lookup e name = case Map.lookup name (env e) of
            Just t  -> Just t
            Nothing -> Nothing
            
-- once we enter in a nested block change all entry to parent_env = True
-- this is not done to ConstEntry and ForIteratorEntry, we don't permit overriding of const and for iterator
change_parent_env :: Env -> Env
change_parent_env (Env e) = Env { env = Map.map change_parent_env_aux e } where
    change_parent_env_aux var@(VarEntry {}) = var { parent_env = True }
    change_parent_env_aux fun@(FunEntry {}) = fun { parent_env = True }
    change_parent_env_aux pro@(ProcEntry {}) = pro { parent_env = True }
    change_parent_env_aux x = x

-- The function "merge" merges two environments
-- If the two environments have a common key, the function keeps the value of the first environment
merge :: Env -> Env -> Env
merge (Env e1) (Env e2) = Env { env = Map.union e1 e2 }

-- The function gets the clashes in bindings between two environments
-- It returns a list of strings, each string is an error message
getClashes :: Env -> Env -> (Int, Int) -> [String]
getClashes (Env e1) (Env e2) pos = Map.foldrWithKey (\k v acc -> if Map.member k e2 then (Err.errMsgClash k pos) : acc else acc) [] e1

-- Function that checks if there are FunEntry or ProcEntry with forward = True and if there are returns their key
getForward :: Env -> [String]
getForward (Env e) = Map.foldrWithKey (\k v acc -> if (isForward v) then k : acc else acc) [] e where
    isForward (FunEntry { forward = True, parent_env = False }) = True
    isForward (ProcEntry { forward = True, parent_env = False }) = True
    isForward _ = False

-- create the function updateForward that foreach FunEntry or ProcEntry with forward = True, set forward = False
updateForward :: Env -> Env
updateForward (Env e) = Env { env = Map.map updateForwardAux e } where
    updateForwardAux fun@(FunEntry { forward = True, parent_env = False }) = fun { forward = False }
    updateForwardAux pro@(ProcEntry { forward = True, parent_env = False }) = pro { forward = False }
    updateForwardAux x = x

-- Function that given the old entry and the new entry, checks if they have the same signature
-- It returns a list of all different errors
checkSignature :: EnvEntry -> EnvEntry -> String -> (Int, Int) -> [String]
checkSignature (FunEntry { params = oldParams, ret = oldRet, forward = oldForw }) (FunEntry { params = newParams, ret = newRet }) id pos =
    let err_already_declared = if oldForw == False then [Err.errMsgFunctionAlreadyImpl id pos] else []
        err_diff_params = if (oldParams /= newParams) then [Err.errMsgDifferentParams id (show oldParams) (show newParams) pos] else []
        err_diff_ret = if (oldRet /= newRet) then [Err.errMsgDifferentRet id (show oldRet) (show newRet) pos] else []
    in err_already_declared ++ err_diff_params ++ err_diff_ret
checkSignature (ProcEntry { params = oldParams, forward = oldForw }) (ProcEntry { params = newParams }) id pos = 
    let err_already_declared = if oldForw == False then [Err.errMsgFunctionAlreadyImpl id pos] else []
        err_diff_params = if (oldParams /= newParams) then [Err.errMsgDifferentParams id (show oldParams) (show newParams) pos] else []
    in err_already_declared ++ err_diff_params
checkSignature _ _ id pos = [Err.errMsgAlreadyDeclared id pos]
