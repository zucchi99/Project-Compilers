
-- compile with
-- ghci StaticSemantic.hs -o StaticSemantic.o Env.hs
{-# LANGUAGE FlexibleInstances #-}

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
-- check if a variable is in the environment for the declaration of a variable
checkPresenceDecl :: Ident -> E.Env -> (Int, Int) -> [[Char]]
checkPresenceDecl id env pos = 
    case E.lookup_override env (id_name id) of
        Just _  -> [Err.errMsgAlreadyDeclared (id_name id) pos]
        Nothing -> []

-- check if the function or procedure is already in the environment
checkPresenceDeclFuncProc :: E.EnvEntry -> String -> E.Env -> (Int, Int) -> (E.Env, [String])
checkPresenceDeclFuncProc new_entry id env pos = 
    case E.lookup_override env id of
        -- if it is in the enviroment, check if it's a forward declaration and if the parameters and return type are the same
        Just old_entry  -> case E.checkSignature old_entry new_entry id pos of
            []      -> (E.addVar env id new_entry, []) 
            errors  -> (env, errors)
        -- if it's not in the enviroment, add it 
        Nothing         -> (E.addVar env id new_entry, [])

-- check that array dimensions are integers and of the same dimensions
mkArrTy :: [(Int,Int)] -> [RightExp] -> (Int, Int) -> [String]
mkArrTy dim locs pos =
    let error_dim = case (length locs) == (length dim) of
            True  -> []
            False -> [ Err.errMsgWrongArrayDim pos ]
        error_types = case all (== T.IntegerType) $ map (right_exp_type) locs of
            True  -> []
            False -> [ Err.errMsgWrongArrayIndex pos ]
    in error_dim ++ error_types

-- check of guard to see if it's a boolean
mkIfErrs :: T.Type -> (Int, Int) -> [String]
mkIfErrs guard_type pos = 
    case guard_type of
        (T.ErrorType)   -> [] -- error already found before, do not add new errors
        (T.BooleanType) -> []
        t               -> [ (Err.errMsgUnexpectedType "Guard" T.BooleanType t pos) ]

-- check if expected_type is compatible with found_type
mkIdDeclErrs :: String -> T.Type -> T.Type -> (Int, Int) -> [String]
mkIdDeclErrs obj expected_type found_type pos = 
    case (T.sup expected_type found_type) == T.ErrorType of
        True  -> [ Err.errMsgUnexpectedType obj expected_type found_type pos ]
        False -> []

-- check sx and dx of a binary operation and return the merged errors
check_sx_dx :: RightExp -> RightExp -> E.Env -> (RightExp, RightExp, [String])
check_sx_dx sx dx p_env = 
    let new_sx = staticsemanticAux (sx {right_exp_env = p_env})
        new_dx = staticsemanticAux (dx {right_exp_env = p_env})
        errors = (right_exp_errors new_sx) ++ (right_exp_errors new_dx)
    in (new_sx, new_dx, errors)

is_one_error :: T.Type -> T.Type -> Bool
is_one_error T.ErrorType _ = True
is_one_error _ T.ErrorType = True
is_one_error _ _ = False

coerc_sx_dx :: RightExp -> RightExp -> (T.Type -> T.Type -> T.Type) -> String -> (Int, Int) -> (T.Type, RightExp, RightExp, [String])
coerc_sx_dx sx dx operation name_op pos = 
    let sup_type = operation (right_exp_type sx) (right_exp_type dx)
        (sx_coerc, dx_coerc, errs) = case (sup_type, is_one_error (right_exp_type sx) (right_exp_type dx)) of
            (T.ErrorType, True) -> (sx, dx, [])
            (T.ErrorType, _)    -> (sx, dx, [Err.errMsgOperationNotPermitted (right_exp_type sx) (right_exp_type dx) name_op pos])
            (sup_type, _)       -> case (right_exp_type sx) == (right_exp_type dx) of
                True  -> (sx, dx, [])
                False -> case (right_exp_type sx) /= sup_type of
                    True  -> (apply_coercion sup_type sx, dx, [])
                    False -> (sx, apply_coercion sup_type dx, [])
    in (sup_type, sx_coerc, dx_coerc, errs)

-- encapsulate the right expression in a coercion expression
apply_coercion :: T.Type -> RightExp -> RightExp
apply_coercion to_type main_r = 
    let pos = right_exp_pos main_r
        env = right_exp_env main_r
        errs = right_exp_errors main_r
    in RightExpCoercion main_r (right_exp_type main_r) to_type pos env errs

-- encapsulate the right expression in a not expression
apply_not ::  RightExp -> RightExp
apply_not main_r = 
    let ty = right_exp_type main_r
        pos = right_exp_pos main_r
        env = right_exp_env main_r
        errs = right_exp_errors main_r
    in RightExpNot main_r pos ty env errs

-- given list of params declarations, create a list of (name, type) for the function or procedure definition
create_params_for_func_proc :: [Declaration] -> ([(String, T.Type, E.ParameterType)], [String])
create_params_for_func_proc params = unzip $ map create_param params where
    create_param decl = case (fromJust (param_type_maybe decl), (variable_type decl)) of
        (E.Value, (T.ArrayType _ _))                        -> ((id_name (variable_name decl), variable_type decl, E.Value), Err.errMsgUnexpectedParams (id_name (variable_name decl)) (declaration_pos decl))
        (E.Value, (T.PointerType _))                        -> ((id_name (variable_name decl), variable_type decl, E.Value), Err.errMsgUnexpectedParams (id_name (variable_name decl)) (declaration_pos decl))
        (E.Value, _)                                        -> ((id_name (variable_name decl), variable_type decl, E.Value), "")
        (E.Reference, (T.PointerType (T.PointerType _)))    -> ((id_name (variable_name decl), variable_type decl, E.Reference), Err.errMsgUnexpectedParams (id_name (variable_name decl)) (declaration_pos decl))
        (E.Reference, (T.PointerType _))                    -> ((id_name (variable_name decl), variable_type decl, E.Reference), "")
        (E.Reference, _)                                    -> ((id_name (variable_name decl), (variable_type decl), E.Reference), Err.errMsgUnexpectedParams (id_name (variable_name decl)) (declaration_pos decl))

-- Given the old environment, add or override the break and continue keywords
add_break_continue :: E.Env -> (Int, Int) -> E.Env
add_break_continue env pos = 
    let env_break = E.mkSingletonEnv "break" (E.ConstEntry T.TBDType (E.StringConst "break") True pos)
        env_break_continue = E.addVar env_break "continue" (E.ConstEntry T.TBDType (E.StringConst "continue") True pos)
    in E.merge env_break_continue env

--check_fun_call :: String -> [RightExp] -> (Int, Int) -> E.Env -> (Maybe T.Type, [RightExp], [String])
check_fun_call function_name params pos env =
    let (fun_type, rexps_coerced, errors_tot) = case E.lookup env function_name of
            Just (E.FunEntry entry_params ty_ret _ _ _ _ _) -> get_check_fun_call entry_params (Just ty_ret) params
            Just (E.ProcEntry entry_params _ _ _)           -> get_check_fun_call entry_params Nothing params
            Nothing                                         -> (Just T.ErrorType, [], [Err.errMsgNotDeclared function_name pos])
            _                                               -> (Just T.ErrorType, [], [Err.errMsgNotFunctionProcedure function_name pos])

    in (fun_type, rexps_coerced, errors_tot)

--get_check_fun_call :: [(String, T.Type, E.ParameterType)] -> Maybe T.Type -> [(RightExp, String)] -> (Maybe T.Type, [RightExp], [String])
get_check_fun_call entry_params ty_ret params = 
    let (right_exps, errs_cleaned) = case length params == length entry_params of
            False   ->  (params, ["Wrong number of parameters"])
            True    ->  let (right_exps, errs) = unzip (check_params_func_call params entry_params)
                            errs_cleaned = filter (not . null) errs
                        in (right_exps, errs_cleaned)
    in (ty_ret, right_exps, errs_cleaned)


check_params_func_call :: [RightExp] -> [(String, T.Type, E.ParameterType)] -> [(RightExp, String)]
check_params_func_call [] _ = []
check_params_func_call xs [] = map (\x -> (x,"")) xs
check_params_func_call (x:xs) ((_, t, E.Reference):ys) = 
    let x_type = right_exp_type x
        errs = case is_maybe_left_exp x of
            Just left_exp -> case is_acceptable_left_exp left_exp of
                (True, _)  -> case (T.need_coerc t x_type || t == x_type) of
                    True  -> ""
                    False -> "Wrong type for parameter " ++ show (length ys + 1)
                (False, left_exp_name) -> "Devi passarmi un left expression valida, non un " ++ left_exp_name
            Nothing       -> "Devi passarmi un left expression, non un valore"
    in  (coerc_if_needed t x, errs) : check_params_func_call xs ys
check_params_func_call (x:xs) ((_, t, _):ys) = case (T.need_coerc t (right_exp_type x) || t == (right_exp_type x)) of
    True  -> (coerc_if_needed t x, "") : check_params_func_call xs ys
    False -> (coerc_if_needed t x, "Wrong type for parameter " ++ show (length ys + 1)) : check_params_func_call xs ys

is_maybe_left_exp :: RightExp -> Maybe LeftExp
is_maybe_left_exp (RightExpLeftExp {left_exp_right_exp=left_exp}) = Just left_exp
is_maybe_left_exp (RightExpCoercion {right_exp_coercion=left_exp}) = is_maybe_left_exp left_exp
is_maybe_left_exp _ = Nothing

is_acceptable_left_exp :: LeftExp -> (Bool, String)
is_acceptable_left_exp (LeftExpPointerAddress _ _ _ _ _) = (False, "pointer address")
is_acceptable_left_exp (LeftExpForIterator _ _ _ _ _) = (False, "for iterator")
is_acceptable_left_exp (LeftExpConst _ _ _ _ _ _) = (False, "constant")
is_acceptable_left_exp _ = (True, "")


-- Se i tipi sono compatibili, ritorna True, altrimenti False
-- Viene usata sopra per controllare che i parametri di una funzione siano compatibili con quelli della sua dichiarazione
-- Se sono compatibili li consideriamo "uguali", la coercion viene fatta dopo
are_compatibile_types :: [T.Type] -> [T.Type] -> Bool
are_compatibile_types [] [] = True
are_compatibile_types _  [] = False
are_compatibile_types [] _ = False
are_compatibile_types (x:xs) (y:ys) = case (T.need_coerc x y || x == y) of 
    True  -> are_compatibile_types xs ys
    otherwise -> False

coerc_if_needed :: T.Type -> RightExp -> RightExp
coerc_if_needed t x = case T.need_coerc t (right_exp_type x) of
    True -> (apply_coercion (T.sup t (right_exp_type x)) x)
    _    -> x

-- check for and, or
check_and_or :: RightExp -> RightExp -> (Int, Int) -> E.Env -> (RightExp, RightExp, [String])
check_and_or sx dx pos parent_env =
        -- Checking if both sx and dx are boolean
    let (sx_checked, dx_checked, merged_errors) = check_sx_dx sx dx parent_env 
        errs_rel_not_bool = if T.all_same_type [T.BooleanType, (right_exp_type sx_checked), (right_exp_type dx_checked)]
            then [] -- Nothing's wrong
            else [Err.errMsgRelationNotBool (right_exp_type sx_checked) (right_exp_type dx_checked) pos] -- at least one is not boolean
        -- concateno gli errori
        errors_tot = merged_errors ++ errs_rel_not_bool
    in (sx_checked, dx_checked, errors_tot)

-- check operation like <, >, <=, =>, ==, <>, +, -, *, /
check_op :: RightExp -> RightExp -> (Int, Int) -> E.Env -> (T.Type -> T.Type -> T.Type) -> String -> (T.Type, RightExp, RightExp, [String])
check_op sx dx pos parent_env operation name_op =
    -- Checking if both sx and dx are compatibile for a relation operation
    let (sx_checked, dx_checked, merged_errors) = check_sx_dx sx dx parent_env
        (sup_type, sx_coerc, dx_coerc, coerc_errors) = coerc_sx_dx sx_checked dx_checked operation name_op pos
        -- concateno gli errori
        errors_tot = merged_errors ++ coerc_errors
    in (sup_type, sx_coerc, dx_coerc, errors_tot)

-- used to convert a ident representing a constant to a right expression
from_leftexpconst_to_rightexp :: LeftExp -> (Int, Int) -> E.Env -> [String] -> RightExp
from_leftexpconst_to_rightexp (LeftExpConst ident value l_pos ty l_env l_errs) pos env errs = 
    case value of
        E.BoolConst b   -> RightExpBoolean b pos ty env errs
        E.IntConst i    -> RightExpInteger i pos ty env errs
        E.RealConst r   -> RightExpReal r pos ty env errs
        E.CharConst c   -> RightExpChar c pos ty env errs
        E.StringConst s -> RightExpLeftExp (LeftExpConst ident value l_pos ty l_env l_errs) pos ty env errs

-- check every Read type of Read...()
check_read_primitive :: LeftExp -> T.Type -> (Int, Int) -> E.Env -> (LeftExp, [String])
check_read_primitive left_exp read_type pos env = 
    let lexp_checked = staticsemanticAux left_exp {left_exp_env = env}
        (l_type, l_err) = (left_exp_type lexp_checked, left_exp_errors lexp_checked)

        -- Controllo se è una costante
        (is_const, err_const) = case lexp_checked of
            LeftExpConst id _ _ _ _ _       -> (True, [Err.errMsgAssignToConst (id_name id) pos] )
            LeftExpForIterator id _ _ _ _   -> (True, [Err.errMsgAssignToForIterator (id_name id) pos])
            _                               -> (False, [])

        -- Controllo che left_exp sia un real
        err_read = case (l_type == read_type) || is_const || l_type == T.ErrorType of
            True    -> []
            _       -> [Err.errMsgWrongReadPrimitiveType read_type l_type pos]

        -- concateno gli errori
        errors_tot = l_err ++ err_const ++ err_read
    in (lexp_checked, errors_tot)

-- check every Read type of Write...()
check_write_primitive :: RightExp -> T.Type -> (Int, Int) -> E.Env -> (RightExp, [String])
check_write_primitive right_exp write_type pos env = 
    let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
        (r_type, r_err) = (right_exp_type rexp_checked, right_exp_errors rexp_checked)
        -- Controllo che right_exp sia un intero
        err_write = case r_type == write_type || r_type == T.ErrorType of
            True -> []
            _ -> [Err.errMsgWrongWritePrimitiveType write_type r_type pos]
        -- concateno gli errori
        errors_tot = r_err ++ err_write
    in (rexp_checked, errors_tot)

-- add costant to env if it's a specific type (integer, real, boolean, char, string) else error
add_const_to_env :: E.Env -> String -> RightExp -> (Int, Int) -> (E.Env, [String])
add_const_to_env env name value pos = 
    let (new_env, errs) = case value of
            RightExpInteger dx _ ty _ _ -> (E.addVar env name (E.ConstEntry ty (E.IntConst dx) False pos), [])
            RightExpReal    dx _ ty _ _ -> (E.addVar env name (E.ConstEntry ty (E.RealConst dx) False pos), [])
            RightExpBoolean dx _ ty _ _ -> (E.addVar env name (E.ConstEntry ty (E.BoolConst dx) False pos), [])
            RightExpChar    dx _ ty _ _ -> (E.addVar env name (E.ConstEntry ty (E.CharConst dx) False pos), [])
            RightExpString  dx _ ty _ _ -> (E.addVar env name (E.ConstEntry ty (E.StringConst dx) False pos), [])
            _                           -> (env, [Err.errMsgConstLimit name pos])
    in (new_env, errs)

-- __________________________ STATIC SEMANTIC ANALISYS
static_semantic_check :: Err Program -> Err Program
static_semantic_check x = 
    let program_checked = case x of
            (ErrM.Ok program)   -> (ErrM.Ok (staticsemanticAux program))
            (ErrM.Bad err)      -> (ErrM.Bad err)

        program_errors = case program_checked of
            (ErrM.Ok (ProgramStart _ _ _ _ errs)) -> intercalate "\n\n" errs
            (ErrM.Bad err)                        -> err

        static_semantic_result = case program_errors of
            "" -> program_checked
            _  -> (ErrM.Bad program_errors)

    in static_semantic_result

static_semantic_debug:: Err Program -> Program
static_semantic_debug x = 
    let program_checked = case x of
            (ErrM.Ok program)   -> staticsemanticAux program
            (ErrM.Bad err)      -> ProgramStart (Ident "error" (0,0) E.emptyEnv []) (Block [] [] (0,0) E.emptyEnv []) (0,0) E.emptyEnv []

    in program_checked

static_semantic_errors :: Err Program -> String
static_semantic_errors x = case x of
    (ErrM.Ok a)    -> "The Static Semantic analysis did not found any error\n"
    (ErrM.Bad err) -> "\n" ++ err ++ "\n"    

-- __________________________ STATIC SEMANTIC CLASSES
class StaticSemanticClass a where
    staticsemanticAux :: a -> a

instance StaticSemanticClass Program where
    staticsemanticAux (ProgramStart name block pos env errors) =
        let block_checked = staticsemanticAux block
        in (ProgramStart name block_checked pos env (errors ++ (block_errors block_checked)))

instance StaticSemanticClass Ident where
    staticsemanticAux ident = ident 

-- Since [Ident] should exists no more, this instance should be not needed
instance StaticSemanticClass [Ident] where
    staticsemanticAux idents = idents

instance StaticSemanticClass Block where
    staticsemanticAux (Block decls stmts pos env errors) =
            -- parto dall'env e aggiungo le dichiarazioni
        let decls_checked = staticsemanticAux $ map (\x -> x {declaration_env = env}) decls
            (env_aft_decls, errs_aft_decls) = case decls_checked of
                []  -> (env, [])
                xs  -> (declaration_env (last xs), foldl (\acc x -> acc ++ (declaration_errors x)) [] xs)
            -- controllo che tutte le forward declaration nel blocco corrente siano state definite -> altrimenti errore
            errs_forw_decls = map (\x -> Err.errMsgNotImplemented x pos) (E.getForward env_aft_decls)
            -- dopo le segno come definite, altrimenti ogni blocco innestato le riconosce come forward e ritorna errore
            env_aft_update = E.updateForward env_aft_decls
            -- Ogni statement può modificare l'env, ma non può aggiugnere nuove dichiarazioni
            stmts_checked = staticsemanticAux $ map (\x -> x {statement_env = env_aft_update}) stmts
            (env_aft_stmts, errs_aft_stmts) = case stmts_checked of
                []  -> (env, [])
                xs  -> (statement_env (last xs), foldl (\acc x -> acc ++ (statement_errors x)) [] xs)
            -- concat all errors
            tot_errors = errors ++ errs_aft_decls ++ errs_forw_decls ++ errs_aft_stmts
        in (Block decls_checked stmts_checked pos env_aft_stmts tot_errors)

instance StaticSemanticClass Declaration where
    staticsemanticAux (DeclarationCostant id maybe_type value pos env errors) = 
            -- controllo value
        let value_checked = staticsemanticAux (value {right_exp_env = env})
            -- se il tipo dichiarato è diverso dal tipo del valore -> ritorno errore
            (type_aft_decl, err_type) = case maybe_type of
                Nothing -> (Just (right_exp_type value_checked), [])
                Just t  -> (Just t, mkIdDeclErrs ("The variable \"" ++ (id_name id) ++ "\"") t (right_exp_type value_checked) pos)
            -- se l'id è già nell'env -> ritorno errore
            (env_aft_decl, err_aft_decl) = case checkPresenceDecl id env pos of
                -- SISTEMARE QUESTO
                []  -> add_const_to_env env (id_name id) value_checked pos
                err -> (env, err)
        in (DeclarationCostant (id { ident_env = env_aft_decl }) type_aft_decl value_checked pos env_aft_decl (errors ++ err_type ++ err_aft_decl))

    staticsemanticAux (DeclarationVariable id var_type value_maybe param_type_maybe pos env errors) =
            -- se il tipo dichiarato è diverso dal tipo del valore -> ritorno errore
        let value_checked = case value_maybe of
                Nothing -> Nothing
                Just var_value -> Just (staticsemanticAux (var_value {right_exp_env = env}))

            -- se il tipo dichiarato è diverso dal tipo del valore -> ritorno errore
            err_type = case value_checked of
                Nothing -> []
                Just var_value ->  mkIdDeclErrs ("The variable \"" ++ (id_name id) ++ "\"") var_type (right_exp_type var_value) pos

            -- se l'id è già nell'env -> ritorno errore
            (env_aft_decl, err_aft_decl) = case checkPresenceDecl id env pos of
                [] -> (E.addVar env (id_name id) (E.VarEntry var_type False pos), [])
                err -> (env, err)
        in (DeclarationVariable (id { ident_env = env_aft_decl }) var_type value_checked param_type_maybe pos env_aft_decl (errors ++ err_type ++ err_aft_decl))

    staticsemanticAux (DeclarationFunction id params fun_type maybe_block pos env errors) =
            -- create a function entry for the enviroment, with params and return type
        let (new_params, params_errors) = create_params_for_func_proc params
            new_fun_entry = case maybe_block of
                Nothing -> E.FunEntry new_params fun_type True False True False pos -- forward declaration (i booleani rappresentano forward, permit_change, changed, parent_env)
                Just _  -> E.FunEntry new_params fun_type False True False False pos -- function definition (i booleani rappresentano forward, permit_change, changed, parent_env)

            -- check if is already present in the env a forward declaration for a function
            (env_after_adding_func, err_already_declared) = checkPresenceDeclFuncProc new_fun_entry (id_name id) env pos

            -- prendo tutto quello presente nell'env e lo aggiungo al blocco facendo presente che andremo nel blocco innestato
            nested_env = E.change_parent_env env
            env_before_params = E.addVar nested_env (id_name id) new_fun_entry

            -- aggiungo i parametri alla funzione
            params_checked = staticsemanticAux $ map (\x -> x {declaration_env = env_before_params}) params
            (env_aft_params, errs_aft_params) = case params_checked of
                []  -> (env_before_params, [])
                xs  -> (declaration_env (last xs), foldl (\acc x -> acc ++ (declaration_errors x)) [] xs)

            -- parto dall'env creato dall'ultimo parametro e lo passo al maybe_block se presente
            block = case maybe_block of
                Nothing -> Nothing
                Just b  -> Just (staticsemanticAux (b {block_env = env_aft_params}))

            -- mi serve estrarre l'env per il controllo del return
            (env_aft_block, errs_aft_block) = case block of
                Nothing -> (env_after_adding_func, [])
                Just b  -> (block_env b, block_errors b)

            -- controllare che la variabile di ritorno sia modificata
            fun_entry = fromJust (E.lookup env_aft_block (id_name id))
            err_if_return = case fun_entry of
                E.FunEntry _ _ _ _ False _ _ -> [Err.errMsgReturnNotSet (id_name id) pos]
                _ -> []

            -- set changed come False, altrimenti la forward declaration permette di non avere il valore di ritorno settato successivamente
            entry_final = case maybe_block of
                Nothing -> fun_entry { E.permit_change = False, E.changed = False } -- forward declaration
                Just _  -> fun_entry { E.permit_change = False, E.changed = True } -- function definition

            -- faccio un override della funzione con gli ultimi 2 bool aggiustati all'env
            env_final = E.addVar env_after_adding_func (id_name id) entry_final

            err_pointer_array = case fun_type of
                T.ArrayType _ _ -> [Err.errMsgFunctionError (id_name id) "A function can not return an array" pos]
                T.PointerType _ -> [Err.errMsgFunctionError (id_name id) "A function can not return a pointer" pos]
                _ -> []

            -- concateno gli errori
            tot_errors = errors ++ filter (not . null) params_errors ++ err_already_declared ++ err_pointer_array ++ errs_aft_params ++ errs_aft_block ++ err_if_return
        in (DeclarationFunction (id { ident_env = env_final }) params_checked fun_type block pos env_final tot_errors)

    staticsemanticAux (DeclarationProcedure id params maybe_block pos env errors) =
            -- create a function entry for the enviroment, with params and return type
        let (new_params, params_errors) = create_params_for_func_proc params
            new_pro_entry = case maybe_block of
                Nothing -> E.ProcEntry new_params True False pos
                Just _  -> E.ProcEntry new_params False False pos

            -- check if is already present in the env a forward declaration for a function
            (env_after_adding_proc, err_already_declared) = checkPresenceDeclFuncProc new_pro_entry (id_name id) env pos

            -- prendo tutto quello presente nell'env e lo aggiungo al blocco facendo presente che andremo nel blocco innestato
            nested_env = E.change_parent_env env
            env_before_params = E.addVar nested_env (id_name id) new_pro_entry

            -- aggiungo i parametri alla funzione
            params_checked = staticsemanticAux $ map (\x -> x {declaration_env = env_before_params}) params
            (env_aft_params, errs_aft_params) = case params_checked of
                []  -> (env_before_params, [])
                xs  -> (declaration_env (last xs), foldl (\acc x -> acc ++ (declaration_errors x)) [] xs)

            -- parto dall'env creato dall'ultimo parametro e lo passo al maybe_block se presente
            block = case maybe_block of
                Nothing -> Nothing
                Just b  -> Just (staticsemanticAux (b {block_env = env_aft_params}))
                -- Just b  -> Just (staticsemanticAux (b {block_env = (E.merge env_aft_params env_after_adding_proc)}))
            
            -- non mi serve estrarre l'env, a differenza delle function
            errs_aft_block = case block of
                Nothing -> []
                Just b  -> block_errors b

            -- concateno gli errori
            tot_errors = errors ++ filter (not . null) params_errors ++ err_already_declared ++ errs_aft_params ++ errs_aft_block
        in (DeclarationProcedure (id { ident_env = env_after_adding_proc }) params_checked block pos env_after_adding_proc tot_errors)

instance StaticSemanticClass [Declaration] where
    -- Parto dal primo enviroment e ogni enviroment si basa sul precedente
    -- L'ultimo elemento conterrà l'enviroment finale
    -- Gli errori devono essere tutti recuperati e messi in un unico array
    staticsemanticAux [] = []
    staticsemanticAux (x:[]) = [staticsemanticAux x]
    staticsemanticAux (x:xs) = foldl (\acc x -> acc ++ [staticsemanticAux_oldEnv x acc]) [staticsemanticAux x] xs where
        staticsemanticAux_oldEnv x old_decls = staticsemanticAux (x {declaration_env = (declaration_env (last old_decls))})

instance StaticSemanticClass Statement where
    staticsemanticAux (StatementBlock block pos env errors) = 
            --                                  ^^^
            -- env in questo caso è l'enviroment da cui vogliamo iniziare a fare il merge
            -- così facendo è possibile aggiungere le dichiarazioni dei parametri o aggiungere i break e i continue

            -- prendo tutto quello presente nell'env e lo aggiungo al blocco facendo presente che andremo nel blocco innestato
        let block_checked = staticsemanticAux (block {block_env = E.change_parent_env env})
        in (StatementBlock block_checked pos env (errors ++ (block_errors block_checked)))

    staticsemanticAux (StatementIf cond then_body maybe_else_body pos env errors) =
            -- eseguo la condizione
        let cond_checked = staticsemanticAux (cond {right_exp_env = env})
            -- Controllo che la condizione sia booleana
            cond_errors = mkIfErrs (right_exp_type cond_checked) (right_exp_pos cond_checked) ++ (right_exp_errors cond_checked)
            
            -- Controllo che il then_body sia corretto
            then_body_checked = staticsemanticAux (then_body {statement_env = env})
            then_errors = statement_errors then_body_checked
            
            -- Controllo che il else_body sia corretto se presente
            else_body_checked = case maybe_else_body of
                Nothing -> Nothing
                Just else_body -> Just (staticsemanticAux (else_body {else_block_env = env}))
            else_errors = case else_body_checked of
                Nothing -> []
                Just else_body -> else_block_errors else_body

            -- Concateno gli errori
            errors_tot = errors ++ cond_errors ++ then_errors ++ else_errors
        in (StatementIf cond_checked then_body_checked else_body_checked pos env errors_tot)

    staticsemanticAux (StatementFor cond then_body assign pos env errors) =
            -- create a for iterator entry for the enviroment, with params and return type
        let assign_checked = staticsemanticAux (assign {assign_env = env})
            (left_exp_iterator, assign_errs) = (left_exp_assignment assign_checked, assign_errors assign_checked)
            
            -- segno la posizione dell'iteratore come quella di dichiarazione
            old_declaration_pos = case E.lookup env (id_name (left_exp_name left_exp_iterator)) of
                Just entry  -> E.pos_decl entry
                _           -> (0,0) -- dummy position

            -- Controllo che var sia intero
            var_errors = case left_exp_type left_exp_iterator of
                T.IntegerType   -> []
                _               -> [Err.errMsgUnexpectedType "The for iterator " "integer" (left_exp_type left_exp_iterator) pos]

            -- Controllo cond e mi assicuro abbia tipo intero
            cond_checked = staticsemanticAux (cond {right_exp_env = env})
            cond_errors = case right_exp_type (dx cond_checked) of
                T.IntegerType   -> []
                _               -> [Err.errMsgUnexpectedType "The for condition " "integer" (right_exp_type cond_checked) (right_exp_pos cond_checked)]

            -- metto l'iteratore del for nell'env come ForIteratorEntry immutabile
            env_with_iterator = E.addVar env (id_name (left_exp_name left_exp_iterator)) (E.ForIteratorEntry T.IntegerType False old_declaration_pos)

            -- Controllo che il then_body sia corretto, aggiungendo break e continue all'env
            -- Il ForIteratorEntry è immutabile, quindi non può essere modificato, altrimenti ritorna un errore
            then_body_checked = staticsemanticAux (then_body {statement_env = (add_break_continue env_with_iterator pos)})
            then_errors = statement_errors then_body_checked
            
            -- Concateno gli errori
            errors_tot = errors ++ var_errors ++ assign_errs ++ cond_errors ++ then_errors

        in (StatementFor cond_checked then_body_checked assign_checked pos env errors_tot)

    staticsemanticAux (StatementWhile cond then_body pos env errors) =
            -- eseguo la condizione
        let cond_checked = staticsemanticAux (cond {right_exp_env = env})
            -- Controllo che la condizione sia booleana
            cond_errors = mkIfErrs (right_exp_type cond_checked) (right_exp_pos cond_checked) ++ (right_exp_errors cond_checked)
            
            -- Aggiungo le dichiarazioni dei break e dei continue all'enviroment
            env_with_break_continue = add_break_continue env pos

            -- Controllo che il body sia corretto
            body_checked = staticsemanticAux (then_body {statement_env = env_with_break_continue})
            body_errors = statement_errors body_checked

            errors_tot = errors ++ cond_errors ++ body_errors
        in (StatementWhile cond_checked body_checked pos env errors_tot)

    staticsemanticAux (StatementRepeatUntil cond then_body pos env errors) =
            -- eseguo la condizione
        let cond_checked = staticsemanticAux (cond {right_exp_env = env})
            -- Controllo che la condizione sia booleana
            cond_errors = mkIfErrs (right_exp_type cond_checked) (right_exp_pos cond_checked) ++ (right_exp_errors cond_checked)
            
            -- Aggiungo le dichiarazioni dei break e dei continue all'enviroment
            env_with_break_continue = add_break_continue env pos
            
            -- Controllo che il body sia corretto
            body_checked = staticsemanticAux (then_body {statement_env = env_with_break_continue})
            body_errors = statement_errors body_checked

            errors_tot = errors ++ cond_errors ++ body_errors
        in (StatementRepeatUntil cond_checked body_checked pos env errors_tot)

    staticsemanticAux (StatementAssign assign pos env errors) =
        let assign_checked = staticsemanticAux (assign {assign_env = env})
            (env_assign, errors_assign) = (assign_env assign_checked, assign_errors assign_checked)

        in (StatementAssign assign_checked pos env_assign (errors ++ errors_assign))

    staticsemanticAux (StatementFuncProcCall id params pos env errors) =
        -- Controllo che la funzione sia presente nell'env, visto che deve essere già stata dichiarata
        -- Nel caso ci si accorga che la funzione in realtà è una procedura, allora si ritorna un errore:
        --     non avrebbe senso assegnare il valore di ritorno di una procedura, visto che non esiste
        let function_name = id_name id
            params_checked = staticsemanticAux $ map (\param -> param {right_exp_env = env}) params
            errs_params = concat $ map right_exp_errors params_checked

            -- controllo che i tipi corrispondano
            (_, params_coerced, errs_fun) = check_fun_call function_name params_checked pos env

            -- concateno gli errori
            errors_tot = errors ++ errs_params ++ errs_fun

        in (StatementFuncProcCall (id { ident_env = env }) params_coerced pos env errors_tot)

    staticsemanticAux (StatementWrite write_primitive pos env errors) =
            -- Controllo che il write_primitive sia corretto
        let write_primitive_cheched = staticsemanticAux (write_primitive {write_primitive_env = env})
            (env_write, errors_write) = (write_primitive_env write_primitive_cheched, write_primitive_errors write_primitive_cheched)
        in (StatementWrite write_primitive_cheched pos env_write (errors ++ errors_write))

    staticsemanticAux (StatementRead read_primitive pos env errors) =
            -- Controllo che il read_primitive sia corretto
        let read_primitive_cheched = staticsemanticAux (read_primitive {read_primitive_env = env})
            (env_read, errors_read) = (read_primitive_env read_primitive_cheched, read_primitive_errors read_primitive_cheched)
        in (StatementRead read_primitive_cheched pos env_read (errors ++ errors_read))

    staticsemanticAux (StatementContinue pos env errors) =
            -- Controllo che ci sia il continue nell'env
        let err_continue_not_found = 
                case E.lookup env "continue" of
                    Just _  -> []
                    Nothing -> [Err.errMsgWrongLoopControl "continue" pos]
        in (StatementContinue pos env (errors ++ err_continue_not_found))

    staticsemanticAux (StatementBreak pos env errors) = 
            -- Controllo che ci sia il break nell'env
        let err_break_not_found = 
                case E.lookup env "break" of
                    Just _  -> []
                    Nothing -> [Err.errMsgWrongLoopControl "break" pos]
        in (StatementBreak pos env (errors ++ err_break_not_found))

instance StaticSemanticClass [Statement] where
    -- Parto dal primo enviroment e ogni enviroment si basa sul precedente
    -- L'ultimo elemento conterrà l'enviroment finale
    -- Gli errori devono essere tutti recuperati e messi in un unico array
    staticsemanticAux [] = []
    staticsemanticAux (x:[]) = [staticsemanticAux x]
    staticsemanticAux (x:xs) = foldl (\acc x -> acc ++ [staticsemanticAux_oldEnv x acc]) [staticsemanticAux x] xs where
        staticsemanticAux_oldEnv x old_decls = staticsemanticAux (x {statement_env = (statement_env (last old_decls))})

instance StaticSemanticClass ElseBlock where
    staticsemanticAux (ElseBlock else_body pos env errors) =
        let else_body_checked = staticsemanticAux (else_body {statement_env = env})
        in (ElseBlock else_body_checked pos env (errors ++ (statement_errors else_body_checked)))

instance StaticSemanticClass RightExp where
    staticsemanticAux (RightExpOr sx dx pos ty env errors) =
        let (sx_checked, dx_checked, errors_tot) = check_and_or sx dx pos env
        in (RightExpOr sx_checked dx_checked pos T.BooleanType env (errors ++ errors_tot))
    
    staticsemanticAux (RightExpAnd sx dx pos ty env errors) = 
        let (sx_checked, dx_checked, errors_tot) = check_and_or sx dx pos env
        in (RightExpAnd sx_checked dx_checked pos T.BooleanType env (errors ++ errors_tot))

    staticsemanticAux (RightExpGreater sx dx pos ty env errors) =
        let (_, sx_checked, dx_checked, errors_tot) = check_op sx dx pos env T.comparison "greater than"
        in (RightExpGreater sx_checked dx_checked pos T.BooleanType env (errors ++ errors_tot))
    
    staticsemanticAux (RightExpLess sx dx pos ty env errors) =
        let (_, sx_checked, dx_checked, errors_tot) = check_op sx dx pos env T.comparison "less than"
        in (RightExpLess sx_checked dx_checked pos T.BooleanType env (errors ++ errors_tot))

    staticsemanticAux (RightExpGreaterEqual sx dx pos ty env errors) =
        let (_, sx_checked, dx_checked, errors_tot) = check_op sx dx pos env T.comparison "greater equal"
        in (RightExpGreaterEqual sx_checked dx_checked pos T.BooleanType env (errors ++ errors_tot))

    staticsemanticAux (RightExpLessEqual sx dx pos ty env errors) =
        let (_, sx_checked, dx_checked, errors_tot) = check_op sx dx pos env T.comparison "less equal"
        in (RightExpLessEqual sx_checked dx_checked pos T.BooleanType env (errors ++ errors_tot))

    staticsemanticAux (RightExpEqual sx dx pos ty env errors) =
        let (_, sx_checked, dx_checked, errors_tot) = check_op sx dx pos env T.equality "equal"
        in (RightExpEqual sx_checked dx_checked pos T.BooleanType env (errors ++ errors_tot))

    staticsemanticAux (RightExpNotEqual sx dx pos ty env errors) =
        let (_, sx_checked, dx_checked, errors_tot) = check_op sx dx pos env T.equality "not equal"
        in (RightExpNotEqual sx_checked dx_checked pos T.BooleanType env (errors ++ errors_tot))

    staticsemanticAux (RightExpPlus sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sup_type, sx_coerc, dx_coerc, errors_tot) = check_op sx dx pos parent_env T.arithmetic "plus"
        in (RightExpPlus sx_coerc dx_coerc pos sup_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpMinus sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sup_type, sx_coerc, dx_coerc, errors_tot) = check_op sx dx pos parent_env T.arithmetic "minus"
        in (RightExpMinus sx_coerc dx_coerc pos sup_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpTimes sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sup_type, sx_coerc, dx_coerc, errors_tot) = check_op sx dx pos parent_env T.arithmetic "times"
        in (RightExpTimes sx_coerc dx_coerc pos sup_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpDivide sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sup_type, sx_coerc, dx_coerc, errors_tot) = check_op sx dx pos parent_env T.arithmetic "divide"
        in (RightExpDivide sx_coerc dx_coerc pos sup_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpMod sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sup_type, sx_coerc, dx_coerc, errors_tot) = check_op sx dx pos parent_env T.arithmetic "mod"
        in (RightExpMod sx_coerc dx_coerc pos sup_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpDiv sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sup_type, sx_coerc, dx_coerc, errors_tot) = check_op sx dx pos parent_env T.arithmetic "div"
        in (RightExpDiv sx_coerc dx_coerc pos sup_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpPower sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sup_type, sx_coerc, dx_coerc, errors_tot) = check_op sx dx pos parent_env T.arithmetic "power"
        in (RightExpPower sx_coerc dx_coerc pos sup_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpNot dx pos ty parent_env errors) =
            -- Faccio questa operazione altrimenti non riesco a ritornare dei messaggi di errore che siano coerenti con quanto scritto
            -- Questo dx_old non servirà ad altro e verrà scartato una volta presi i messaggi d'errore
        let dx_old = staticsemanticAux (dx {right_exp_env = parent_env})
            
            -- Gestione del NOT per il TAC: per evitare di valutare i booleani lazy con not(x op.bool y) nel TAC, eliminamo il not e invertiamo l'operatore
            -- (not ( x  or y )) ----> ((not x) and (not y))
            -- (not ( x and y )) ----> ((not x)  or (not y))
            dx_new = case dx of
                RightExpOr  sx_or  dx_or  p t e er  -> RightExpAnd (apply_not sx_or)  (apply_not dx_or)  p t e er
                RightExpAnd sx_and dx_and p t e er  -> RightExpOr  (apply_not sx_and) (apply_not dx_and) p t e er
                right_exp                           -> right_exp

            -- Analisi di semantica statica su dx_new
            dx_checked = staticsemanticAux (dx_new {right_exp_env = parent_env})
            -- Controllo che dx_new sia booleano
            err_unary_not_permitted = case right_exp_type dx_checked of
                T.BooleanType   -> []
                T.ErrorType     -> []   -- Se dx_checked ha errori, non devo aggiungere un altro errore
                res_type        -> [Err.errMsgUnaryOperationNotPermitted res_type [T.BooleanType] "not" pos]

            -- concateno gli errori
            errors_tot = errors ++ (right_exp_errors dx_old) ++ err_unary_not_permitted

            new_right_exp_checked = case dx_new of
                RightExpOr  _ _ _ _ _ _ -> dx_checked {right_exp_errors = errors_tot}
                RightExpAnd _ _ _ _ _ _ -> dx_checked {right_exp_errors = errors_tot}
                _                       -> RightExpNot dx_checked pos T.BooleanType parent_env errors_tot

        in new_right_exp_checked

    staticsemanticAux (RightExpMinusUnary dx pos ty parent_env errors) =
            -- Controllo che dx sia un math type permesso
        let dx_checked = staticsemanticAux (dx {right_exp_env = parent_env})
            (res_type, err_unary_not_permitted) = case right_exp_type dx_checked of
                T.IntegerType   -> (T.IntegerType, [])
                T.RealType      -> (T.RealType, [])
                T.ErrorType     -> (T.ErrorType, [])    -- Se dx_checked ha errori, non devo aggiungere un altro errore
                _               -> (T.ErrorType, [Err.errMsgUnaryOperationNotPermitted (right_exp_type dx_checked) [T.IntegerType, T.RealType] "minus" pos])
            -- concateno gli errori
            errors_tot = errors ++ (right_exp_errors dx_checked) ++ err_unary_not_permitted
        in (RightExpMinusUnary dx_checked pos res_type parent_env errors_tot)

    staticsemanticAux (RightExpPlusUnary dx pos ty parent_env errors) =
            -- Controllo che dx sia un math type permesso
        let dx_checked = staticsemanticAux (dx {right_exp_env = parent_env})
            (res_type, err_unary_not_permitted) = case right_exp_type dx_checked of
                T.IntegerType   -> (T.IntegerType, [])
                T.RealType      -> (T.RealType, [])
                T.ErrorType     -> (T.ErrorType, [])    -- Se dx_checked ha errori, non devo aggiungere un altro errore
                _               -> (T.ErrorType, [Err.errMsgUnaryOperationNotPermitted (right_exp_type dx_checked) [T.IntegerType, T.RealType] "plus" pos])
            -- concateno gli errori
            errors_tot = errors ++ (right_exp_errors dx_checked) ++ err_unary_not_permitted
        in (RightExpPlusUnary dx_checked pos res_type parent_env errors_tot)

    staticsemanticAux right_exp@(RightExpInteger dx pos ty parent_env errors) = right_exp

    staticsemanticAux right_exp@(RightExpReal dx pos ty parent_env errors) = right_exp

    staticsemanticAux right_exp@(RightExpBoolean dx pos ty parent_env errors) = right_exp

    staticsemanticAux right_exp@(RightExpChar dx pos ty parent_env errors) = right_exp

    staticsemanticAux right_exp@(RightExpString dx pos ty parent_env errors) = right_exp

    staticsemanticAux (RightExpFuncProcCall id params pos ty parent_env errors) =
        -- Controllo che la funzione sia presente nell'env, visto che deve essere già stata dichiarata
        -- Nel caso ci si accorga che la funzione in realtà è una procedura, allora si ritorna un errore:
        --     non avrebbe senso assegnare il valore di ritorno di una procedura, visto che non esiste
        let function_name = id_name id
            params_checked = staticsemanticAux $ map (\param -> param {right_exp_env = parent_env}) params
            errs_params = concat $ map right_exp_errors params_checked

            -- controllo che i tipi corrispondano
            (fun_type, params_coerced, errs_fun) = check_fun_call function_name params_checked pos parent_env
            -- controllo che sia una function
            (fun_type_checked, err_fun_not_proc) = case fun_type of
                Nothing -> (T.ErrorType, [Err.errMsgAssignToProc function_name pos])
                Just t  -> (T.ErrorType, [])

            -- concateno gli errori
            errors_tot = errors ++ errs_params ++ errs_fun ++ err_fun_not_proc

        in (RightExpFuncProcCall (id { ident_env = parent_env }) params_coerced pos fun_type_checked parent_env errors_tot)

    staticsemanticAux (RightExpLeftExp left_exp pos ty parent_env errors) =
            -- Controllo che left_exp sia corretto
        let left_exp_checked = staticsemanticAux (left_exp {left_exp_env = parent_env})
            -- se la left_exp è una costante -> sostituisco la right_exp_copy con un right_exp col corretto valore/tipo
            new_right_exp = case left_exp_checked of
                const@(LeftExpConst _ _ _ _ _ _)    -> from_leftexpconst_to_rightexp const pos parent_env (errors ++ (left_exp_errors left_exp_checked))
                _                                   -> (RightExpLeftExp left_exp_checked pos (left_exp_type left_exp_checked) parent_env (errors ++ (left_exp_errors left_exp_checked)))
        in new_right_exp

    -- staticsemanticAux coerc@(RightExpCoercion main_re from_type to_type pos ty parent_env errors) = coerc
        -- La funzione non viene implementata in questo caso, visto che naturalmente non esisterà mai 
        -- un nodo di questo tipo, ma verrà solo aggiunto da noi artificialmente

instance StaticSemanticClass [RightExp] where
    staticsemanticAux xs = map (staticsemanticAux) xs

instance StaticSemanticClass LeftExp where

    staticsemanticAux (LeftExpIdent id pos ty env errors) = 
            -- Controllo che l'id sia presente nell'env
        let (left_type, left_value, new_env, error_type, env_type) = case E.lookup env (id_name id) of
                Nothing                                                 -> (T.ErrorType, Nothing, env, [Err.errMsgNotDeclared (id_name id) pos], "nothing") -- ritorno errore
                Just (E.VarEntry ty_ret _ _)                            -> (ty_ret, Nothing, env, [], "var")        -- ritorna tipo
                Just (E.ConstEntry ty_ret value _ _)                    -> (ty_ret, Just value, env, [], "const")   -- ritorna tipo e valore
                Just (E.ForIteratorEntry ty_ret _ _)                    -> (ty_ret, Nothing, env, [], "for")        -- ritorna tipo
                -- Posso cambiare qua dentro lo stato di changed, visto che sto sicuramente sovrascrivvendo il valore nell'env SOLO IN CASO DI ASSIGN
                -- Infatti, dalle altre parti in cui viene valutato LeftExpIdent, l'env non viene passato al blocco padre
                Just fun_entry@(E.FunEntry _ ty_ret _ True _ _ _)       -> (ty_ret, Nothing, E.addVar env (id_name id) (fun_entry {E.changed = True}), [], "fun")
                -- Non posso assegnare al nome di una procedura se sono fuori dal suo blocco
                Just fun_entry@(E.FunEntry _ ty_ret _ False _ _ _)      -> (T.ErrorType, Nothing, env, [Err.errMsgReturnOutsideFunction (id_name id) pos], "fun")
                -- Se è altro, non posso usarlo come left_exp
                _                                                       -> (T.ErrorType, Nothing, env, [Err.errAssignToLeftExpr (id_name id) pos], "proc")

            -- concateno gli errori
            errors_tot = errors ++ error_type

            -- aggiorno l'env dell'identificatore
            new_id = id { ident_env = new_env }

            new_leftexp_ident = case (left_value, env_type) of
                (Just val, _)       -> (LeftExpConst new_id val pos left_type new_env errors_tot)   -- costante     -> immutabile
                (Nothing, "for")    -> (LeftExpForIterator new_id pos left_type new_env errors_tot) -- for iterator -> immutabile
                (Nothing, _)        -> (LeftExpIdent new_id pos left_type new_env errors_tot)       -- variabile    -> mutabile
        in new_leftexp_ident

    -- staticsemanticAux for_iter@(LeftExpForIterator id value pos ty env errors) = for_iter
        -- La funzione non viene implementata in questo caso, visto che naturalmente non esisterà mai 
        -- un nodo di questo tipo, ma verrà solo aggiunto da noi artificialmente

    -- staticsemanticAux const@(LeftExpConst id value pos ty env errors) = const
        -- La funzione non viene implementata in questo caso, visto che naturalmente non esisterà mai 
        -- un nodo di questo tipo, ma verrà solo aggiunto da noi artificialmente

    staticsemanticAux (LeftExpArrayAccess array_name array_locations lexp_type pos env errors) =
            -- Controllo che le varie right_exp siano corrette
        let loc_checked = staticsemanticAux $ map (\x -> x {right_exp_env = env}) array_locations
            -- estrapolo tutti gli errori delle right_exp
            array_loc_types_errors = concat $ map right_exp_errors loc_checked
            -- Controllo che la left_exp sia corretta
            lexp_checked = staticsemanticAux array_name {left_exp_env = env} 
            -- controllo che lexp_checked non sia una costante
            err_const = case lexp_checked of
                LeftExpConst id _ _ _ _ _       -> [Err.errMsgCostNotPointArray (id_name id) pos]
                LeftExpForIterator id _ _ _ _   -> [Err.errMsgForIteratorNotPointArray (id_name id) pos]
                _                               -> []

            -- controllo che sia un array e ritorno il tipo, la dimensione e il tipo degli elementi
            -- prendiamo il tipo dell'array per poter fare il controllo sull'assignment
            (arr_type, arr_dim, err_not_array) = case (left_exp_type lexp_checked, (length err_const) == 0) of
                (T.ArrayType ty dim, True)  -> (ty, dim, mkArrTy dim loc_checked pos)       -- ritorno il tipo degli elementi e la dimensione
                (_, True)                   -> (T.ErrorType, [], [Err.errMsgNotArray pos])  -- [] non potrà mai essere una dimensione di un array
                _                           -> (T.ErrorType, [], [])                        -- non voglio errore ridondanti

            -- concateno gli errori
            errors_tot = errors ++ array_loc_types_errors ++ (left_exp_errors lexp_checked) ++ err_const ++ err_not_array

        in (LeftExpArrayAccess lexp_checked loc_checked arr_type pos env errors_tot)

    staticsemanticAux (LeftExpPointerValue left_exp pos left_exp_ty env errors) =
        -- Controllo che la left_exp sia corretta e "porto su" il tipo
        let lexp_checked = staticsemanticAux (left_exp {left_exp_env = env})

            -- controllo che lexp_checked non sia una costante o un for iterator
            (type_aft, err_const) = case lexp_checked of
                LeftExpConst id _ _ _ _ _       -> (T.ErrorType, [Err.errMsgCostNotPointArray (id_name id) pos])
                LeftExpForIterator id _ _ _ _   -> (T.ErrorType, [Err.errMsgForIteratorNotPointArray (id_name id) pos])
                _                               -> (left_exp_type lexp_checked, [])

            -- controllo che type_aft sia un pointer
            (type_extract, err_not_pointer) = case type_aft of
                T.PointerType ty    -> (ty, [])
                T.ErrorType         -> (T.ErrorType, [])    -- non voglio errori ridondanti
                _                   -> (T.ErrorType, [Err.errMsgPointerError "The element is not a pointer " pos])
        in (LeftExpPointerValue lexp_checked pos type_extract env (errors ++ (left_exp_errors lexp_checked) ++ err_const ++ err_not_pointer))

    staticsemanticAux (LeftExpPointerAddress left_exp pos left_exp_ty env errors) =
        -- Controllo che la left_exp sia corretta e "porto su" il tipo
        let lexp_checked = staticsemanticAux (left_exp {left_exp_env = env})

            -- controllo che lexp_checked non sia una costante o un for iterator
            (type_aft, err_const) = case lexp_checked of
                LeftExpConst id _ _ _ _ _       -> (T.ErrorType, [Err.errMsgCostNotPointArray (id_name id) pos])
                LeftExpForIterator id _ _ _ _   -> (T.ErrorType, [Err.errMsgForIteratorNotPointArray (id_name id) pos])
                _                               -> (left_exp_type lexp_checked, [])

            -- incapsulo il tipo in un pointer
            type_encapsulated = T.PointerType type_aft
        in (LeftExpPointerAddress lexp_checked pos type_encapsulated env (errors ++ (left_exp_errors lexp_checked) ++ err_const))

instance StaticSemanticClass Assign where
    staticsemanticAux (VariableAssignment left_exp right_exp pos env errors) =
            -- Controllo che la left_exp sia corretta
        let lexp_checked = staticsemanticAux (left_exp {left_exp_env = env})
            -- controllo che lexp_checked non sia una costante
            (type_aft, err_const) = case lexp_checked of
                LeftExpConst id _ _ _ _ _       -> (T.ErrorType, [Err.errMsgAssignToConst (id_name id) pos])
                LeftExpForIterator id _ _ _ _   -> (T.ErrorType, [Err.errMsgAssignToForIterator (id_name id) pos])
                LeftExpPointerAddress _ _ _ _ _ -> (T.ErrorType, [Err.errMsgPointerError "The left expression is a pointer address" pos])
                _                               -> (left_exp_type lexp_checked, [])
            -- Controllo che la left_exp non sia un array
            err_array = case type_aft of
                T.ArrayType _ _ -> [Err.errAssignToLeftExpr "array" pos]
                _               -> []
            -- Controllo che la right_exp sia corretta
            rexp_checked = staticsemanticAux (right_exp {right_exp_env = env})
            -- Controllo che left_exp e right_exp abbiano lo stesso tipo
            err_different_types = case type_aft == T.ErrorType || right_exp_type rexp_checked == T.ErrorType of
                True    -> []
                False   -> mkIdDeclErrs "The assignment" type_aft (right_exp_type rexp_checked) pos
            -- controllo se right_exp ha bisogno di una coercion
            rexp_checked_coerced = case T.need_coerc type_aft (right_exp_type rexp_checked) of
                True    -> apply_coercion (T.sup type_aft (right_exp_type rexp_checked)) rexp_checked
                False   -> rexp_checked
            -- Concateno gli errori
            errors_tot = errors ++ (left_exp_errors lexp_checked) ++ err_const ++ err_array ++ (right_exp_errors rexp_checked) ++ err_different_types
        in (VariableAssignment lexp_checked rexp_checked_coerced pos (left_exp_env lexp_checked) errors_tot)
    
instance StaticSemanticClass WritePrimitive where
    staticsemanticAux (WriteInt right_exp pos env errors) =
        let (rexp_checked, errs) = check_write_primitive right_exp T.IntegerType pos env
        in (WriteInt rexp_checked pos env (errors ++ errs))  
    
    staticsemanticAux (WriteReal right_exp pos env errors) =
        let (rexp_checked, errs) = check_write_primitive right_exp T.RealType pos env
        in (WriteReal rexp_checked pos env (errors ++ errs))

    staticsemanticAux (WriteChar right_exp pos env errors) =
        let (rexp_checked, errs) = check_write_primitive right_exp T.CharType pos env
        in (WriteChar rexp_checked pos env (errors ++ errs))

    staticsemanticAux (WriteString right_exp pos env errors) =
        let (rexp_checked, errs) = check_write_primitive right_exp T.StringType pos env
        in (WriteString rexp_checked pos env (errors ++ errs))

instance StaticSemanticClass ReadPrimitive where
    staticsemanticAux (ReadInt left_exp pos env errors) =
        let (lexp_checked, errs) = check_read_primitive left_exp (T.PointerType T.IntegerType) pos env
        in (ReadInt lexp_checked pos env (errors ++ errs))

    staticsemanticAux (ReadReal left_exp pos env errors) =
        let (lexp_checked, errs) = check_read_primitive left_exp (T.PointerType T.RealType) pos env
        in (ReadReal lexp_checked pos env (errors ++ errs))

    staticsemanticAux (ReadChar left_exp pos env errors) =
        let (lexp_checked, errs) = check_read_primitive left_exp (T.PointerType T.CharType) pos env
        in (ReadChar lexp_checked pos env (errors ++ errs))
            
    staticsemanticAux (ReadString left_exp pos env errors) =
        let (lexp_checked, errs) = check_read_primitive left_exp (T.PointerType T.StringType) pos env
        in (ReadString lexp_checked pos env (errors ++ errs))
