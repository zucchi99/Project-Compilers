
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

{-
getErrorsFromMaybe :: Maybe T.Type -> [String]
getErrorsFromMaybe (Just (T.ErrorType m)) = m
getErrorsFromMaybe maybe                  = []
-}

checkPresenceStmt :: Ident -> E.Env -> (Int, Int) -> [[Char]]
checkPresenceStmt id env pos = 
    case E.lookup env (id_name id) of
        Just _  -> []
        Nothing -> [Err.errMsgNotDeclared (id_name id) pos]

checkPresenceDecl :: Ident -> E.Env -> (Int, Int) -> [[Char]]
checkPresenceDecl id env pos = 
    case E.lookup env (id_name id) of
        Just _  -> [Err.errMsgAlreadyDeclared (id_name id) pos]
        Nothing -> []

-- check if the function or procedure is already in the environment
-- if it is, check if it's a forward declaration and if the parameters and return type are the same
-- if it's not, add it to the environment
checkPresenceDeclFuncProc :: E.EnvEntry -> String -> E.Env -> (Int, Int) -> (E.Env, [String])
checkPresenceDeclFuncProc new_entry id env pos = 
    case E.lookup env id of
        Just old_entry  -> case E.checkSignature old_entry new_entry id pos of
            []      -> (E.addVar env id new_entry, []) 
            errors  -> (env, errors)
        Nothing         -> (E.addVar env id new_entry, [])

-- mkArrTy(E1.type, E2.type)
-- Controllo di indice dell'array (deve essere int)
mkArrTy :: [(Int,Int)] -> [RightExp] -> (Int, Int) -> [String]
mkArrTy dim locs pos =
    let error_dim = case (length locs) == (length dim) of
            True  -> []
            False -> [ Err.errMsgWrongArrayDim pos ]
        error_types = case all (== T.IntegerType) $ map (right_exp_type) locs of
            True  -> []
            False -> [ Err.errMsgWrongArrayIndex pos ]
    in error_dim ++ error_types

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
mkIfErrs guard_type pos = 
    case guard_type of
        (T.ErrorType)   -> [] -- error already found before, do not add new errors
        (T.BooleanType) -> []
        t               -> [ (Err.errMsgUnexpectedType "Guard" T.BooleanType t pos) ]

-- mkIdDeclErrs(id, E.type, T.type)
-- Controllo durante inizializzazione
-- etype = declaration type, ttype = expression type
mkIdDeclErrs :: String -> T.Type -> T.Type -> (Int, Int) -> [String]
mkIdDeclErrs id etype ttype pos = 
    case (T.sup etype ttype) == T.ErrorType of      -- etype is compatible with ttype?
        True  -> [ Err.errMsgUnexpectedType ("The variable named '" ++ id ++ "'") etype ttype pos ]
        False -> []

-- mkFunEnv(id, F.types, T.type)
-- mkFunEnv(id, τ1 × . . . × τn, τ ) = {id : τ1 × . . . × τn → τ}
-- Nota: La stringa è il nome della funzione, la lista di tipi sono i tipi dei parametri e il tipo finale è il tipo di ritorno
--       Il tutto crea un enviroment con una sola entry (che equivale ad una funzione)
-- mkFunEnv :: String -> [(String, T.Type)] -> T.Type -> Bool -> E.Env
-- mkFunEnv id types returnType forw = E.mkSingletonEnv id E.FunEntry{E.params=types, E.ret=returnType, E.forward = forw}

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

get_sx_dx_errors_right_exp :: RightExp -> RightExp -> E.Env -> (RightExp, RightExp, [String])
get_sx_dx_errors_right_exp sx dx p_env = 
    let new_sx = staticsemanticAux (sx {right_exp_env = p_env})
        new_dx = staticsemanticAux (dx {right_exp_env = p_env})
        errors = (right_exp_errors new_sx) ++ (right_exp_errors new_dx)
    in (new_sx, new_dx, errors)

apply_coercion :: T.Type -> RightExp -> RightExp
apply_coercion to_type main_r = 
    let pos = right_exp_pos main_r
        env = right_exp_env main_r
        errs = right_exp_errors main_r
    in RightExpCoercion main_r (right_exp_type main_r) to_type pos env errs

apply_not ::  RightExp -> RightExp
apply_not main_r = 
    let ty = right_exp_type main_r
        pos = right_exp_pos main_r
        env = right_exp_env main_r
        errs = right_exp_errors main_r
    in RightExpNot main_r pos ty env errs

create_params_for_func_proc :: [Declaration] -> [(String, T.Type)]
create_params_for_func_proc params = map (\decl -> (id_name (variable_name decl), variable_type decl)) params

-- Given the old environment, add or override the break and continue keywords
add_break_continue :: E.Env -> E.Env
add_break_continue env = 
    let env_break = E.mkSingletonEnv "break" (E.ConstEntry T.TBDType)
        env_break_continue = E.addVar env_break "continue" (E.ConstEntry T.TBDType)
    in E.merge env_break_continue env

check_math_op :: RightExp -> RightExp -> (Int, Int) -> E.Env -> String -> (RightExp, RightExp, T.Type, [String])
check_math_op sx dx pos parent_env name_op =
    let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
        sx_type = right_exp_type sx_checked
        dx_type = right_exp_type dx_checked

        (sx_coerc, dx_coerc, math_type, errs_plus_not_permitted) = 
            case T.sup sx_type dx_type of
                    -- Something's wrong. We have to return an ErrorType, because we can't understand where the mistake actaully is
                T.ErrorType -> (sx_checked, dx_checked, T.ErrorType, [Err.errMsgOperationNotPermitted sx_type dx_type name_op pos])
                            -- Se sono tutti dello stesso tipo, allora si può semplicemente ritornare il nodo
                sup_type    |  (T.all_same_type [sup_type, dx_type, sx_type]) -> (sx_checked, dx_checked, sup_type, [])
                            -- Altrimenti, bisogna fare il cast del nodo con il tipo "non T.sup"
                            -- In questo caso il nodo dx è quello che deve essere castato
                            |  sup_type /= dx_type -> (sx_checked, (apply_coercion sup_type dx_checked), sup_type, [])
                            |  otherwise           -> ((apply_coercion sup_type sx_checked), dx_checked, sup_type, [])

        -- concateno gli errori
        errors_tot = merged_errors ++ errs_plus_not_permitted
    in (sx_coerc, dx_coerc, math_type, errors_tot)

need_coerc :: T.Type -> T.Type -> Bool
need_coerc T.RealType T.IntegerType = True
need_coerc _ _ = False


-- keyword can be "break" or "continue"
{- checkLoop :: String -> E.Env -> [String]
checkLoop keyword env
    | E.lookup env keyword == Nothing = []
    | otherwise = [ Err.errMsgWrongLoopControl keyword] -}


-- __________________________ STATIC SEMANTIC ANALISYS
staticsemanticcheck x = case x of
    -- parse successful
    -- (ErrM.Ok a)    -> intercalate "\n\n" $ program_errors $ staticsemanticAux a
    (ErrM.Ok a)    -> staticsemanticAux a
    -- parse error
    (ErrM.Bad err) -> (ProgramStart (Ident "CJOSUL" (-1,-1) E.emptyEnv []) (Block [] [] (-1,-1) E.emptyEnv []) (-1,-1) E.emptyEnv [])

-- __________________________ STATIC SEMANTIC CLASSES
class StaticSemanticClass a where
    staticsemanticAux :: a -> a

instance StaticSemanticClass Program where
    staticsemanticAux (ProgramStart name block pos env errors) =
        -- l'env di block sarà sicuramente vuoto dato che è il primo blocco
        let block_checked = staticsemanticAux block
        in (ProgramStart name block_checked pos env (errors ++ (block_errors block_checked)))

instance StaticSemanticClass Ident where
    staticsemanticAux ident = ident

-- Since [Ident] should exists no more, this instance should be not needed
instance StaticSemanticClass [Ident] where
    staticsemanticAux idents = idents

instance StaticSemanticClass Block where
    staticsemanticAux (Block decls stmts pos env errors) =
            --                               ^^^
            -- env in questo caso è l'enviroment da cui vogliamo iniziare a fare il merge
            -- così facendo è possibile aggiungere le dichiarazioni dei parametri o aggiungere i break e i continue
            -- parto dall'env e aggiungo le dichiarazioni
        let decls_checked = staticsemanticAux decls
            (env_aft_decls, errs_aft_decls) = case decls_checked of
                []  -> (env, [])
                xs  -> (declaration_env (last xs), foldl (\acc x -> acc ++ (declaration_errors x)) [] xs)

            -- controllo che tutte le forward declaration siano state definite -> altrimenti errore
            errs_forw_decls = map (\x -> Err.errMsgNotImplemented x pos) (E.getForward env_aft_decls)
            -- dopo le segno come definite, altrimenti ogni blocco innestato le riconosce come forward e ritorna errore
            env_aft_update = E.updateForward env_aft_decls

            -- una volta finite le dichiarazioni del blocco, faccio il merge con il blocco padre 
            -- conserva le dichiarazioni di ambo i blocchi, ma se ci sono dichiarazioni con lo stesso id tiene quelle del blocco figlio
            -- Ogni statement può modificare l'env, ma non può aggiugnere nuove dichiarazioni
            env_before_stmts = E.merge env_aft_update env
            stmts_checked = staticsemanticAux $ map (\x -> x {statement_env = env_before_stmts}) stmts
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
                Just t  -> (Just t, mkIdDeclErrs (id_name id) t (right_exp_type value_checked) pos)
            -- se l'id è già nell'env -> ritorno errore
            (env_aft_decl, err_aft_decl) = case checkPresenceDecl id env pos of
                []  -> (E.addVar env (id_name id) (E.ConstEntry (fromJust type_aft_decl)), [])
                err -> (env, err)
        in (DeclarationCostant id type_aft_decl value_checked pos env_aft_decl (errors ++ err_type ++ err_aft_decl))

    staticsemanticAux (DeclarationVariable id var_type value_maybe pos env errors) =
            -- se il tipo dichiarato è diverso dal tipo del valore -> ritorno errore
        let (value_checked, err_type) = case value_maybe of
                Nothing -> (Nothing, [])
                Just var_value -> (Just (staticsemanticAux (var_value {right_exp_env = env})), mkIdDeclErrs (id_name id) var_type (right_exp_type var_value) pos)
            -- se l'id è già nell'env -> ritorno errore
            (env_aft_decl, err_aft_decl) = case checkPresenceDecl id env pos of
                [] -> (E.addVar env (id_name id) (E.VarEntry var_type), [])
                err -> (env, err)
        in (DeclarationVariable id var_type value_maybe pos env_aft_decl (errors ++ err_type ++ err_aft_decl))

    staticsemanticAux (DeclarationFunction id params fun_type maybe_block pos env errors) =
            -- create a function entry for the enviroment, with params and return type
        let new_fun_entry = case maybe_block of
                Nothing -> E.FunEntry (create_params_for_func_proc params) fun_type True False True -- forward declaration
                Just _  -> E.FunEntry (create_params_for_func_proc params) fun_type False True False -- function definition

            -- check if is already present in the env a forward declaration for a function
            (env_after_adding_func, err_already_declared) = checkPresenceDeclFuncProc new_fun_entry (id_name id) env pos

            -- parto dall'env vuoto e aggiungo le dichiarazioni
            env_before_params = E.mkSingletonEnv (id_name id) new_fun_entry

            -- aggiungo i parametri alla funzione
            params_checked = staticsemanticAux $ map (\x -> x {declaration_env = env_before_params}) params
            (env_aft_params, errs_aft_params) = case params_checked of
                []  -> (env_before_params, [])
                xs  -> (declaration_env (last xs), foldl (\acc x -> acc ++ (declaration_errors x)) [] xs)

            -- parto dall'env creato dall'ultimo parametro e lo passo al maybe_block se presente
            block = case maybe_block of
                Nothing -> Nothing
                Just b  -> Just (staticsemanticAux (b {block_env = (E.merge env_aft_params env_after_adding_func)}))

            -- mi serve estrarre l'env per il controllo del return
            (env_aft_block, errs_aft_block) = case block of
                Nothing -> (env_after_adding_func, [])
                Just b  -> (block_env b, block_errors b)

            -- controllare che la variabile di ritorno sia modificata
            fun_entry = fromJust (E.lookup env_aft_block (id_name id))
            err_if_return = case fun_entry of
                E.FunEntry _ _ _ _ False -> [Err.errMsgReturnNotSet (id_name id) pos]
                _ -> []

            -- set changed come False, altrimenti la forward declaration permette di non avere il valore di ritorno settato successivamente
            entry_final = case maybe_block of
                Nothing -> fun_entry { E.permit_change = False, E.changed = False } -- forward declaration
                Just _  -> fun_entry { E.permit_change = False, E.changed = True } -- function definition

            -- faccio un override della funzione con gli ultimi 2 bool aggiustati all'env
            env_final = E.addVar env_after_adding_func (id_name id) entry_final

            -- concateno gli errori
            tot_errors = errors ++ err_already_declared ++ errs_aft_params ++ errs_aft_block ++ err_if_return
        in (DeclarationFunction id params_checked fun_type block pos env_final tot_errors)

    staticsemanticAux (DeclarationProcedure id params maybe_block pos env errors) =
            -- create a function entry for the enviroment, with params and return type
        let new_pro_entry = case maybe_block of
                Nothing -> E.ProcEntry (create_params_for_func_proc params) True
                Just _  -> E.ProcEntry (create_params_for_func_proc params) False

            -- check if is already present in the env a forward declaration for a function
            (env_after_adding_proc, err_already_declared) = checkPresenceDeclFuncProc new_pro_entry (id_name id) env pos

            -- parto dall'env vuoto e aggiungo le dichiarazioni
            env_before_params = E.mkSingletonEnv (id_name id) new_pro_entry

            -- aggiungo i parametri alla funzione
            params_checked = staticsemanticAux $ map (\x -> x {declaration_env = env_before_params}) params
            (env_aft_params, errs_aft_params) = case params_checked of
                []  -> (env_before_params, [])
                xs  -> (declaration_env (last xs), foldl (\acc x -> acc ++ (declaration_errors x)) [] xs)

            -- parto dall'env creato dall'ultimo parametro e lo passo al maybe_block se presente
            block = case maybe_block of
                Nothing -> Nothing
                Just b  -> Just (staticsemanticAux (b {block_env = (E.merge env_aft_params env_after_adding_proc)}))
            
            -- non mi serve estrarre l'env, a differenza delle function
            errs_aft_block = case block of
                Nothing -> []
                Just b  -> block_errors b

            -- concateno gli errori
            tot_errors = errors ++ err_already_declared ++ errs_aft_params ++ errs_aft_block
        in (DeclarationProcedure id params_checked block pos env_after_adding_proc tot_errors)

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
        let block_checked = staticsemanticAux (block {block_env = env})
        in (StatementBlock block_checked pos env (errors ++ (block_errors block_checked)))

    staticsemanticAux (StatementIf cond then_body maybe_else_body pos env errors) =
            -- eseguo la condizione
        let cond_checked = staticsemanticAux (cond {right_exp_env = env})
            -- Controllo che la condizione sia booleana
            cond_errors = mkIfErrs (right_exp_type cond_checked) (right_exp_pos cond_checked)
            
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

    staticsemanticAux x@(StatementFor cond then_body var pos env errors) = x
        -- DA SISTEMARE


        --     -- Controllo che la condizione sia booleana, che contenga la variabile var e che sia fattibile
        -- let 
        --     -- Controllo che il then_body sia corretto
        --     -- Controllo che il var sia una variabile Intera, che non venga modificato dentro then_body
        -- in

    staticsemanticAux (StatementWhile cond then_body pos env errors) =
            -- eseguo la condizione
        let cond_checked = staticsemanticAux (cond {right_exp_env = env})
            -- Controllo che la condizione sia booleana
            cond_errors = mkIfErrs (right_exp_type cond_checked) (right_exp_pos cond_checked)
            
            -- Aggiungo le dichiarazioni dei break e dei continue all'enviroment
            env_with_break_continue = add_break_continue env

            -- Controllo che il body sia corretto
            body_checked = staticsemanticAux (then_body {statement_env = env_with_break_continue})
            body_errors = statement_errors body_checked

            errors_tot = errors ++ cond_errors ++ body_errors
        in (StatementWhile cond_checked body_checked pos env errors_tot)

    staticsemanticAux (StatementRepeatUntil cond then_body pos env errors) =
            -- eseguo la condizione
        let cond_checked = staticsemanticAux (cond {right_exp_env = env})
            -- Controllo che la condizione sia booleana
            cond_errors = mkIfErrs (right_exp_type cond_checked) (right_exp_pos cond_checked)
            
            -- Aggiungo le dichiarazioni dei break e dei continue all'enviroment
            env_with_break_continue = add_break_continue env
            
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

            errs_fun = case E.lookup env function_name of
                    -- Non è stato trovato nulla con quel nome nell'env
                Nothing         -> [Err.errMsgNotDeclared function_name pos]
                    -- Tutto ok, la funzione è stata dichiarata e i parametri sono corretti
                Just (E.FunEntry entry_params _ _ _ _)  |   map snd entry_params == map right_exp_type params_checked   -> []
                    -- La funzione è dichiarata nell'enviroment, ma i parametri non corrispondono 
                    -- TODO: si potrebbero mandare messaggi più significativi, tipo se manca un parametro o se c'è un parametro in più, se i tipi sono sbagliati, ...
                                                        |   otherwise                                                   -> [Err.errMsgWrongParams function_name pos]
                    -- Tutto ok, la procedura è stata dichiarata e i parametri sono corretti
                Just (E.ProcEntry entry_params _)       |   map snd entry_params == map right_exp_type params_checked   -> []
                    -- La funzione è dichiarata nell'enviroment, ma i parametri non corrispondono 
                    -- TODO: si potrebbero mandare messaggi più significativi, tipo se manca un parametro o se c'è un parametro in più, se i tipi sono sbagliati, ...
                                                        |   otherwise                                                   -> [Err.errMsgWrongParams function_name pos]
                    -- Si è trovata una variabile o una costante con quel nome (non dovrebbe mai succedere)
                _               -> [Err.errMsgNotFunctionProcedure (id_name id) pos]

            -- concateno gli errori
            errors_tot = errors ++ errs_params ++ errs_fun
        in (StatementFuncProcCall id params_checked pos env errors_tot)

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
    staticsemanticAux (RightExpOr sx dx pos ty parent_env errors) =
            -- Checking if both sx and dx are boolean
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
            errs_rel_not_bool = 
                if T.all_same_type [T.BooleanType, (right_exp_type sx_checked), (right_exp_type dx_checked)]
                    then [] -- Nothing's wrong
                    else [Err.errMsgRelationNotBool (right_exp_type sx_checked) (right_exp_type dx_checked) pos] -- at least one is not boolean
            -- concateno gli errori
            errors_tot = errors ++ merged_errors ++ errs_rel_not_bool
        in (RightExpOr sx_checked dx_checked pos T.BooleanType parent_env errors_tot)
    
    staticsemanticAux (RightExpAnd sx dx pos ty parent_env errors) = 
        -- Checking if both sx and dx are boolean
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
            errs_rel_not_bool = 
                if T.all_same_type [T.BooleanType, (right_exp_type sx_checked), (right_exp_type dx_checked)]
                    then [] -- Nothing's wrong
                    else [Err.errMsgRelationNotBool (right_exp_type sx_checked) (right_exp_type dx_checked) pos] -- at least one is not boolean
            -- concateno gli errori
            errors_tot = errors ++ merged_errors ++ errs_rel_not_bool
        in (RightExpAnd sx_checked dx_checked pos T.BooleanType parent_env errors_tot)

    staticsemanticAux (RightExpGreater sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
            errs_rel_not_bool = 
                case T.rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
                    T.ErrorType -> [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "greater" pos] -- Something's wrong
                    _           -> [] -- Nothing's wrong
            -- concateno gli errori
            errors_tot = errors ++ merged_errors ++ errs_rel_not_bool
        in (RightExpGreater sx_checked dx_checked pos T.BooleanType parent_env errors_tot)
    
    staticsemanticAux (RightExpLess sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
            errs_rel_not_bool = 
                case T.rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
                    T.ErrorType -> [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "less" pos] -- Something's wrong
                    _           -> [] -- Nothing's wrong
            -- concateno gli errori
            errors_tot = errors ++ merged_errors ++ errs_rel_not_bool
        in (RightExpLess sx_checked dx_checked pos T.BooleanType parent_env errors_tot)

    staticsemanticAux (RightExpGreaterEqual sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
            errs_rel_not_bool = 
                case T.rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
                    T.ErrorType -> [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "greater or equal" pos] -- Something's wrong
                    _           -> [] -- Nothing's wrong
            -- concateno gli errori
            errors_tot = errors ++ merged_errors ++ errs_rel_not_bool
        in (RightExpGreaterEqual sx_checked dx_checked pos T.BooleanType parent_env errors_tot)

    staticsemanticAux (RightExpLessEqual sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
            errs_rel_not_bool = 
                case T.rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
                    T.ErrorType -> [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "less or equal" pos] -- Something's wrong
                    _           -> [] -- Nothing's wrong
            -- concateno gli errori
            errors_tot = errors ++ merged_errors ++ errs_rel_not_bool
        in (RightExpLessEqual sx_checked dx_checked pos T.BooleanType parent_env errors_tot)

    staticsemanticAux (RightExpEqual sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
            errs_rel_not_bool = 
                case T.rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
                    T.ErrorType -> [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "equal" pos] -- Something's wrong
                    _           -> [] -- Nothing's wrong
            -- concateno gli errori
            errors_tot = errors ++ merged_errors ++ errs_rel_not_bool
        in (RightExpEqual sx_checked dx_checked pos T.BooleanType parent_env errors_tot)

    staticsemanticAux (RightExpPlus sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_coerc, dx_coerc, math_type, errors_tot) = check_math_op sx dx pos parent_env "plus"
        in (RightExpPlus sx_coerc dx_coerc pos math_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpMinus sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_coerc, dx_coerc, math_type, errors_tot) = check_math_op sx dx pos parent_env "minus"
        in (RightExpMinus sx_coerc dx_coerc pos math_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpTimes sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_coerc, dx_coerc, math_type, errors_tot) = check_math_op sx dx pos parent_env "times"
        in (RightExpTimes sx_coerc dx_coerc pos math_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpDivide sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_coerc, dx_coerc, math_type, errors_tot) = check_math_op sx dx pos parent_env "divide"
        in (RightExpDivide sx_coerc dx_coerc pos math_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpMod sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_coerc, dx_coerc, math_type, errors_tot) = check_math_op sx dx pos parent_env "mod"
        in (RightExpMod sx_coerc dx_coerc pos math_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpDiv sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_coerc, dx_coerc, math_type, errors_tot) = check_math_op sx dx pos parent_env "div"
        in (RightExpDiv sx_coerc dx_coerc pos math_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpPower sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_coerc, dx_coerc, math_type, errors_tot) = check_math_op sx dx pos parent_env "power"
        in (RightExpPower sx_coerc dx_coerc pos math_type parent_env (errors ++ errors_tot))

    staticsemanticAux (RightExpNot dx pos ty parent_env errors) =
            -- Controllo che dx sia booleano
        let dx_checked = staticsemanticAux (dx {right_exp_env = parent_env})
            err_unary_not_permitted = case right_exp_type dx_checked of
                T.BooleanType   -> []
                res_type        -> [Err.errMsgUnaryOperationNotPermitted res_type [T.BooleanType] "not" pos]
            -- concateno gli errori
            errors_tot = errors ++ (right_exp_errors dx_checked) ++ err_unary_not_permitted
            -- Gestione del NOT per il TAC: per evitare di valutare i booleani lazy con not(x op.bool y) nel TAC, eliminamo il not e invertiamo l'operatore
            -- (not ( x or y ))  ----> ((not x) and (not y))
            -- (not ( x and y )) ----> ((not x)  or (not y))
            right_exp_to_return = case dx_checked of
                RightExpOr  sx_or  dx_or  _ _ _ _     -> RightExpAnd (apply_not sx_or) (apply_not dx_or) pos T.BooleanType parent_env errors
                RightExpAnd sx_and dx_and _ _ _ _     -> RightExpOr  (apply_not sx_and) (apply_not dx_and) pos T.BooleanType parent_env errors
                _                                     -> (RightExpNot dx_checked pos T.BooleanType parent_env errors_tot)
        in right_exp_to_return

    staticsemanticAux (RightExpMinusUnary dx pos ty parent_env errors) =
            -- Controllo che dx sia un math type permesso
        let dx_checked = staticsemanticAux (dx {right_exp_env = parent_env})

            (res_type, err_unary_not_permitted) = case right_exp_type dx_checked of
                T.ErrorType -> (T.ErrorType, [Err.errMsgUnaryOperationNotPermitted (right_exp_type dx_checked) [T.IntegerType, T.RealType] "minus" pos])
                res_type    -> (res_type, [])
            -- concateno gli errori
            errors_tot = errors ++ (right_exp_errors dx_checked) ++ err_unary_not_permitted
        in (RightExpMinusUnary dx_checked pos res_type parent_env errors_tot)

    staticsemanticAux (RightExpPlusUnary dx pos ty parent_env errors) =
            -- Controllo che dx sia un math type permesso
        let dx_checked = staticsemanticAux (dx {right_exp_env = parent_env})

            (res_type, err_unary_not_permitted) = case right_exp_type dx_checked of
                T.ErrorType -> (T.ErrorType, [Err.errMsgUnaryOperationNotPermitted (right_exp_type dx_checked) [T.IntegerType, T.RealType] "plus" pos])
                res_type    -> (res_type, [])
            -- concateno gli errori
            errors_tot = errors ++ (right_exp_errors dx_checked) ++ err_unary_not_permitted
        in (RightExpPlusUnary dx_checked pos res_type parent_env errors_tot)

    -- Tutto viene già inizializzato da Parser.y (valore, tipo, env, ...)
    staticsemanticAux right_exp@(RightExpInteger dx pos ty parent_env errors) = right_exp

    -- Tutto viene già inizializzato da Parser.y (valore, tipo, env, ...)
    staticsemanticAux right_exp@(RightExpReal dx pos ty parent_env errors) = right_exp

    -- Tutto viene già inizializzato da Parser.y (valore, tipo, env, ...)
    staticsemanticAux right_exp@(RightExpBoolean dx pos ty parent_env errors) = right_exp

    -- Tutto viene già inizializzato da Parser.y (valore, tipo, env, ...)
    staticsemanticAux right_exp@(RightExpChar dx pos ty parent_env errors) = right_exp

    -- Tutto viene già inizializzato da Parser.y (valore, tipo, env, ...)
    staticsemanticAux right_exp@(RightExpString dx pos ty parent_env errors) = right_exp

    staticsemanticAux (RightExpFuncProcCall id params pos ty parent_env errors) =
        -- Controllo che la funzione sia presente nell'env, visto che deve essere già stata dichiarata
        -- Nel caso ci si accorga che la funzione in realtà è una procedura, allora si ritorna un errore:
        --     non avrebbe senso assegnare il valore di ritorno di una procedura, visto che non esiste
        let function_name = id_name id
            params_checked = staticsemanticAux $ map (\param -> param {right_exp_env = parent_env}) params
            errs_params = concat $ map right_exp_errors params_checked

            (checked_type, errs_fun) = case E.lookup parent_env function_name of
                    -- Non è stato trovato nulla con quel nome nell'env
                Nothing                                     -> (T.ErrorType, [Err.errMsgNotDeclared function_name pos])
                    -- Tutto ok, la funzione è stata dichiarata e i parametri sono corretti
                Just (E.FunEntry entry_params ty_ret _ _ _) |   map snd entry_params == map right_exp_type params_checked   -> (ty_ret, [])
                    -- La funzione è dichiarata nell'enviroment, ma i parametri non corrispondono 
                    -- TODO: si potrebbero mandare messaggi più significativi, tipo se manca un parametro o se c'è un parametro in più, se i tipi sono sbagliati, ...
                                                            |   otherwise                                                   -> (ty_ret, [Err.errMsgWrongParams function_name pos])
                    -- La funzione è dichiarata nell'enviroment, ma è una procedura
                Just (E.ProcEntry _ _)                      -> (T.ErrorType, [Err.errMsgAssignToProc function_name pos])
                    -- Si è trovata una variabile o una costante con quel nome (non dovrebbe mai succedere)
                _                                           -> (T.ErrorType, [Err.errMsgNotFunctionProcedure (id_name id) pos])

            -- concateno gli errori
            errors_tot = errors ++ errs_params ++ errs_fun
        in (RightExpFuncProcCall id params_checked pos checked_type parent_env errors_tot)

    staticsemanticAux (RightExpCopy left_exp pos ty parent_env errors) =
            -- Controllo che left_exp sia corretto
        let left_exp_checked = staticsemanticAux (left_exp {left_exp_env = parent_env})
        in (RightExpCopy left_exp_checked pos (left_exp_type left_exp_checked) parent_env (errors ++ (left_exp_errors left_exp_checked)))

    -- staticsemanticAux coerc@(RightExpCoercion main_re from_type to_type pos ty parent_env errors) = coerc
    -- La funzione non viene implementata in questo caso, visto che naturalmente non esisterà mai 
    -- un nodo di questo tipo, ma verrà solo aggiunto da noi artificialmente

instance StaticSemanticClass [RightExp] where
    staticsemanticAux xs = map (staticsemanticAux) xs

instance StaticSemanticClass LeftExp where
    -- Controllo che l'id sia presente nell'env e che sia una variabile (altrimenti non avrebbe senso assegnare qualcosa)
    staticsemanticAux (LeftExpIdent id pos ty env errors) = 
            -- Controllo che l'id sia presente nell'env
        let (left_type, env_type, error_type) = case E.lookup env (id_name id) of
                Nothing                     -> (T.ErrorType, env, [Err.errMsgNotDeclared id pos])
                Just (E.VarEntry ty_ret)    -> (ty_ret, env, [])
                Just (E.ConstEntry ty_ret)  -> (ty_ret, env, [])
                -- Posso cambiare qua dentro lo stato di changed, visto che sto sicuramente sovrascrivvendo il valore nell'env SOLO IN CASO DI ASSIGN
                -- Infatti, dalle altre parti in cui viene valutato LeftExpIdent, l'env non viene passato al blocco padre
                Just fun_entry@(E.FunEntry _ ty_ret _ True _)    -> (ty_ret, E.addVar env (id_name id) (fun_entry {E.changed = True}), [])
                -- Non posso assegnare al nome di una procedura se sono fuori dal suo blocco
                Just fun_entry@(E.FunEntry _ ty_ret _ False _)   -> (T.ErrorType, env, [Err.errMsgReturnOutsideFunction (id_name id) pos])
                -- Se è una costante o una procedure, non posso assegnare nulla
                _                           -> (T.ErrorType, env, [Err.errAssignToLeftExpr id pos])

            -- concateno gli errori
            errors_tot = errors ++ error_type
        in (LeftExpIdent id pos left_type env_type errors_tot)

    staticsemanticAux (LeftExpArrayAccess array_name array_locations lexp_type pos env errors) =
            -- Controllo che le varie right_exp siano corrette
        let loc_checked = staticsemanticAux $ map (\x -> x {right_exp_env = env}) array_locations
            -- estrapolo tutti gli errori delle right_exp
            array_loc_types_errors = concat $ map right_exp_errors loc_checked
            
            -- Controllo che la left_exp sia corretta
            lexp_checked = staticsemanticAux array_name {left_exp_env = env} 
            -- controllo che sia un array e ritorno il tipo, la dimensione e il tipo degli elementi
            -- prendiamo il tipo dell'array per poter fare il controllo sull'assignment
            (arr_type, arr_dim, err_not_array) = case left_exp_type lexp_checked of
                T.ArrayType ty dim -> (ty, dim, mkArrTy dim loc_checked pos)
                -- [] non potrà mai essere una dimensione di un array
                _                  -> (T.ErrorType, [], [Err.errMsgNotArray pos])

            -- concateno gli errori
            errors_tot = errors ++ array_loc_types_errors ++ err_not_array

        in (LeftExpArrayAccess lexp_checked loc_checked arr_type pos env errors_tot)

    staticsemanticAux (LeftExpPointerValue left_exp pos left_exp_ty env errors) =
        -- Controllo che la left_exp sia corretta e "porto su" il tipo
        let lexp_checked = staticsemanticAux (left_exp {left_exp_env = env})
        in (LeftExpPointerValue lexp_checked pos (left_exp_type lexp_checked) env (errors ++ (left_exp_errors lexp_checked)))

    staticsemanticAux (LeftExpPointerAddress left_exp pos left_exp_ty env errors) =
        -- Controllo che la left_exp sia corretta e "porto su" il tipo
        let (lexp_checked) = staticsemanticAux left_exp {left_exp_env = env}
        in (LeftExpPointerAddress lexp_checked pos (left_exp_type lexp_checked) env (errors ++ (left_exp_errors lexp_checked)))

instance StaticSemanticClass Assign where
    staticsemanticAux (VariableAssignment left_exp right_exp pos env errors) =
            -- Controllo che la left_exp sia corretta
        let lexp_checked = staticsemanticAux (left_exp {left_exp_env = env})
            -- controllo che la lexp non sia una costante
            -- COME POSSO FARE A SAPERE SE LA LEXP È UNA COSTANTE??

            -- Controllo che la right_exp sia corretta
            rexp_checked = staticsemanticAux (right_exp {right_exp_env = env})
            -- Controllo che left_exp e right_exp abbiano lo stesso tipo
            err_different_types = mkAssignErrs (left_exp_type lexp_checked) (right_exp_type rexp_checked) pos
            -- controllo se right_exp ha bisogno di una coercion
            rexp_checked_coerced = case need_coerc (left_exp_type lexp_checked) (right_exp_type rexp_checked) of
                True    -> apply_coercion (T.sup (left_exp_type lexp_checked) (right_exp_type rexp_checked)) rexp_checked
                False   -> rexp_checked
            -- Concateno gli errori
            errors_tot = errors ++ (left_exp_errors lexp_checked) ++ (right_exp_errors rexp_checked) ++ err_different_types
        in (VariableAssignment lexp_checked rexp_checked_coerced pos (left_exp_env lexp_checked) errors_tot)
    
instance StaticSemanticClass WritePrimitive where
    staticsemanticAux (WriteInt right_exp pos env errors) =
        let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            (r_type, r_err) = (right_exp_type rexp_checked, right_exp_errors rexp_checked)
            -- Controllo che right_exp sia un intero
            err_write = case r_type of
                T.IntegerType -> []
                _ -> [Err.errMsgWrongWritePrimitiveType T.IntegerType r_type pos]
            -- concateno gli errori
            errors_tot = errors ++ r_err ++ err_write

        in (WriteInt rexp_checked pos env errors_tot)        
    
    staticsemanticAux (WriteReal right_exp pos env errors) =
        let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            (r_type, r_err) = (right_exp_type rexp_checked, right_exp_errors rexp_checked)
            -- Controllo che right_exp sia un real
            err_write = case r_type of
                T.RealType -> []
                _ -> [Err.errMsgWrongWritePrimitiveType T.RealType r_type pos]
            -- concateno gli errori
            errors_tot = errors ++ r_err ++ err_write
            
        in (WriteReal rexp_checked pos env errors_tot)

    staticsemanticAux (WriteChar right_exp pos env errors) =
        let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            (r_type, r_err) = (right_exp_type rexp_checked, right_exp_errors rexp_checked)
            -- Controllo che right_exp sia un real
            err_write = case r_type of
                T.CharType -> []
                _ -> [Err.errMsgWrongWritePrimitiveType T.CharType r_type pos]
            -- concateno gli errori
            errors_tot = errors ++ r_err ++ err_write
            
        in (WriteChar rexp_checked pos env errors_tot)

    staticsemanticAux (WriteString right_exp pos env errors) =
        let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            (r_type, r_err) = (right_exp_type rexp_checked, right_exp_errors rexp_checked)
            -- Controllo che right_exp sia un real
            err_write = case r_type of
                T.StringType -> []
                _ -> [Err.errMsgWrongWritePrimitiveType T.StringType r_type pos]
            -- concateno gli errori
            errors_tot = errors ++ r_err ++ err_write
            
        in (WriteString rexp_checked pos env errors_tot)

instance StaticSemanticClass ReadPrimitive where
    staticsemanticAux (ReadInt left_exp pos env errors) =
        let lexp_checked = staticsemanticAux left_exp {left_exp_env = env}
            (l_type, l_err) = (left_exp_type lexp_checked, left_exp_errors lexp_checked)
            -- Controllo che right_exp sia un real
            err_read = case l_type of
                T.IntegerType -> []
                _ -> [Err.errMsgWrongReadPrimitiveType T.IntegerType l_type pos]
            -- concateno gli errori
            errors_tot = errors ++ l_err ++ err_read
            
        in (ReadInt lexp_checked pos env errors_tot)

    staticsemanticAux (ReadReal left_exp pos env errors) =
        let lexp_checked = staticsemanticAux left_exp {left_exp_env = env}
            (l_type, l_err) = (left_exp_type lexp_checked, left_exp_errors lexp_checked)
            -- Controllo che right_exp sia un real
            err_read = case l_type of
                T.RealType -> []
                _ -> [Err.errMsgWrongReadPrimitiveType T.RealType l_type pos]
            -- concateno gli errori
            errors_tot = errors ++ l_err ++ err_read
            
        in (ReadReal lexp_checked pos env errors_tot)

    staticsemanticAux (ReadChar left_exp pos env errors) =
        let lexp_checked = staticsemanticAux left_exp {left_exp_env = env}
            (l_type, l_err) = (left_exp_type lexp_checked, left_exp_errors lexp_checked)
            -- Controllo che right_exp sia un real
            err_read = case l_type of
                T.CharType -> []
                _ -> [Err.errMsgWrongReadPrimitiveType T.CharType l_type pos]
            -- concateno gli errori
            errors_tot = errors ++ l_err ++ err_read
            
        in (ReadChar lexp_checked pos env errors_tot)
            
    staticsemanticAux (ReadString left_exp pos env errors) =
        let lexp_checked = staticsemanticAux left_exp {left_exp_env = env}
            (l_type, l_err) = (left_exp_type lexp_checked, left_exp_errors lexp_checked)
            -- Controllo che right_exp sia un real
            err_read = case l_type of
                T.StringType -> []
                _ -> [Err.errMsgWrongReadPrimitiveType T.StringType l_type pos]
            -- concateno gli errori
            errors_tot = errors ++ l_err ++ err_read
            
        in (ReadString lexp_checked pos env errors_tot)
