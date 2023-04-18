
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

checkPresenceStmt id env pos = 
    case E.lookup env (id_name id) of
        Just _  -> []
        Nothing -> [Err.errMsgNotDeclared (id_name id) pos]


checkPresenceDecl id env pos = 
    case E.lookup env (id_name id) of
        Just _  -> [Err.errMsgAlreadyDeclared (id_name id) pos]
        Nothing -> []

checkPresenceDeclFunc id env pos =
    case E.lookup env (id_name id) of
        Just fun@(E.FunEntry _ _ True)  -> (Just fun, [])
        Just _                          -> (Nothing, [Err.errMsgAlreadyDeclared (id_name id) pos])
        Nothing                         -> (Nothing, [])

checkPresenceDeclProc id env pos =
    case E.lookup env (id_name id) of
        Just pro@(E.ProcEntry _ True)   -> (Just pro, [])
        Just _                          -> (Nothing, [Err.errMsgAlreadyDeclared (id_name id) pos])
        Nothing                         -> (Nothing, [])

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
mkFunEnv :: String -> [(String, T.Type)] -> T.Type -> Bool -> E.Env
mkFunEnv id types returnType forw = E.mkSingletonEnv id E.FunEntry{E.params=types, E.ret=returnType, E.forward = forw}

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


-- __________________________ STATIC SEMANTIC ANAL-ISYS
staticsemanticcheck x = case x of
    -- parse successful
    (ErrM.Ok a)    -> intercalate "\n\n" $ program_errors $ staticsemanticAux a
    -- parse error
    (ErrM.Bad err) -> err

-- __________________________ STATIC SEMANTIC CLASSES
class StaticSemanticClass a where
    staticsemanticAux :: a -> a

instance StaticSemanticClass Program where
    staticsemanticAux (ProgramStart name block pos env errors) =
        -- l'env di block sarà sicuramente vuoto dato che è il primo blocco
        let blockAft@(Block decls stmts posBlk envBlk errorsBlk) = staticsemanticAux block
        in (ProgramStart name blockAft pos env (errors ++ errorsBlk))

instance StaticSemanticClass Ident where
    staticsemanticAux x@(Ident name pos env errors) = x

-- Since [Ident] should exists no more, this instance should be not needed
--instance StaticSemanticClass [Ident] where
  --  staticsemanticAux env idents = (env, [])

instance StaticSemanticClass Block where
    staticsemanticAux (Block decls stmts pos env errors) = 
            -- parto dall'env vuoto e aggiungo le dichiarazioni
            -- (Inizialmente tutti gli env sono vuoti, quindi non serve fornire l'env iniziale)
        let decls_aft_decl = staticsemanticAux decls
            last_decl = last decls_aft_decl
            (env_aft_decls, errs_aft_decls) = (declaration_env last_decl, declaration_errors last_decl)
            -- controllo che tutte le forward declaration siano state definite -> altrimenti errore
            errs_forw_decls = map (\x -> Err.errMsgNotImplemented x pos) (E.getForward env_aft_decls)

            -- una volta finite le dichiarazioni del blocco, faccio il merge con il blocco padre 
            -- conserva le dichiarazioni di ambo i blocchi, ma se ci sono dichiarazioni con lo stesso id tiene quelle del blocco figlio
            -- Ogni statement può modificare l'env, ma non può aggiugnere nuove dichiarazioni
            first_stmt = head stmts
            stmts_aft_decl = staticsemanticAux (first_stmt {statement_env = (E.merge env_aft_decls env)} : tail stmts)
            last_stmt = last stmts_aft_decl
            (env_aft_stmts, errs_aft_stmts) = (statement_env last_stmt, statement_errors last_stmt)

            -- concat all errors
            tot_errors = errors ++ errs_aft_decls ++ errs_forw_decls ++ errs_aft_stmts

        in (Block decls stmts pos env_aft_stmts tot_errors)

instance StaticSemanticClass Declaration where
    staticsemanticAux (DeclarationCostant id maybe_type value pos env errors) = 
            -- se il tipo dichiarato è diverso dal tipo del valore -> ritorno errore
        let (type_aft_decl, err_type) = case maybe_type of
                Nothing -> (Just (right_exp_type value), [])
                Just t -> (Just t, mkIdDeclErrs (id_name id) t (right_exp_type value) pos)
            -- se l'id è già nell'env -> ritorno errore
            (env_aft_decl, err_aft_decl) = case checkPresenceDecl id env pos of
                [] -> (E.addVar env (id_name id) (E.ConstEntry (fromJust type_aft_decl)), [])
                err -> (env, err)
        in (DeclarationCostant id type_aft_decl value pos env_aft_decl (errors ++ err_type ++ err_aft_decl))

    staticsemanticAux (DeclarationVariable id var_type value_maybe pos env errors) =
            -- se il tipo dichiarato è diverso dal tipo del valore -> ritorno errore
        let err_type = case value_maybe of
                Nothing -> []
                Just var_value -> mkIdDeclErrs (id_name id) var_type (right_exp_type var_value) pos
            -- se l'id è già nell'env -> ritorno errore
            (env_aft_decl, err_aft_decl) = case checkPresenceDecl id env pos of
                [] -> (E.addVar env (id_name id) (E.VarEntry var_type), [])
                err -> (env, err)
        in (DeclarationVariable id var_type value_maybe pos env_aft_decl (errors ++ err_type ++ err_aft_decl))

    staticsemanticAux (DeclarationFunction id params fun_type maybe_block pos env errors) =
            -- check if is already present in the env a forward declaration for a function
        let (fun_in_env, err_already_declared) = checkPresenceDeclFunc id env pos

            -- parto dall'env vuoto e aggiungo le dichiarazioni
            -- (Inizialmente tutti gli env sono vuoti, quindi non serve fornire l'env iniziale)
            decls_aft_params = staticsemanticAux params
            last_params = last decls_aft_params
            (env_aft_params, errs_aft_params) = (declaration_env last_params, declaration_errors last_params)

            -- extract the name and the type of the parameters
            -- by the Parser.y the params should always be DeclarationVariable and the value should always be Nothing
            params_type = (map (\x -> (id_name (variable_name x), variable_type x)) decls_aft_params)

            -- create an EnvEntry for the function, check if it's a forward declaration or not
            this_fun = case maybe_block of
                -- inside params there are only DeclarationVariable
                Nothing     -> E.FunEntry params_type fun_type True
                Just block  -> E.FunEntry params_type fun_type False

            -- check if the function has the same type of the return or the same type of the parameters already declared
            err_check_equal = case fun_in_env of
                Just fun_in_env_ext@(E.FunEntry params fun_type True) -> case fun_in_env_ext == this_fun of
                    True -> []
                    False -> [Err.errMsgWrongFunctionType id pos]
                _ -> []

            -- combine various errors
            err_before_decl = err_already_declared ++ err_check_equal

            -- if there are no errors, add the function to the env
            (env_aft_decl, err_aft_decl) = case err_before_decl of
                [] -> (E.addVar env (id_name id) this_fun, [])
                err -> (env, err)

            -- nel blocco fare la merge coi parametri, la vecchia env e break, continue, ...
            -- controllare che il tipo di ritorno sia compatibile con il tipo dichiarato (la variabile di ritorno si chiama come la funzione)

        in (DeclarationFunction id params fun_type maybe_block pos env_aft_decl (errors ++ errs_aft_params))


    staticsemanticAux x@(DeclarationProcedure id params maybe_block pos env errros) = x
        -- In base a block:
        --      Nothing -> È una forward declaration, per cui controllo che non ci sia già nell'env.
        --      Just block -> Controllo che non ci sia nell'env o che sia presente la sua forward declaration

instance StaticSemanticClass [Declaration] where
    -- Parto dall'enviroment passato
    -- Ogni enviroment parte dal precedente
    -- Restituisco l'enviroment finale e gli errori concatenati in ordine di ritrovamento
    staticsemanticAux decls = decls
        
        -- foldl (addDeclaration) (env, []) decls where
        -- addDeclaration (env, errs) decl = 
        --     let (envAfter, errsAfter) = staticsemanticAux env decl
        --     in (envAfter, errs ++ errsAfter)

instance StaticSemanticClass [Statement] where
    staticsemanticAux stmts = stmts

instance StaticSemanticClass Statement where
    staticsemanticAux x@(StatementBlock block pos env errors) = x
    staticsemanticAux x@(StatementIf cond then_body maybe_else_body pos env errors) = x
        -- Controllo che la condizione sia booleana
        -- Controllo che il then_body sia corretto
        -- Controllo che il else_body sia corretto
    staticsemanticAux x@(StatementFor cond then_body var pos env errors) = x
        -- Controllo che la condizione sia booleana, che contenga la variabile var e che sia fattibile
        -- Controllo che il then_body sia corretto
        -- Controllo che il var sia una variabile Intera, che non venga modificato dentro then_body
    staticsemanticAux x@(StatementWhile cond then_body pos env errors) = x
        -- Controllo che la condizione sia booleana
        -- Controllo che il then_body sia corretto
    staticsemanticAux x@(StatementRepeatUntil cond then_body pos env errors) = x
        -- Controllo che la condizione sia booleana
        -- Controllo che il then_body sia corretto
    staticsemanticAux x@(StatementAssign assign pos env errors) = x
        -- Controllo che l'assegnamento sia corretto
    staticsemanticAux x@(StatementFuncProcCall id params pos env errors) = x
        -- Controllo che la funzione sia presente nell'env
        -- Controllo che i parametri corrispondano al tipo dei parametri voluti della funzione
    staticsemanticAux x@(StatementWrite write_primitive pos env errors) = x
        -- Controllo che il write_primitive sia corretto
    staticsemanticAux x@(StatementRead read_primitive pos env errors) = x
        -- Controllo che il read_primitive sia corretto
    staticsemanticAux x@(StatementContinue pos env errors) = x
        -- Controllo che ci sia il continue nell'env
    staticsemanticAux x@(StatementBreak pos env errors) = x
        -- Controllo che ci sia il break nell'env

instance StaticSemanticClass ElseBlock where
    staticsemanticAux x@(ElseBlock else_body pos env errors) = x

get_sx_dx_errors_right_exp :: RightExp -> RightExp -> E.Env -> (RightExp, RightExp, [String])
get_sx_dx_errors_right_exp sx dx p_env = 
    let new_sx = staticsemanticAux (sx {right_exp_env = p_env})
        new_dx = staticsemanticAux (dx {right_exp_env = p_env})
    in (new_sx, new_dx, merge_errors_right_exp new_sx new_dx)

merge_errors_right_exp :: RightExp -> RightExp -> [String]
merge_errors_right_exp sx dx = (right_exp_errors sx) ++ (right_exp_errors dx)

apply_coercion :: T.Type -> RightExp -> RightExp
apply_coercion to_type main_r = 
    let pos = right_exp_pos main_r
        env = right_exp_env main_r
        errs = right_exp_errors main_r
    in RightExpCoercion main_r (right_exp_type main_r) to_type pos env errs


instance StaticSemanticClass RightExp where
    staticsemanticAux (RightExpOr sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are boolean
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
        in if T.all_same_type [T.BooleanType, (right_exp_type sx_checked), (right_exp_type dx_checked)]
            -- Nothing's wrong
            then RightExpOr sx_checked dx_checked pos T.BooleanType parent_env errors
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            else RightExpOr sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgRelationNotBool (right_exp_type sx_checked) (right_exp_type dx_checked) pos])
    
    staticsemanticAux (RightExpAnd sx dx pos ty parent_env errors) = 
        -- Checking if both sx and dx are boolean
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env   
        in if T.all_same_type [T.BooleanType, (right_exp_type sx_checked), (right_exp_type dx_checked)]
            -- Nothing's wrong
            then RightExpAnd sx_checked dx_checked pos T.BooleanType parent_env errors
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            else RightExpAnd sx_checked dx_checked pos T.BooleanType parent_env (errors ++ [Err.errMsgRelationNotBool (right_exp_type sx_checked) (right_exp_type dx_checked) pos])
    
    staticsemanticAux (RightExpGreater sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env    
        in case T.rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            T.ErrorType -> RightExpGreater sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "greater" pos])
            -- Nothing's wrong
            _           -> RightExpGreater sx_checked dx_checked pos T.BooleanType parent_env errors
    
    staticsemanticAux (RightExpLess sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
        in case T.rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            T.ErrorType -> RightExpLess sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "less" pos])
            -- Nothing's wrong
            _           -> RightExpLess sx_checked dx_checked pos T.BooleanType parent_env errors

    staticsemanticAux (RightExpGreaterEqual sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
        in case T.rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            T.ErrorType -> RightExpGreaterEqual sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "'greater or equal'" pos])
            -- Nothing's wrong
            _           -> RightExpGreaterEqual sx_checked dx_checked pos T.BooleanType parent_env errors

    staticsemanticAux (RightExpLessEqual sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
        in case T.rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            T.ErrorType -> RightExpLessEqual sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "'less or equal'" pos])
            -- Nothing's wrong
            _           -> RightExpLessEqual sx_checked dx_checked pos T.BooleanType parent_env errors

    staticsemanticAux (RightExpEqual sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
        
        in case T.rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            T.ErrorType -> RightExpEqual sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "equal" pos])
            -- Nothing's wrong
            _           -> RightExpEqual sx_checked dx_checked pos T.BooleanType parent_env errors

    staticsemanticAux (RightExpPlus sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
            sx_type = right_exp_type sx_checked
            dx_type = right_exp_type dx_checked
        
        in case T.sup sx_type dx_type of
            -- Something's wrong. We have to return an ErrorType, because we can't understand where the mistake actaully is
            T.ErrorType -> RightExpPlus sx_checked dx_checked pos T.ErrorType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted sx_type dx_type "plus" pos])
                        -- Se sono tutti dello stesso tipo, allora si può semplicemente ritornare il nodo
            sup_type    |  (T.all_same_type [sup_type, dx_type, sx_type]) -> RightExpPlus sx_checked dx_checked pos sup_type parent_env (errors ++ merged_errors)
                        -- Altrimenti, bisogna fare il cast del nodo con il tipo "non T.sup"
                        -- In questo caso il nodo dx è quello che deve essere castato
                        |  sup_type /= sx_type -> RightExpPlus sx_checked (apply_coercion sup_type dx_checked) pos sup_type parent_env (errors ++ merged_errors)
                        |  otherwise           -> RightExpPlus (apply_coercion sup_type sx_checked) dx_checked pos sup_type parent_env (errors ++ merged_errors)

    staticsemanticAux (RightExpMinus sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
            sx_type = right_exp_type sx_checked
            dx_type = right_exp_type dx_checked
        
        in case T.sup sx_type dx_type of
            -- Something's wrong. We have to return an ErrorType, because we can't understand where the mistake actaully is
            T.ErrorType -> RightExpMinus sx_checked dx_checked pos T.ErrorType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted sx_type dx_type "minus" pos])
                        -- Se sono tutti dello stesso tipo, allora si può semplicemente ritornare il nodo
            sup_type    |  (T.all_same_type [sup_type, dx_type, sx_type]) -> RightExpMinus sx_checked dx_checked pos sup_type parent_env (errors ++ merged_errors)
                        -- Altrimenti, bisogna fare il cast del nodo con il tipo "non T.sup"
                        -- In questo caso il nodo dx è quello che deve essere castato
                        |  sup_type /= sx_type -> RightExpMinus sx_checked (apply_coercion sup_type dx_checked) pos sup_type parent_env (errors ++ merged_errors)
                        |  otherwise           -> RightExpMinus (apply_coercion sup_type sx_checked) dx_checked pos sup_type parent_env (errors ++ merged_errors)

    staticsemanticAux (RightExpTimes sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
            sx_type = right_exp_type sx_checked
            dx_type = right_exp_type dx_checked
        
        in case T.sup sx_type dx_type of
            -- Something's wrong. We have to return an ErrorType, because we can't understand where the mistake actaully is
            T.ErrorType -> RightExpTimes sx_checked dx_checked pos T.ErrorType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted sx_type dx_type "minus" pos])
                        -- Se sono tutti dello stesso tipo, allora si può semplicemente ritornare il nodo
            sup_type    |  (T.all_same_type [sup_type, dx_type, sx_type]) -> RightExpTimes sx_checked dx_checked pos sup_type parent_env (errors ++ merged_errors)
                        -- Altrimenti, bisogna fare il cast del nodo con il tipo "non T.sup"
                        -- In questo caso il nodo dx è quello che deve essere castato
                        |  sup_type /= sx_type -> RightExpTimes sx_checked (apply_coercion sup_type dx_checked) pos sup_type parent_env (errors ++ merged_errors)
                        |  otherwise           -> RightExpTimes (apply_coercion sup_type sx_checked) dx_checked pos sup_type parent_env (errors ++ merged_errors)

    staticsemanticAux (RightExpDivide sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
            sx_type = right_exp_type sx_checked
            dx_type = right_exp_type dx_checked
        
        in case T.sup sx_type dx_type of
            -- Something's wrong. We have to return an ErrorType, because we can't understand where the mistake actaully is
            T.ErrorType -> RightExpDivide sx_checked dx_checked pos T.ErrorType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted sx_type dx_type "minus" pos])
                        -- Se sono tutti dello stesso tipo, allora si può semplicemente ritornare il nodo
            sup_type    |  (T.all_same_type [sup_type, dx_type, sx_type]) -> RightExpDivide sx_checked dx_checked pos sup_type parent_env (errors ++ merged_errors)
                        -- Altrimenti, bisogna fare il cast del nodo con il tipo "non T.sup"
                        -- In questo caso il nodo dx è quello che deve essere castato
                        |  sup_type /= sx_type -> RightExpDivide sx_checked (apply_coercion sup_type dx_checked) pos sup_type parent_env (errors ++ merged_errors)
                        |  otherwise           -> RightExpDivide (apply_coercion sup_type sx_checked) dx_checked pos sup_type parent_env (errors ++ merged_errors)

    staticsemanticAux (RightExpMod sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
            sx_type = right_exp_type sx_checked
            dx_type = right_exp_type dx_checked
        
        in case T.sup sx_type dx_type of
            -- Something's wrong. We have to return an ErrorType, because we can't understand where the mistake actaully is
            T.ErrorType -> RightExpDivide sx_checked dx_checked pos T.ErrorType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted sx_type dx_type "minus" pos])
                        -- Se sono tutti dello stesso tipo, allora si può semplicemente ritornare il nodo
            sup_type    |  (T.all_same_type [sup_type, dx_type, sx_type]) -> RightExpDivide sx_checked dx_checked pos sup_type parent_env (errors ++ merged_errors)
                        -- Altrimenti, bisogna fare il cast del nodo con il tipo "non T.sup"
                        -- In questo caso il nodo dx è quello che deve essere castato
                        |  sup_type /= sx_type -> RightExpDivide sx_checked (apply_coercion sup_type dx_checked) pos sup_type parent_env (errors ++ merged_errors)
                        |  otherwise           -> RightExpDivide (apply_coercion sup_type sx_checked) dx_checked pos sup_type parent_env (errors ++ merged_errors)

    staticsemanticAux (RightExpDiv sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
            sx_type = right_exp_type sx_checked
            dx_type = right_exp_type dx_checked
        
        in case T.sup sx_type dx_type of
            -- Something's wrong. We have to return an ErrorType, because we can't understand where the mistake actaully is
            T.ErrorType -> RightExpDivide sx_checked dx_checked pos T.ErrorType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted sx_type dx_type "minus" pos])
                        -- Se sono tutti dello stesso tipo, allora si può semplicemente ritornare il nodo
            sup_type    |  (T.all_same_type [sup_type, dx_type, sx_type]) -> RightExpDivide sx_checked dx_checked pos sup_type parent_env (errors ++ merged_errors)
                        -- Altrimenti, bisogna fare il cast del nodo con il tipo "non T.sup"
                        -- In questo caso il nodo dx è quello che deve essere castato
                        |  sup_type /= sx_type -> RightExpDivide sx_checked (apply_coercion sup_type dx_checked) pos sup_type parent_env (errors ++ merged_errors)
                        |  otherwise           -> RightExpDivide (apply_coercion sup_type sx_checked) dx_checked pos sup_type parent_env (errors ++ merged_errors)

    staticsemanticAux (RightExpPower sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
            sx_type = right_exp_type sx_checked
            dx_type = right_exp_type dx_checked
        
        in case T.sup sx_type dx_type of
            -- Something's wrong. We have to return an ErrorType, because we can't understand where the mistake actaully is
            T.ErrorType -> RightExpPower sx_checked dx_checked pos T.ErrorType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted sx_type dx_type "minus" pos])
                        -- Se sono tutti dello stesso tipo, allora si può semplicemente ritornare il nodo
            sup_type    |  (T.all_same_type [sup_type, dx_type, sx_type]) -> RightExpPower sx_checked dx_checked pos sup_type parent_env (errors ++ merged_errors)
                        -- Altrimenti, bisogna fare il cast del nodo con il tipo "non T.sup"
                        -- In questo caso il nodo dx è quello che deve essere castato
                        |  sup_type /= sx_type -> RightExpPower sx_checked (apply_coercion sup_type dx_checked) pos sup_type parent_env (errors ++ merged_errors)
                        |  otherwise           -> RightExpPower (apply_coercion sup_type sx_checked) dx_checked pos sup_type parent_env (errors ++ merged_errors)

    staticsemanticAux (RightExpNot dx pos ty parent_env errors) =
        -- Controllo che dx sia booleano
        let dx_checked = staticsemanticAux (dx {right_exp_env = parent_env})
        in case right_exp_type dx_checked of
            T.BooleanType -> RightExpNot dx_checked pos T.BooleanType parent_env (errors ++ right_exp_errors dx_checked)
            _         -> RightExpNot dx_checked pos T.BooleanType parent_env (errors ++ (right_exp_errors dx_checked) ++ [Err.errMsgUnaryOperationNotPermitted (right_exp_type dx_checked) [T.BooleanType] "not" pos])

    staticsemanticAux (RightExpMinusUnary dx pos ty parent_env errors) =
        -- Controllo che dx sia un math type permesso
        let dx_checked = staticsemanticAux (dx {right_exp_env = parent_env})
        in case T.mathType (right_exp_type dx_checked) of
            T.ErrorType -> RightExpMinusUnary dx_checked pos T.ErrorType parent_env (errors ++ (right_exp_errors dx_checked) ++ [Err.errMsgUnaryOperationNotPermitted (right_exp_type dx_checked) [T.IntegerType, T.RealType] "minus" pos])
            res_type    -> RightExpMinusUnary dx_checked pos res_type parent_env (errors ++ (right_exp_errors dx_checked))

    staticsemanticAux (RightExpPlusUnary dx pos ty parent_env errors) =
        -- Controllo che dx sia un math type permesso
        let dx_checked = staticsemanticAux (dx {right_exp_env = parent_env})
        in case T.mathType (right_exp_type dx_checked) of
            T.ErrorType -> RightExpMinusUnary dx_checked pos T.ErrorType parent_env (errors ++ (right_exp_errors dx_checked) ++ [Err.errMsgUnaryOperationNotPermitted (right_exp_type dx_checked) [T.IntegerType, T.RealType] "plus" pos])
            res_type    -> RightExpMinusUnary dx_checked pos res_type parent_env (errors ++ (right_exp_errors dx_checked))

    staticsemanticAux (RightExpInteger dx pos ty parent_env errors) =
        -- Tutto viene già inizializzato da Parser.y (valore, tipo, env, ...)
        -- Basta ritornare lo stesso valore
        (RightExpInteger dx pos ty parent_env errors)

    staticsemanticAux (RightExpReal dx pos ty parent_env errors) =
        -- Tutto viene già inizializzato da Parser.y (valore, tipo, env, ...)
        -- Basta ritornare lo stesso valore
        (RightExpReal dx pos ty parent_env errors)

    staticsemanticAux (RightExpBoolean dx pos ty parent_env errors) =
        -- Tutto viene già inizializzato da Parser.y (valore, tipo, env, ...)
        -- Basta ritornare lo stesso valore
        (RightExpBoolean dx pos ty parent_env errors)

    staticsemanticAux (RightExpChar dx pos ty parent_env errors) =
        -- Tutto viene già inizializzato da Parser.y (valore, tipo, env, ...)
        -- Basta ritornare lo stesso valore
        (RightExpChar dx pos ty parent_env errors)

    staticsemanticAux (RightExpString dx pos ty parent_env errors) =
       -- Tutto viene già inizializzato da Parser.y (valore, tipo, env, ...)
        -- Basta ritornare lo stesso valore
        (RightExpString dx pos ty parent_env errors)

    staticsemanticAux (RightExpFuncProcCall id params pos ty parent_env errors) =
        -- Controllo che la funzione sia presente nell'env, visto che deve essere già stata dichiarata
        -- Nel caso ci si accorga che la funzione in realtà è una procedura, allora si ritorna un errore:
        --     non avrebbe senso assegnare il valore di ritorno di una procedura, visto che non esiste
        let function_name = id_name id
            params_checked = map (\(param) -> staticsemanticAux (param {right_exp_env = parent_env})) params
        
        in case E.lookup parent_env function_name of
            Nothing -> RightExpFuncProcCall id params pos T.ErrorType parent_env (errors ++ [Err.errMsgNotDeclared function_name pos])
            -- Tutto ok, la funzione è stata dichiarata
            
            Just (E.FunEntry entry_params ty_ret _) |   map snd entry_params == map right_exp_type params_checked -> RightExpFuncProcCall id params_checked pos ty_ret parent_env errors
            -- La funzione è dichiarata nell'enviroment, ma i parametri non corrispondono 
            -- TODO: si potrebbero mandare messaggi più significativi, tipo se manca un parametro o se c'è un parametro in più, se i tipi sono sbagliati, ...
                                                  |   otherwise -> RightExpFuncProcCall id params pos ty_ret parent_env (errors ++ [Err.errMsgWrongParams function_name pos]) -- TODO: se abbiamo tempo si potrebbe 
            -- La funzione è dichiarata nell'enviroment, ma è una procedura
           
            Just _ -> RightExpFuncProcCall id params pos T.ErrorType parent_env (errors ++ [Err.errMsgAssignToProc function_name pos])
            
    staticsemanticAux (RightExpCopy left_exp pos ty parent_env errors) =
        -- Controllo che left_exp sia corretto
        let left_exp_checked = staticsemanticAux (left_exp {left_exp_env = parent_env})
        in case left_exp_type left_exp_checked of
            T.ErrorType     -> RightExpCopy left_exp_checked pos T.ErrorType parent_env (errors ++ (left_exp_errors left_exp_checked))
            lft_expr_type   -> RightExpCopy left_exp_checked pos (left_exp_type left_exp_checked) parent_env (errors ++ (left_exp_errors left_exp_checked))


    -- staticsemanticAux (RightExpCoercion main_re from_type to_type pos ty parent_env errors) =
        -- La funzione non viene implementata in questo caso, visto che naturalmente non esisterà mai 
        -- un nodo di questo tipo, ma verrà solo aggiunto da noi artificialmente

instance StaticSemanticClass [RightExp] where
    staticsemanticAux xs = map (staticsemanticAux) xs

instance StaticSemanticClass LeftExp where
    -- Controllo che l'id sia presente nell'env e che sia una variabile (altrimenti non avrebbe senso assegnare qualcosa)
    staticsemanticAux (LeftExpIdent id pos ty env errors) = case E.lookup env (id_name id) of
        Nothing -> (LeftExpIdent id pos T.ErrorType env (errors ++ [Err.errMsgNotDeclared id pos]))
        Just (E.VarEntry ty_ret) -> (LeftExpIdent id pos ty_ret env errors)
        Just _ -> (LeftExpIdent id pos T.ErrorType env (errors ++ [Err.errAssignToLeftExpr id pos]))

    staticsemanticAux (LeftExpArrayAccess array_name array_locations lexp_type pos env errors) =
        -- Controllo che la left_exp sia corretta
        let lexp_checked = staticsemanticAux array_name {left_exp_env = env} 
            -- Controllo che le varie locazioni indicate siano di tipo integer (non può essere altro)
            first_rexp = (head array_locations) {right_exp_env = env}
            array_locations_after = staticsemanticAux (first_rexp : tail array_locations)
            -- Estraggo i tipi dalle locazioni per generare eventuali errori
            array_loc_types_errors = foldl (\lst x -> lst ++ mkArrTy lexp_type (right_exp_type x) (right_exp_pos x)) [] array_locations_after
        in (LeftExpArrayAccess lexp_checked array_locations_after (left_exp_type lexp_checked) pos env (errors ++ array_loc_types_errors))

    staticsemanticAux (LeftExpPointerValue left_exp pos left_exp_ty env errors) =
        -- Controllo che la left_exp sia corretta e "porto su" il tipo
        let lexp_checked = staticsemanticAux left_exp {left_exp_env = env} 
        in (LeftExpPointerValue lexp_checked pos (left_exp_type lexp_checked) env (errors ++ (left_exp_errors lexp_checked)))

    staticsemanticAux (LeftExpPointerAddress left_exp pos left_exp_ty env errors) =
        -- Controllo che la left_exp sia corretta e "porto su" il tipo
        let (lexp_checked) = staticsemanticAux left_exp {left_exp_env = env}
        in (LeftExpPointerAddress lexp_checked pos (left_exp_type lexp_checked) env (errors ++ (left_exp_errors lexp_checked)))

instance StaticSemanticClass Assign where
    staticsemanticAux (VariableAssignment left_exp right_exp pos env errors) =
        -- Controllo che la left_exp sia corretta
        let lexp_checked = staticsemanticAux left_exp {left_exp_env = env}
            -- Controllo che la right_exp sia corretta
            rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            -- Estraggo i tipi delle left_exp e right_exp per generare eventuali errori
            l_type = left_exp_type lexp_checked
            r_type = right_exp_type rexp_checked
            -- Estraggo gli errori delle left_exp e right_exp da aggiungere
            l_errors = left_exp_errors lexp_checked
            r_errors = right_exp_errors rexp_checked
        -- Controllo che left_exp e right_exp abbiano lo stesso tipo
        
        in (VariableAssignment lexp_checked rexp_checked pos env (errors ++ l_errors ++ r_errors ++ (mkAssignErrs l_type r_type pos)))
    
instance StaticSemanticClass WritePrimitive where
    staticsemanticAux (WriteInt right_exp pos env errors) =
        let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            r_type = right_exp_type rexp_checked
            tot_errors = errors ++ (right_exp_errors rexp_checked)
        
        -- Controllo che right_exp sia un intero
        in case r_type of
            T.IntegerType -> (WriteInt rexp_checked pos env tot_errors)
            _ -> (WriteInt rexp_checked pos env $ tot_errors ++ [Err.errMsgWrongWritePrimitiveType T.IntegerType r_type pos])
    
    staticsemanticAux (WriteReal right_exp pos env errors) =
        let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            r_type = right_exp_type rexp_checked
            tot_errors = errors ++ (right_exp_errors rexp_checked)
        
        -- Controllo che right_exp sia un real
        in case r_type of
            T.RealType -> (WriteReal rexp_checked pos env tot_errors)
            _ -> (WriteReal rexp_checked pos env $ tot_errors ++ [Err.errMsgWrongWritePrimitiveType T.RealType r_type pos])

    staticsemanticAux (WriteChar right_exp pos env errors) =
        let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            r_type = right_exp_type rexp_checked
            tot_errors = errors ++ (right_exp_errors rexp_checked)
        
        -- Controllo che right_exp sia un real
        in case r_type of
            T.CharType -> (WriteChar rexp_checked pos env tot_errors)
            _ -> (WriteChar rexp_checked pos env $ tot_errors ++ [Err.errMsgWrongWritePrimitiveType T.CharType r_type pos])

    staticsemanticAux (WriteString right_exp pos env errors) =
        let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            r_type = right_exp_type rexp_checked
            tot_errors = errors ++ (right_exp_errors rexp_checked)
        
        -- Controllo che right_exp sia un real
        in case r_type of
            T.StringType -> (WriteString rexp_checked pos env tot_errors)
            _ -> (WriteString rexp_checked pos env $ tot_errors ++ [Err.errMsgWrongWritePrimitiveType T.StringType r_type pos])

instance StaticSemanticClass ReadPrimitive where
    staticsemanticAux (ReadInt left_exp pos env errors) = 
        let lexp_checked = staticsemanticAux left_exp {left_exp_env = env}
            l_type = left_exp_type lexp_checked
            tot_errors = errors ++ (left_exp_errors lexp_checked)

        -- Controllo che left_exp sia un intero
        in case l_type of
            T.IntegerType -> (ReadInt lexp_checked pos env tot_errors)
            _ -> (ReadInt lexp_checked pos env $ tot_errors ++ [Err.errMsgWrongReadPrimitiveType T.IntegerType l_type pos])

    staticsemanticAux (ReadReal left_exp pos env errors) =
        let lexp_checked = staticsemanticAux left_exp {left_exp_env = env}
            l_type = left_exp_type lexp_checked
            tot_errors = errors ++ (left_exp_errors lexp_checked)

        -- Controllo che left_exp sia un real
        in case l_type of
            T.RealType -> (ReadReal lexp_checked pos env tot_errors)
            _ -> (ReadReal lexp_checked pos env $ tot_errors ++ [Err.errMsgWrongReadPrimitiveType T.RealType l_type pos])
            
    staticsemanticAux (ReadChar left_exp pos env errors) =
        let lexp_checked = staticsemanticAux left_exp {left_exp_env = env}
            l_type = left_exp_type lexp_checked
            tot_errors = errors ++ (left_exp_errors lexp_checked)

        -- Controllo che left_exp sia un char
        in case l_type of
            T.CharType -> (ReadChar lexp_checked pos env tot_errors)
            _ -> (ReadChar lexp_checked pos env $ tot_errors ++ [Err.errMsgWrongReadPrimitiveType T.CharType l_type pos])
            
    staticsemanticAux (ReadString left_exp pos env errors) =
        let lexp_checked = staticsemanticAux left_exp {left_exp_env = env}
            l_type = left_exp_type lexp_checked
            tot_errors = errors ++ (left_exp_errors lexp_checked)

        -- Controllo che left_exp sia un string
        in case l_type of
            T.StringType -> (ReadString lexp_checked pos env tot_errors)
            _ -> (ReadString lexp_checked pos env $ tot_errors ++ [Err.errMsgWrongReadPrimitiveType T.StringType l_type pos])
            

{- main = do 

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
    putStrLn "" -}