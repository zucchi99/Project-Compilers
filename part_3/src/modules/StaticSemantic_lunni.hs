
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
    case (T.sup etype ttype) == T.ErrorType of      -- etype is compatible with ttype?
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


-- __________________________ STATIC SEMANTIC ANAL-ISYS
staticsemanticcheck x = case x of
    -- parse successful
    (ErrM.Ok a)    -> intercalate "\n\n" $ snd $ staticsemanticAux E.emptyEnv a
    -- parse error
    (ErrM.Bad err) -> err

-- __________________________ STATIC SEMANTIC CLASSES
class StaticSemanticClass a where
    staticsemanticAux :: a -> a

instance StaticSemanticClass Program where
    staticsemanticAux env (ProgramStart name block pos) = staticsemanticAux env block

instance StaticSemanticClass Ident where
    staticsemanticAux env (Ident _ _) = (env, [])

-- Since [Ident] should exists no more, this instance should be not needed
--instance StaticSemanticClass [Ident] where
  --  staticsemanticAux env idents = (env, [])

instance StaticSemanticClass BlockWithDecl where
    staticsemanticAux env (BlockWithDeclaration decls stmts pos) = 
            -- parto da un env vuoto e aggiungo le dichiarazioni
            -- se trovo un env già presente, aggiungo l'errore
        let (env1, errs1) = staticsemanticAux E.emptyEnv decls
            -- una volta finite le dichiarazioni del blocco, faccio il merge con il blocco padre
            -- dopo controllo ogni statement con l'env finale, con la possibilità di modificare l'env, ma non di aggiungere nuove dichiarazioni
            (env2, errs2) = staticsemanticAux (E.merge env1 env) stmts
        in (env2, errs1 ++ errs2)

instance StaticSemanticClass Declaration where
    staticsemanticAux env (DeclarationCostant id const_type_maybe value pos) = 
            -- se l'id è già nell'env -> ritorno errore
            -- se il tipo dichiarato è diverso dal tipo del valore -> ritorno errore




        --     -- controllo se l'id è presente nell'env
        -- let err_presence = checkPresenceDecl id env pos
        --     -- controllo se il tipo dichiarato è compatibile con il tipo del valore
        --     err_type = case type_maybe of
        --         Nothing -> []
        --         -- Ho bisogno di una funzione che data una RightExp mi restituisca il tipo
        --         Just t ->
        --         -- Just t -> mkIdDeclErrs id (T.getType value) t
        --     -- concateno gli errori
        --     errors = err_presence ++ err_type
        --     -- ottengo il tipo della costante
        --     type_const = case type_maybe of
        --         Nothing -> T.getType value
        --         Just t -> t
        --     -- concateno gli errori
        --     errors = err_presence ++ err_type
            
        -- -- se non ci sono errori, aggiungo l'entry all'env, altrimenti ritorno l'env precedente
        -- in if null errors
        --     then (E.insert env id (E.ConstEntry (T.getType type_const)), errors)
        --     else (env, errors)
    staticsemanticAux env (DeclarationVariable id var_type value_maybe pos) =
            -- se l'id è già nell'env -> ritorno errore
            -- se il tipo dichiarato è diverso dal tipo del valore -> ritorno errore



        --     -- controllo se l'id è presente nell'env
        -- let err_presence = checkPresenceDecl id env pos
        --     -- controllo se il tipo dichiarato è compatibile con il tipo del valore
        --     err_type = case value_maybe of
        --         Nothing -> []
        --         -- Ho bisogno di una funzione che data una RightExp mi restituisca il tipo
        --         Just value ->
        --         -- Just value -> mkIdDeclErrs id (T.getType value) (T.getType var_type) pos
        --     -- concateno gli errori
        --     errors = err_presence ++ err_type
        -- -- se non ci sono errori, aggiungo l'entry all'env, altrimenti ritorno l'env precedente
        -- in if null errors
        --     then (E.insert env id (E.VarEntry (T.getType var_type)), errors)
        --     else (env, errors)

    staticsemanticAux env (DeclarationFunction id params fun_type maybe_block pos) =
        -- In base a block:
        --      Nothing -> È una forward declaration, per cui controllo che non ci sia già nell'env.
        --      Just block ->   Devo controllare più cose:
        --                              1) Controllo che non ci sia nell'env o che sia presente la sua forward declaration
        --                              2) Controllo che il tipo di ritorno sia compatibile con il tipo della funzione dichiarata
    staticsemanticAux env (DeclarationProcedure id params maybe_block pos) =
        -- In base a block:
        --      Nothing -> È una forward declaration, per cui controllo che non ci sia già nell'env.
        --      Just block -> Controllo che non ci sia nell'env o che sia presente la sua forward declaration

instance StaticSemanticClass [Declaration] where
    -- Parto dall'enviroment passato
    -- Ogni enviroment parte dal precedente
    -- Restituisco l'enviroment finale e gli errori concatenati in ordine di ritrovamento
    staticsemanticAux env decls = foldl (addDeclaration) (env, []) decls where
        addDeclaration (env, errs) decl = 
            let (envAfter, errsAfter) = staticsemanticAux env decl
            in (envAfter, errs ++ errsAfter)

instance StaticSemanticClass Statement where
    staticsemanticAux env (StatementBlock block _) = staticsemanticAux env block
    staticsemanticAux env (StatementIf cond then_body maybe_else_body pos) = 
        -- Controllo che la condizione sia booleana
        -- Controllo che il then_body sia corretto
        -- Controllo che il else_body sia corretto
    staticsemanticAux env (StatementFor cond then_body var pos) = 
        -- Controllo che la condizione sia booleana, che contenga la variabile var e che sia fattibile
        -- Controllo che il then_body sia corretto
        -- Controllo che il var sia una variabile Intera, che non venga modificato dentro then_body
    staticsemanticAux env (StatementWhile cond then_body pos) =
        -- Controllo che la condizione sia booleana
        -- Controllo che il then_body sia corretto
    staticsemanticAux env (StatementRepeatUntil cond then_body pos) =
        -- Controllo che la condizione sia booleana
        -- Controllo che il then_body sia corretto
    staticsemanticAux env (StatementAssign assign pos) =
        -- Controllo che l'assegnamento sia corretto
    staticsemanticAux env (StatementFunctionCall id params pos) =
        -- Controllo che la funzione sia presente nell'env
        -- Controllo che i parametri corrispondano al tipo dei parametri voluti della funzione
    staticsemanticAux env (StatementProcedureCall id params pos) =
        -- Controllo che la funzione sia presente nell'env
        -- Controllo che i parametri corrispondano al tipo dei parametri voluti della funzione
    staticsemanticAux env (StatementWrite write_primitive pos) =
        -- Controllo che il write_primitive sia corretto
    staticsemanticAux env (StatementRead read_primitive pos) =
        -- Controllo che il read_primitive sia corretto

instance StaticSemanticClass ElseBlock where
    staticsemanticAux env (ElseBlock else_body _) = staticsemanticAux env else_body

get_sx_dx_errors_right_exp :: RightExp -> RightExp -> (RightExp -> RightExp -> [String])
get_sx_dx_errors_right_exp sx dx p_env = (new_sx, new_dx, new_errors) where
    new_errors = merge_errors_right_exp new_sx new_dx where
        new_sx = staticsemanticAux (sx {env = p_env})
        new_dx = staticsemanticAux (dx {env = p_env})

merge_errors_right_exp :: RightExp -> RightExp -> [String]
merge_errors_right_exp sx dx = (right_exp_errors sx) ++ (right_exp_errors dx)

apply_coercion :: Type -> RightExp -> RightExp
apply_coercion to_type main_r@(_ sx_re dx_re pos from_ty env errs) = RightExpCoercion main_r from_ty to_type pos env errs


instance StaticSemanticClass RightExp where
    staticsemanticAux (RightExpOr sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are boolean
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
        in if all_same_type [T.BooleanType, (right_exp_type sx_checked), (right_exp_type dx_checked)]
            -- Nothing's wrong
            then RightExpOr sx_checked dx_checked pos T.BooleanType parent_env errors
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            else RightExpOr sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgRelationNotBool (right_exp_type sx_checked) (right_exp_type dx_checked) pos])
    
    staticsemanticAux (RightExpAnd sx dx pos ty parent_env errors) = 
        -- Checking if both sx and dx are boolean
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env   
        in if all_same_type [T.BooleanType, (right_exp_type sx_checked), (right_exp_type dx_checked)]
            -- Nothing's wrong
            then RightExpAnd sx_checked dx_checked pos T.BooleanType parent_env errors
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            else RightExpAnd sx_checked dx_checked pos T.BooleanType parent_env (errors ++ [Err.errMsgRelationNotBool (right_exp_type sx_checked) (right_exp_type dx_checked) pos])
    
    staticsemanticAux (RightExpGreater sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env    
        in case rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            T.ErrorType -> RightExpGreater sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "greater" pos])
            -- Nothing's wrong
            _           -> RightExpGreater sx_checked dx_checked pos T.BooleanType parent_env errors
    
    staticsemanticAux (RightExpLess sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
        in case rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            T.ErrorType -> RightExpLess sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "less" pos])
            -- Nothing's wrong
            _           -> RightExpLess sx_checked dx_checked pos T.BooleanType parent_env errors

    staticsemanticAux (RightExpGreaterEqual sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
        in case rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            T.ErrorType -> RightExpGreaterEqual sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "'greater or equal'" pos])
            -- Nothing's wrong
            _           -> RightExpGreaterEqual sx_checked dx_checked pos T.BooleanType parent_env errors

    staticsemanticAux (RightExpLessEqual sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env 
        in case rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            T.ErrorType -> RightExpLessEqual sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "'less or equal'" pos])
            -- Nothing's wrong
            _           -> RightExpLessEqual sx_checked dx_checked pos T.BooleanType parent_env errors

    staticsemanticAux (RightExpEqual sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a relation operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
        
        in case rel (right_exp_type sx_checked) (right_exp_type dx_checked) of 
            -- Something's wrong, but we return a BooleanType anyway, so that the compiler can continue without generating redundant errors
            T.ErrorType -> RightExpEqual sx_checked dx_checked pos T.BooleanType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted (right_exp_type sx_checked) (right_exp_type dx_checked) "equal" pos])
            -- Nothing's wrong
            _           -> RightExpEqual sx_checked dx_checked pos T.BooleanType parent_env errors

    staticsemanticAux (RightExpPlus sx dx pos ty parent_env errors) =
        -- Checking if both sx and dx are compatibile for a math operation
        let (sx_checked, dx_checked, merged_errors) = get_sx_dx_errors_right_exp sx dx parent_env
            sx_type = right_exp_type sx_checked
            dx_type = right_exp_type dx_checked
        
        in case sup sx_type dx_type of
            -- Something's wrong. We have to return an ErrorType, because we can't understand where the mistake actaully is
            T.ErrorType -> RightExpEqual sx_checked dx_checked pos T.ErrorType parent_env (errors ++ merged_errors ++ [Err.errMsgOperationNotPermitted sx_type dx_type "plus" pos])
                        -- Se sono tutti dello stesso tipo, allora si può semplicemente ritornare il nodo
            sup_type    |  (all_same_type [sup_type, dx_type, sx_type]) -> RightExpPlus sx_checked dx_checked pos sup_type parent_env (errors ++ merged_errors)
                        -- Altrimenti, bisogna fare il cast del nodo con il tipo "non sup"
                        -- In questo caso il nodo dx è quello che deve essere castato
                        |  sup_type \= sx_type -> RightExpPlus sx_checked (apply_coercion sup_type dx_checked) pos sup_type parent_env (errors ++ merged_errors)
                        |  otherwise           -> RightExpPlus (apply_coercion sup_type sx_checked) dx_checked pos sup_type parent_env (errors ++ merged_errors)

    staticsemanticAux (RightExpMinus sx dx pos ty parent_env errors) =
        -- Controllo che sx e dx siano mathtype
    staticsemanticAux (RightExpTimes sx dx pos ty parent_env errors) =
        -- Controllo che sx e dx siano mathtype
    staticsemanticAux (RightExpDivide sx dx pos ty parent_env errors) =
        -- Controllo che sx e dx siano mathtype
    staticsemanticAux (RightExpMod sx dx pos ty parent_env errors) =
        -- Controllo che sx e dx siano mathtype
    staticsemanticAux (RightExpDiv sx dx pos ty parent_env errors) =
        -- Controllo che sx e dx siano mathtype
    staticsemanticAux (RightExpPower sx dx pos ty parent_env errors) =
        -- Controllo che sx e dx siano mathtype
    staticsemanticAux (RightExpNot dx pos ty parent_env errors) =
        -- Controllo che dx sia booleano
    staticsemanticAux (RightExpMinusUnary dx pos ty parent_env errors) =
        -- Controllo che dx sia mathtype
    staticsemanticAux (RightExpPlusUnary dx pos ty parent_env errors) =
        -- Controllo che dx sia mathtype
    staticsemanticAux (RightExpInteger dx pos ty parent_env errors) =
        -- Controllo che dx sia mathtype
    staticsemanticAux (RightExpReal dx pos ty parent_env errors) =
        -- Controllo che dx sia mathtype
    staticsemanticAux (RightExpBoolean dx pos ty parent_env errors) =
        -- Controllo che dx sia mathtype
    staticsemanticAux (RightExpChar dx pos ty parent_env errors) =
        -- Controllo che dx sia mathtype
    staticsemanticAux (RightExpString dx pos ty parent_env errors) =
        -- Controllo che dx sia mathtype
    staticsemanticAux (RightExpFunctionCall id params pos ty parent_env errors) =
        -- Controllo che la funzione sia presente nell'env
        -- Controllo che i parametri corrispondano al tipo dei parametri voluti della funzione
    staticsemanticAux (RightExpCopy left_exp pos ty parent_env errors) =
        -- Controllo che left_exp sia corretto

    -- staticsemanticAux (RightExpCoercion main_re from_type to_type pos ty parent_env errors) =
        -- La funzione non viene implementata in questo caso, visto che naturalmente non esisterà mai 
        -- un nodo di questo tipo, ma verrà solo aggiunto da noi artificialmente

instance StaticSemanticClass LeftExp where
    -- Controllo che l'id sia presente nell'env e che sia una variabile (altrimenti non avrebbe senso assegnare qualcosa)
    staticsemanticAux (LeftExpIdent id pos ty env errors) = case E.lookup id env of
        Nothing -> (LeftExpIdent id pos T.ErrorType env (errors ++ [Err.errMsgNotDeclared id pos]))
        Just (VarEntry ty_ret) -> (LeftExpIdent id pos ty_ret env errors)
        Just _ -> (LeftExpIdent id pos T.ErrorType env (errors ++ [Err.errAssignToLeftExpr id pos]))

    staticsemanticAux (LeftExpArrayAccess array_name array_locations lexp_type pos env errors) =
        -- Controllo che la left_exp sia corretta
        let lexp_checked = staticsemanticAux array_name {left_exp_env = env} 
            -- Controllo che le varie locazioni indicate siano di tipo integer (non può essere altro)
            first_rexp = head array_locations {right_exp_env = env}
            array_locations_after = staticsemanticAux (first_rexp : tail array_locations)
            -- Estraggo i tipi dalle locazioni per generare eventuali errori
            array_loc_types_errors = map (\x -> mkArrTy lexp_type (right_exp_type x)) array_locations_after
        in (LeftExpArrayAccess lexp_checked array_locations_after pos (left_exp_type lexp_checked) env (errors ++ array_loc_types_errors))

    staticsemanticAux (LeftExpPointerValue left_exp pos left_exp_ty env errors) =
        -- Controllo che la left_exp sia corretta e "porto su" il tipo
        let lexp_checked = staticsemanticAux array_name {left_exp_env = env} 
        in (LeftExpPointerValue lexp_checked pos (left_exp_type lexp_checked) env (errors ++ (left_exp_errors array_location_after)))

    staticsemanticAux (LeftExpPointerAddress left_exp pos left_exp_ty env errors) =
        -- Controllo che la left_exp sia corretta e "porto su" il tipo
        let lexp_checked = staticsemanticAux array_name {left_exp_env = env} 
        in (LeftExpPointerAddress lexp_checked pos (left_exp_type lexp_checked) env (errors ++ (left_exp_errors array_location_after)))
        
instance StaticSemanticClass Assign where
    staticsemanticAux env (VariableAssignment left_exp right_exp pos env errors) =
        -- Controllo che la left_exp sia corretta
        let lexp_checked = staticsemanticAux left_exp {left_exp_env = env}
            -- Controllo che la right_exp sia corretta
            rexp_checked = staticsemanticAux right_exp {left_exp_env = env}
            -- Estraggo i tipi delle left_exp e right_exp per generare eventuali errori
            l_type = left_exp_type lexp_checked
            r_type = right_exp_type rexp_checked
            -- Estraggo gli errori delle left_exp e right_exp da aggiungere
            l_errors = left_exp_errors lexp_checked
            r_errors = right_exp_errors rexp_checked
        -- Controllo che left_exp e right_exp abbiano lo stesso tipo
        
        in (VariableAssignment lexp_checked rexp_checked pos env (errors ++ l_errors ++ r_errors ++ $ mkAssignErrs l_type r_type pos))
    
instance StaticSemanticClass WritePrimitive where
    staticsemanticAux env (WriteInt right_exp pos env errors) =
        let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            r_type = right_exp_type rexp_checked
            tot_errors = errors ++ (right_exp_errors rexp_checked)
        
        -- Controllo che right_exp sia un intero
        in case r_type of
            T.IntegerType -> (WriteInt rexp_checked pos env tot_errors)
            _ -> (WriteInt rexp_checked pos env $ tot_errors ++ [Err.errMsgWrongWritePrimitiveType T.IntegerType r_type pos])
    
    staticsemanticAux env (WriteReal right_exp pos) =
        let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            r_type = right_exp_type rexp_checked
            tot_errors = errors ++ (right_exp_errors rexp_checked)
        
        -- Controllo che right_exp sia un real
        in case r_type of
            T.RealType -> (WriteReal rexp_checked pos env tot_errors)
            _ -> (WriteReal rexp_checked pos env $ tot_errors ++ [Err.errMsgWrongWritePrimitiveType T.RealType r_type pos])

    staticsemanticAux env (WriteChar right_exp pos) =
        let rexp_checked = staticsemanticAux right_exp {right_exp_env = env}
            r_type = right_exp_type rexp_checked
            tot_errors = errors ++ (right_exp_errors rexp_checked)
        
        -- Controllo che right_exp sia un real
        in case r_type of
            T.CharType -> (WriteChar rexp_checked pos env tot_errors)
            _ -> (WriteChar rexp_checked pos env $ tot_errors ++ [Err.errMsgWrongWritePrimitiveType T.CharType r_type pos])

    staticsemanticAux env (WriteString right_exp pos) =
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
            T.StringrType -> (ReadString lexp_checked pos env tot_errors)
            _ -> (ReadString lexp_checked pos env $ tot_errors ++ [Err.errMsgWrongReadPrimitiveType T.StringType l_type pos])
            

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