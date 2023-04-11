

module ErrorMessage where

errMsgAssign :: (Show a1, Show a2) => a2 -> a1 -> [Char]
errMsgAssign lhs rhs = "Can't assign value of type " ++ (show rhs) ++ " to variable of type " ++ (show lhs)

errMsgNotMathType :: Show a => a -> [Char]
errMsgNotMathType t = (show t) ++ "Not a math type"

errMsgNotCompatible :: (Show a1, Show a2) => a1 -> a2 -> [Char]
errMsgNotCompatible t1 t2 = (show t1) ++ " is not compatible with type " ++ (show t2)

errMsgReturnNotCompatible :: (Show a1, Show a2) => a1 -> a2 -> [Char]
errMsgReturnNotCompatible t1 t2 = "Return type is not compatible with type " ++ (show t2)

errMsgInternalErr :: [Char]
errMsgInternalErr = "Unexpected internal error"

--errMsgArrIdxNotInt t = errMsgUnexpectedType "Array indexes" T.IntegerType t

--errMsgGuardNotBool t = errMsgUnexpectedType "Guard" T.BooleanType t
errMsgUnexpectedType :: (Show a1, Show a2) => [Char] -> a1 -> a2 -> [Char]
errMsgUnexpectedType obj t_exp t_found = obj ++ " must be of type " ++ (show t_exp)  ++ " but type " ++ (show t_found) ++ " is given"

errMsgTypeNotArray :: Show a => a -> [Char]
errMsgTypeNotArray t_found = "Expected an array but type " ++ (show t_found) ++ " is given"

errMsgClash :: (Show a1) => a1 -> [Char]
errMsgClash varname = "The name " ++ (show varname) ++ " is already declared in this scope"

errMsgWrongLoopControl :: (Show a1) => a1 -> [Char]
errMsgWrongLoopControl t1 = (show t1) ++ " is used outside a loop"
