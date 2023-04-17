

module ErrorMessage where

errMsgNotImplemented :: (Show a) => a -> (Int, Int) -> [Char]
errMsgNotImplemented s pos = "In the block " ++ printPos pos ++ (show s) ++ " is forward declared but not implemented"

errMsgWrongFunctionType :: (Show a) => a -> (Int, Int) -> [Char]
errMsgWrongFunctionType s pos = printPos pos ++ (show s) ++ " is forward declared but the types of the parameters and/or the return are different"

errMsgAssign :: (Show a1, Show a2) => a2 -> a1 -> (Int, Int) -> [Char]
errMsgAssign lhs rhs pos = printPos pos ++ "Can't assign value of type " ++ (show rhs) ++ " to variable of type " ++ (show lhs)

errMsgNotMathType :: (Show a) => a -> (Int, Int) -> [Char]
errMsgNotMathType t pos = printPos pos ++ (show t) ++ "Not a math type"

errMsgNotCompatible :: (Show a1, Show a2) => a1 -> a2 -> (Int, Int) -> [Char]
errMsgNotCompatible t1 t2 pos = printPos pos ++ (show t1) ++ " is not compatible with type " ++ (show t2)

errMsgReturnNotCompatible :: (Show a) => a -> (Int, Int) -> [Char]
errMsgReturnNotCompatible t pos = printPos pos ++ "Return type is not compatible with type " ++ (show t)

errMsgInternalErr :: [Char]
errMsgInternalErr = "Unexpected internal error"

errMsgAlreadyDeclared :: (Show a) => a -> (Int, Int) -> [Char]
errMsgAlreadyDeclared id pos = printPos pos ++ "The name " ++ (show id) ++ " is already declared in this scope"

errMsgNotDeclared :: (Show a) => a -> (Int, Int) -> [Char]
errMsgNotDeclared id pos = printPos pos ++ "The name " ++ (show id) ++ " is not declared in this scope"

--errMsgArrIdxNotInt t = errMsgUnexpectedType "Array indexes" T.IntegerType t
--errMsgGuardNotBool t = errMsgUnexpectedType "Guard" T.BooleanType t

errMsgUnexpectedType :: (Show a1, Show a2) => [Char] -> a1 -> a2 -> (Int, Int) -> [Char]
errMsgUnexpectedType obj t_exp t_found pos = printPos pos ++ obj ++ " must be of type " ++ (show t_exp)  ++ " but type " ++ (show t_found) ++ " is given"

errMsgTypeNotArray :: (Show a) => a -> (Int, Int) -> [Char]
errMsgTypeNotArray t_found pos = printPos pos ++ "Expected an array but type " ++ (show t_found) ++ " is given"

errMsgClash :: (Show a) => a -> (Int, Int) -> [Char]
errMsgClash varname pos = printPos pos ++ "The name " ++ (show varname) ++ " is already declared in this scope"

errMsgWrongLoopControl :: (Show a) => a -> (Int, Int) -> [Char]
errMsgWrongLoopControl t pos = printPos pos ++ (show t) ++ " is used outside a loop"

errAssignToLeftExpr :: (Show a) => a -> (Int, Int) -> [Char]
errAssignToLeftExpr t pos = printPos pos ++ "Can't assign value to expression of type " ++ (show t)

errMsgWrongWritePrimitiveType :: (Show a1, Show a2) => a1 -> a2 -> (Int, Int) -> [Char]
errMsgWrongWritePrimitiveType t1 t2 pos = printPos pos ++ "A 'Write' primitive of " ++ (show t1) ++ " type is used, but type " ++ (show t2) ++ " is given"

errMsgWrongReadPrimitiveType :: (Show a1, Show a2) => a1 -> a2 -> (Int, Int) -> [Char]
errMsgWrongReadPrimitiveType t1 t2 pos = printPos pos ++ "A 'Read' primitive of " ++ (show t1) ++ " type is used, but it's trying to assign a " ++ (show t2) ++ " type"

errMsgRelationNotBool :: (Show a1, Show a2) => a1 -> a2 -> (Int, Int) -> [Char]
errMsgRelationNotBool t1 t2 pos = printPos pos ++ "A relation operation must be between booleans, but types " ++ (show t1) ++" and " ++ (show t1) ++ " are given"

errMsgOperationNotPermitted :: (Show a1, Show a2) => a1 -> a2 -> String -> (Int, Int) -> [Char]
errMsgOperationNotPermitted t1 t2 op pos = printPos pos ++ "A " ++ (show op) ++ " operation must be between compatible types, but types " ++ (show t1) ++" and " ++ (show t1) ++ " are given"

errMsgUnaryOperationNotPermitted :: (Show a) => a -> [a] -> String -> (Int, Int) -> [Char]
errMsgUnaryOperationNotPermitted t possible_types op pos = printPos pos ++ "A unary" ++ (show op) ++ " operation must be applied only to allowd types (" ++ allowd_types ++ "), but type " ++ (show t) ++ " is given" 
    where allowd_types = intercalate ", " (map show possible_types)

errMsgAssignToProc :: (Show a) => a -> (Int, Int) -> [Char]
errMsgAssignToProc t pos = printPos pos ++ "Procedure " ++ (show t) ++ " can't assign values"

errMsgWrongParams :: (Show a) => a -> (Int, Int) -> [Char]
errMsgWrongParams t pos = printPos pos ++ "Wrong parameters for function " ++ (show t)

printPos :: (Int, Int) -> [Char]
printPos (l, c) = "At line " ++ (show l) ++ ", column " ++ (show c) ++ ": \n\t"
