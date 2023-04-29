module ErrorMessage where

import Data.List

errMsgFunctionAlreadyImpl :: (Show a) => a -> (Int, Int) -> [Char]
errMsgFunctionAlreadyImpl varname pos = printPos pos ++ "The function/procedure " ++ (show varname) ++ " is already implemented"

errMsgDifferentParams :: (Show a) => a -> String -> String -> (Int, Int) -> [Char]
errMsgDifferentParams varname p1 p2 pos = printPos pos ++ "The function/procedure " ++ (show varname) ++ " is already forward defined, but with different parameters: \n old = " ++ p1 ++ " \n new = " ++ p2

errMsgDifferentRet :: (Show a) => a -> String -> String -> (Int, Int) -> [Char]
errMsgDifferentRet varname r1 r2 pos = printPos pos ++ "The function " ++ (show varname) ++ " is already forward defined, but with different return type: \n old = " ++ r1 ++ " \n new = " ++ r2

errMsgNotImplemented :: (Show a) => a -> (Int, Int) -> [Char]
errMsgNotImplemented s pos = "In the block " ++ printPos pos ++ (show s) ++ " is forward declared but not implemented"

errMsgReturnNotSet :: (Show a) => a -> (Int, Int) -> [Char]
errMsgReturnNotSet s pos = "Inside any block of the function " ++ printPos pos ++ (show s) ++ " there is no return statement for this function"

errMsgReturnOutsideFunction :: String -> (Int, Int) -> [Char]
errMsgReturnOutsideFunction id pos = printPos pos ++ "The Return statement of function " ++ (show id) ++ " is outside its block"

errMsgWrongFunctionType :: (Show a) => a -> (Int, Int) -> [Char]
errMsgWrongFunctionType s pos = printPos pos ++ (show s) ++ " is forward declared but the types of the parameters and/or the return are different"

errMsgNotMathType :: (Show a) => a -> (Int, Int) -> [Char]
errMsgNotMathType t pos = printPos pos ++ (show t) ++ "Not a math type"

errMsgNotCompatible :: (Show a1, Show a2) => a1 -> a2 -> (Int, Int) -> [Char]
errMsgNotCompatible t1 t2 pos = printPos pos ++ (show t1) ++ " is not compatible with type " ++ (show t2)

errMsgReturnNotCompatible :: (Show a) => a -> (Int, Int) -> [Char]
errMsgReturnNotCompatible t pos = printPos pos ++ "Return type is not compatible with type " ++ (show t)

errMsgInternalErr :: (Int, Int) -> [Char]
errMsgInternalErr pos = printPos pos ++ "Unexpected internal error"

errMsgAlreadyDeclared :: (Show a) => a -> (Int, Int) -> [Char]
errMsgAlreadyDeclared id pos = printPos pos ++ "The name " ++ (show id) ++ " is already declared in this scope"

errMsgNotDeclared :: (Show a) => a -> (Int, Int) -> [Char]
errMsgNotDeclared id pos = printPos pos ++ "The name " ++ (show id) ++ " is not declared in this scope"

errMsgNotFunctionProcedure :: (Show a) => a -> (Int, Int) -> [Char]
errMsgNotFunctionProcedure id pos = printPos pos ++ "The object " ++ (show id) ++ " is not a function or procedure"

errMsgUnexpectedType :: (Show a1, Show a2) => [Char] -> a1 -> a2 -> (Int, Int) -> [Char]
errMsgUnexpectedType obj t_exp t_found pos = printPos pos ++ obj ++ " must be of type " ++ (show t_exp)  ++ " but type " ++ (show t_found) ++ " found"

errMsgTypeNotArray :: (Show a) => a -> (Int, Int) -> [Char]
errMsgTypeNotArray t_found pos = printPos pos ++ "Expected an array but type " ++ (show t_found) ++ " found"

errMsgClash :: (Show a) => a -> (Int, Int) -> [Char]
errMsgClash varname pos = printPos pos ++ "The name " ++ (show varname) ++ " is already declared in this scope"

errMsgWrongLoopControl :: (Show a) => a -> (Int, Int) -> [Char]
errMsgWrongLoopControl t pos = printPos pos ++ (show t) ++ " is used outside a loop"

errAssignToLeftExpr :: (Show a) => a -> (Int, Int) -> [Char]
errAssignToLeftExpr t pos = printPos pos ++ "Can't assign value to type " ++ (show t)

errMsgWrongWritePrimitiveType :: (Show a1, Show a2) => a1 -> a2 -> (Int, Int) -> [Char]
errMsgWrongWritePrimitiveType t1 t2 pos = printPos pos ++ "A 'Write' primitive of " ++ (show t1) ++ " type is used, but type " ++ (show t2) ++ " is given"

errMsgWrongReadPrimitiveType :: (Show a1, Show a2) => a1 -> a2 -> (Int, Int) -> [Char]
errMsgWrongReadPrimitiveType t1 t2 pos = printPos pos ++ "A 'Read' primitive of " ++ (show t1) ++ " type is used, but it's trying to assign a " ++ (show t2) ++ " type"

errMsgRelationNotBool :: (Show a1, Show a2) => a1 -> a2 -> (Int, Int) -> [Char]
errMsgRelationNotBool t1 t2 pos = printPos pos ++ "A relation operation must be between booleans, but types " ++ (show t1) ++" and " ++ (show t2) ++ " are given"

errMsgNotArray :: (Int, Int) -> [Char]
errMsgNotArray pos = printPos pos ++ "The type is not an array"

errMsgWrongArrayDim :: (Int, Int) -> [Char]
errMsgWrongArrayDim pos = printPos pos ++ "The array access has not the right dimension"

errMsgWrongArrayIndex :: (Int, Int) -> [Char]
errMsgWrongArrayIndex pos = printPos pos ++ "The array index must be one or more integers, but different types are given"

errMsgOperationNotPermitted :: (Show a1, Show a2) => a1 -> a2 -> String -> (Int, Int) -> [Char]
errMsgOperationNotPermitted t1 t2 op pos = printPos pos ++ "A " ++ (show op) ++ " operation must be between compatible types, but types " ++ (show t1) ++" and " ++ (show t2) ++ " are given"

errMsgUnaryOperationNotPermitted :: (Show a) => a -> [a] -> String -> (Int, Int) -> [Char]
errMsgUnaryOperationNotPermitted t possible_types op pos = printPos pos ++ "A unary" ++ (show op) ++ " operation must be applied only to allowd types (" ++ allowd_types ++ "), but type " ++ (show t) ++ " is given" 
    where allowd_types = intercalate ", " (map show possible_types)

errMsgAssignToProc :: (Show a) => a -> (Int, Int) -> [Char]
errMsgAssignToProc t pos = printPos pos ++ "Procedure " ++ (show t) ++ " can't assign values"

errMsgAssignToConst :: String -> (Int, Int) -> [Char]
errMsgAssignToConst id pos = printPos pos ++ (show id) ++ ": It's not possible to assign a value to a const " 

errMsgAssignToForIterator :: String -> (Int, Int) -> [Char]
errMsgAssignToForIterator id pos = printPos pos ++ (show id) ++ ": It's not possible to assign a value to a for iterator " 

errMsgConstLimit :: String -> (Int, Int) -> [Char]
errMsgConstLimit id pos = printPos pos ++ (show id) ++ ": A const can only be an integer, a char, a string, a boolean or a real " 

errMsgWrongParams :: (Show a) => a -> (Int, Int) -> [Char]
errMsgWrongParams t pos = printPos pos ++ "Wrong parameters for function " ++ (show t)

errMsgUnexpectedParams :: (Show a) => a -> (Int, Int) -> [Char]
errMsgUnexpectedParams t pos = printPos pos ++ (show t) "is not a valid argument for the function" 

errMsgCostNotPointArray :: String -> (Int, Int) -> String
errMsgCostNotPointArray id pos = printPos pos ++ (show id) ++ ": A constant can't be an array or a pointer"

errMsgForIteratorNotPointArray :: String -> (Int, Int) -> String
errMsgForIteratorNotPointArray id pos = printPos pos ++ (show id) ++ ": A for iterator can't be an array or a pointer"

errMsgPointerError :: String -> (Int, Int) -> String
errMsgPointerError error pos = printPos pos ++ "Pointer error : " ++ error

errMsgFunctionError :: String -> String -> (Int, Int) -> String
errMsgFunctionError id error pos = printPos pos ++ "Function error " ++ (show id) ++ " : " ++ error

printPos :: (Int, Int) -> [Char]
printPos (l, c) = "At line " ++ (show l) ++ ", column " ++ (show c) ++ ": \n\t"
