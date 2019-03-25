--Author: Julian Rathke, 2018 
--Provides an implementation of a type checker for the \Toy language from the lecture notes.
module ToyTypes where 
import ToyGrammar

--Data structures as defined in ToyGrammar:
--data ToyType = TyInt | TyBool | TyUnit | TyPair ToyType ToyType | TyFun ToyType ToyType
--type Environment = [ (String,Expr) ]
--data Expr = TmInt Int | TmTrue | TmFalse | TmUnit | TmCompare Expr Expr 
--           | TmPair Expr Expr | TmAdd Expr Expr | TmVar String 
--           | TmFst Expr | TmSnd Expr
--           | TmIf Expr Expr Expr | TmLet String ToyType Expr Expr
--           | TmLambda String ToyType Expr | TmApp Expr Expr
--           | Cl ( String ToyType Expr Environment)

type TypeEnvironment = [ (String,ToyType) ]

getBinding :: String -> TypeEnvironment -> ToyType
getBinding x [] = error "Variable binding not found"
getBinding x ((y,t):tenv) | x == y  = t
                        | otherwise = getBinding x tenv

addBinding :: String -> ToyType -> TypeEnvironment -> TypeEnvironment
addBinding x t tenv = (x,t):tenv

typeOf :: TypeEnvironment -> Expr -> ToyType
typeOf tenv (TmInt _ )  = TyInt

typeOf tenv (TmTrue ) = TyBool

typeOf tenv (TmFalse ) = TyBool

typeOf tenv (TmUnit ) = TyUnit

typeOf tenv (TmCompare e1 e2) = TyBool
  where (TyInt,TyInt) = (typeOf tenv e1, typeOf tenv e2)

typeOf tenv (TmPair e1 e2) = TyPair t1 t2 
  where (t1,t2) = (typeOf tenv e1, typeOf tenv e2) 

typeOf tenv (TmFst e1) = t1 
  where (TyPair t1 t2) = typeOf tenv e1

typeOf tenv (TmSnd e2) = t2
  where (TyPair t1 t2) = typeOf tenv e2

typeOf tenv (TmAdd e1 e2) = TyInt 
  where (TyInt,TyInt) = (typeOf tenv e1, typeOf tenv e2)

typeOf tenv (TmVar x) = getBinding x tenv

typeOf tenv (TmIf e1 e2 e3) | t2 == t3 = t2
  where (TyBool,t2,t3) = (typeOf tenv e1, typeOf tenv e2, typeOf tenv e3)

typeOf tenv (TmLambda x t e) = TyFun t u 
  where u = typeOf (addBinding x t tenv) e

typeOf tenv (TmApp e1 e2) | t1 == t3 = t2
  where ((TyFun t1 t2),t3) = (typeOf tenv e1, typeOf tenv e2)

typeOf tenv (TmLet x t e1 e2) | t == t1 = typeOf (addBinding x t tenv) e2
  where t1 = typeOf tenv e1

typeOf tenv _ = error "Type Error"

-- Function for printing the results of the TypeCheck
unparseType :: ToyType -> String
unparseType TyBool = "Bool"
unparseType TyInt = "Int"
unparseType TyUnit = "Unit"
unparseType (TyPair t1 t2) = "( " ++ (unparseType t1) ++ " , " ++ (unparseType t2) ++ " )"
unparseType (TyFun t1 t2) = (unparseType t1) ++ " -> " ++ (unparseType t2)
