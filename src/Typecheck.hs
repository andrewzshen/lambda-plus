module Typecheck where

import AST
import qualified Data.Map as Map

type Gamma = Map.Map String Ty

newtype TypeError = TypeError String
instance Show TypeError where show (TypeError msg) = msg

tyErr :: String -> Either TypeError a
tyErr = Left . TypeError

equalTy :: Ty -> Ty -> Bool
equalTy t1 t2 = case (t1, t2) of
    (TInt, TInt)               -> True
    (TBool, TBool)             -> True
    (TUnit, TUnit)             -> True
    (TVoid, TVoid)             -> True

    (TVar x, TVar y)           -> x == y

    (TList elemTy1, TList elemTy2)               -> elemTy1 `equalTy` elemTy2
    (TFun paramTy1 retTy1, TFun paramTy2 retTy2) -> paramTy1 `equalTy` paramTy2 && retTy1 `equalTy` retTy2
    (TProd a1 b1, TProd a2 b2)                   -> a1 `equalTy` a2 && b1 `equalTy` b2
    (TSum a1 b1, TSum a2 b2)                     -> a1 `equalTy` a2 && b1 `equalTy` b2
    _                          -> False

abstractEval :: Gamma -> Expr -> Either TypeError Ty
abstractEval env e = case e of
    IntLit _              -> pure TInt
    BoolLit _             -> pure TBool
    Unit                  -> pure TUnit
    Absurd _              -> pure TVoid

    Var x -> case Map.lookup x env of
        Just ty -> pure ty
        Nothing -> tyErr ("Unbound variable " ++ x)

    Binop _ lhs rhs -> do
        lhsTy <- abstractEval env lhs
        rhsTy <- abstractEval env rhs
        if lhsTy `equalTy` TInt && rhsTy `equalTy` TInt 
            then pure TInt 
            else tyErr "Arith expects int operands"
        
    Comp relop lhs rhs -> do
        lhsTy <- abstractEval env lhs
        rhsTy <- abstractEval env rhs
        if lhsTy `equalTy` TInt && rhsTy `equalTy` TInt 
            then pure TBool
            else tyErr "Comp expects int operands"

    App fn arg -> do
        fnTy <- abstractEval env fn
        argTy <- abstractEval env arg
        case fnTy of
            TFun paramTy retTy
                | argTy `equalTy` paramTy -> pure retTy
                | otherwise               -> tyErr "Function argument type mismatch"
            _ -> tyErr "Application of non-function"

    IfThenElse cond tt ff -> do
        condTy <- abstractEval env cond
        if condTy `equalTy` TBool
            then do
                ttTy <- abstractEval env tt
                ffTy <- abstractEval env ff
                if ttTy `equalTy` ffTy
                    then pure ttTy
                    else tyErr "Condition must be bool type"
            else tyErr "Branches must have same type"

    ListCons head tail -> do
        headTy <- abstractEval env head
        tailTy <- abstractEval env tail
        case tailTy of
            TList elemTy
                | elemTy `equalTy` headTy -> pure (TList elemTy)
                | otherwise               -> tyErr "Head type mismatch in list"
            _ -> tyErr "Tail must be list type"

    Both e1 e2 -> TProd <$> abstractEval env e1 <*> abstractEval env e2
        
    E1 e1 -> undefined
    E2 e2 -> undefined
    
    Annot body expectedTy -> do
        actualTy <- abstractEval env body
        if actualTy `equalTy` expectedTy
            then pure expectedTy
            else tyErr "Type annotation does not match actual type"

    Lambda (Just paramTy) (param, body) -> do
        bodyT <- abstractEval (Map.insert param paramTy env) body
        pure (TFun paramTy bodyT)
    Lambda Nothing _ -> tyErr "Lambda requires type annotation"

    Fix (Just expectedTy) (self, body) -> do
        bodyT <- abstractEval (Map.insert self expectedTy env) body
        if expectedTy `equalTy` bodyT
            then pure expectedTy
            else tyErr "Fix body does not match annotation"
    Fix Nothing _ -> tyErr "Fix requires type annotation"

    Let bound (name, body) -> do
        boundTy <- abstractEval env bound
        abstractEval (Map.insert name boundTy env) body

    ListMatch scrut nil (head, (tail, cons)) -> do
        scrutTy <- abstractEval env scrut
        case scrutTy of
            TList elemTy -> do
                nilTy <- abstractEval env nil
                let env' = Map.insert head elemTy (Map.insert tail (TList elemTy) env)
                consTy <- abstractEval env' cons
                if nilTy `equalTy` consTy
                    then pure nilTy
                    else tyErr "ListMatch branches must have same type"
            _ -> tyErr "ListMatch expects a list"

    Either e' (x, b1) (y, b2) -> undefined

    I1 e1 -> do
        ty <- abstractEval env e1
        case ty of
            TProd ty1 _ -> pure ty1
            _           -> tyErr "Expected prod type"

    I2 e2 -> do
        ty <- abstractEval env e2
        case ty of
            TProd _ ty2 -> pure ty2
            _           -> tyErr "Expected prod type"