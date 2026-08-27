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

    (TList t1, TList t2)       -> t1 `equalTy` t2
    (TFun a1 b1, TFun a2 b2)   -> a1 `equalTy` a2 && b1 `equalTy` b2
    (TProd a1 b1, TProd a2 b2) -> a1 `equalTy` a2 && b1 `equalTy` b2
    (TSum a1 b1, TSum a2 b2)   -> a1 `equalTy` a2 && b1 `equalTy` b2
    _                          -> False

abstractEval :: Gamma -> Expr -> Either TypeError Ty
abstractEval env e = case e of
    IntLit _              -> pure TInt
    BoolLit _             -> pure TBool
    Unit                  -> pure TUnit
    Absurd _              -> pure TVoid

    Var x -> case Map.lookup x env of
        Just t  -> pure t
        Nothing -> tyErr ("Unbound variable " ++ x)

    Binop _ lhs rhs -> do
        lhsT <- abstractEval env lhs
        rhsT <- abstractEval env rhs
        if lhsT `equalTy` TInt && rhsT `equalTy` TInt 
            then pure TInt 
            else tyErr "Arith expects int operands"
        
    Comp relop lhs rhs -> do
        lhsT <- abstractEval env lhs
        rhsT <- abstractEval env rhs
        if lhsT `equalTy` TInt && rhsT `equalTy` TInt 
            then pure TBool
            else tyErr "Comp expects int operands"

    App fn arg -> do
        fnT <- abstractEval env fn
        argT <- abstractEval env arg
        case fnT of
            TFun paramT retT
                | argT `equalTy` paramT -> pure retT
                | otherwise             -> tyErr "Function argument type mismatch"
            _ -> tyErr "Application of non-function"

    IfThenElse cond tt ff -> do
        condT <- abstractEval env cond
        if condT `equalTy` TBool
            then do
                ttT <- abstractEval env tt
                ffT <- abstractEval env ff
                if ttT `equalTy` ffT 
                    then pure ttT
                    else tyErr "Condition must be bool type"
            else tyErr "Branches must have same type"

    ListCons head tail -> do
        headT <- abstractEval env head
        tailT <- abstractEval env tail
        case tailT of
            TList elemT
                | elemT `equalTy` headT -> pure (TList elemT)
                | otherwise             -> tyErr "Head type mismatch in list"
            _ -> tyErr "Tail must be list type"

    Both e1 e2 -> TProd <$> abstractEval env e1 <*> abstractEval env e2
        
    E1 e'                 -> undefined
    E2 e'                 -> undefined

    I1 e' -> do
        t <- abstractEval env e'
        case t of
            TProd t1 _ -> pure t1
            _ -> tyErr "Expected prod type"

    I2 e' -> do
        t <- abstractEval env e'
        case t of
            TProd _ t2 -> pure t2
            _ -> tyErr "Expected prod type"
    
    Annot body expectedT -> do
        actualT <- abstractEval env body
        if actualT `equalTy` expectedT
            then pure expectedT
            else tyErr "Type annotation does not match actual type"

    Lambda (Just paramT) (param, body) -> do
        bodyT <- abstractEval (Map.insert param paramT env) body
        pure (TFun paramT bodyT)
    Lambda Nothing _ -> tyErr "Lambda requires type annotation"

    Fix (Just expectedT) (x, body) -> do
        bodyT <- abstractEval (Map.insert x expectedT env) body
        if expectedT `equalTy` bodyT
            then pure expectedT
            else tyErr "Fix body does not match annotation"
    Fix Nothing _ -> tyErr "Fix requires type annotation"

    Let bound (name, body) -> do
        boundT <- abstractEval env bound
        abstractEval (Map.insert name boundT env) body

    ListMatch scrut nil (head, (tail, cons)) -> do
        scrutT <- abstractEval env scrut
        case scrutT of
            TList elemT -> do
                nilT <- abstractEval env nil
                let env' = Map.insert head elemT (Map.insert tail (TList elemT) env)
                consT <- abstractEval env' cons
                if nilT `equalTy` consT
                    then pure nilT
                    else tyErr "ListMatch branches must have same type"
            _ -> tyErr "ListMatch expects a list"

    Either e' (x, b1) (y, b2) ->
        case eval e' of
            E1 b1' -> eval (subst x b1' b1)
            E2 b2' -> eval (subst x b2' b2)
            _ -> undefined