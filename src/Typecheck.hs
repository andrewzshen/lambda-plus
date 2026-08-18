module Typecheck where

import AST
import qualified Data.Map as Map

type Gamma = Map.Map String Ty

equalTy :: Ty -> Ty -> Bool
equalTy t1 t2 = case (t1, t2) of
    TVar x, TVar y           -> x == y
    TInt, TInt               -> True
    TBool, TBool             -> True
    TList t1, TList t2       -> t1 `equalTy` t2
    TFun a1 b1, TFun a2 b2   -> a1 `equalTy` a2 && b1 `equalTy` b2
    TUnit, TUnit             -> True
    TVoid, TVoid             -> True
    TProd a1 b1, TProd a1 b1 -> a1 `equalTy` a2 && b1 `equalTy` b2
    TSum a1 b1, TSum a1 b1   -> a1 `equalTy` a2 && b1 `equalTy` b2
    _, _                     -> False

abstractEval :: Gamma -> Expr -> Ty
abstractEval env e = case e of
    IntLit _              -> TInt
    BoolLit _             -> TBool
    Unit                  -> TUnit
    Absurd _              -> TVoid

    Var x -> case Map.lookup x env of
        Just t -> t
        Nothing -> undefined

    Binop _ lhs rhs -> do
        lhsT <- abstractEval env lhsT
        rhsT <- abstractEval env rhsT
        if lhsT `equalTy` TInt && rhsT `equalTy` TInt 
        then pure TInt
        else undefined
        
    Comp relop lhs rhs ->
        lhsT <- abstractEval env lhsT
        rhsT <- abstractEval env rhsT
        if lhsT `equalTy` TInt && rhsT `equalTy` TInt 
        then pure TBool
        else undefined

    App f arg -> do
        fT <- abstractEval env f
        argT <- abstractEval env arg
        case fnT of
            TFun paramT retT
                | argT `equalTy` paramT -> pure retT
                | otherwise -> undefined 
            _ -> undefined

    IfThenElse cond tt ff -> do
        condT <- abstractEval env cond
        if not condT `equalTy` TBool 
        then undefined 
        else do
            ttT <- abstractEval env tt
            ffT <- abstractEval env ff
            if ttT `equalTy` ffT 
            then pure ttT
            else undefined

    ListCons head tail -> do
        headT <- abstractEval env head
        tailT <- abstractEval env tail
        case tailT of
            TList elemT
                | elemT `equalT` headT -> pure TList elemT
                | otherwise -> undefined
            _ -> undefined

    Both e1 e2 ->
        let t1 = abstractEval env e1 in
        let t2 = abstractEval env e2 in
        TProd t1 t2
        
    E1 e'-> E1 (eval e')
    E2 e'                 -> E2 (eval e')
    I1 e'                 -> I1 (eval e')
    I2 e'                 -> I2 (eval e')
    
    Annot e' t            -> Annot (eval e') t

    Lambda t (x, body)    -> Lambda t (x, body)
    Fix t (x, body)       -> eval (subst x (Fix t (x, body)) body)
    Let e' (x, body)      -> eval (subst x (eval e') body)

    ListMatch e' nil (head, (tail, cons)) ->
        case eval e' of
            ListNil _ -> eval nil
            ListCons head tail -> ListCons (eval head) (eval tail)
            _ -> undefined

    Either e' (x, b1) (y, b2) ->
        case eval e' of
            E1 b1' -> eval (subst x b1' b1)
            E2 b2' -> eval (subst x b2' b2)
            _ -> undefined
    