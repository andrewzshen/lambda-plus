module Eval where

import AST
import qualified Vars

freeVars :: Expr -> Vars.T
freeVars e = case e of
    IntLit _              -> Vars.empty 
    BoolLit _             -> Vars.empty 
    ListNil _             -> Vars.empty
    Unit                  -> Vars.empty
    Var x                 -> Vars.singleton x

    Binop _ lhs rhs       -> children [lhs, rhs] 
    Comp _ lhs rhs        -> children [lhs, rhs] 
    App f arg             -> children [f, arg]
    IfThenElse cond tt ff -> children [cond, tt, ff]
    ListCons head tail    -> children [head, tail]
    Both e1 e2            -> children [e1, e2]
    E1 e'                 -> children [e']
    E2 e'                 -> children [e']
    I1 e'                 -> children [e']
    I2 e'                 -> children [e']
    Absurd e'             -> children [e']
    Annot e' _            -> children [e'] 

    Lambda _ (x, body)    -> scoped [x] body
    Fix _ (x, body)       -> scoped [x] body

    Let e' (x, body)                      -> children [e'] `Vars.union` scoped [x] body
    ListMatch e' nil (head, (tail, cons)) -> children [e', nil] `Vars.union` scoped [head, tail] cons
    Either e' (x, b1) (y, b2)             -> Vars.unions [children [e'], scoped [x] b1, scoped [y] b2]

    where
        children :: [Expr] -> Vars.T 
        children = Vars.unions . map freeVars

        scoped :: [String] -> Expr -> Vars.T
        scoped bound body = Vars.deleteAll bound (freeVars body)

subst :: String -> Expr -> Expr -> Expr
subst x e = go
    where
        go :: Expr -> Expr
        go c = case c of
            IntLit n              -> IntLit n
            BoolLit b             -> BoolLit b
            ListNil t             -> ListNil t
            Unit                  -> Unit

            Var y                 -> if x == y then e else Var y
    
            Binop binop lhs rhs   -> Binop binop (go lhs) (go rhs)
            Comp relop lhs rhs    -> Comp relop (go lhs) (go rhs)
            App f arg             -> App (go f) (go arg)
            IfThenElse cond tt ff -> IfThenElse (go cond) (go tt) (go ff)
            ListCons head tail    -> ListCons (go head) (go tail)
            Both e1 e2            -> Both (go e1) (go e2)
            E1 e'                 -> E1 (go e')
            E2 e'                 -> E2 (go e')
            I1 e'                 -> I1 (go e')
            I2 e'                 -> I2 (go e')
            Absurd e'             -> Absurd (go e')
            Annot e' t            -> Annot (go e') t

            Lambda t (y, body)                   -> Lambda t (y, substBinder (y, body))
            Fix t (y, body)                      -> Fix t (y, substBinder (y, body))
            Let e' (y, body)                     -> Let (go e') (y, substBinder(y, body))

            ListMatch e' nil (head, (tail, cons)) -> ListMatch (go e') (go nil) (head, (tail, cons'))
                where
                    cons' = if x == head || x == tail then cons else go cons

            Either e' (y, b1) (z, b2)             -> Either (go e') (y, substBinder (y, b1)) (z, substBinder (z, b2))

            where 
                substBinder :: Binder Expr -> Expr
                substBinder (y, body) 
                    | x == y                       = body
                    | y `Vars.member` (freeVars e) = body 
                    | otherwise                    = go body

eval :: Expr -> Expr
eval e = case e of
    IntLit n              -> IntLit n
    BoolLit b             -> BoolLit b
    ListNil t             -> ListNil t
    Unit                  -> Unit

    Var _                 -> undefined

    Binop binop lhs rhs ->
        case (eval lhs, eval rhs) of
            (IntLit x, IntLit y) ->
                case binop of
                    Add -> IntLit (x + y)
                    Sub -> IntLit (x - y)
                    Mul -> IntLit (x * y)
            _ -> undefined

    Comp relop lhs rhs ->
        case (eval lhs, eval rhs) of
            (IntLit x, IntLit y) ->
                case relop of
                    Eq -> BoolLit (x == y)
                    Lt -> BoolLit (x < y)
                    Gt -> BoolLit (x > y)
            _ -> undefined

    App f arg -> 
        case eval f of
            Lambda _ (x, body) -> eval (subst x (eval arg) body)
            _ -> undefined

    IfThenElse cond tt ff -> 
        case eval cond of
            BoolLit b | b         -> eval tt
                      | otherwise -> eval ff
            _ -> undefined

    ListCons head tail    -> ListCons (eval head) (eval tail)
    Both e1 e2            -> Both (eval e1) (eval e2)
    E1 e'                 -> E1 (eval e')
    E2 e'                 -> E2 (eval e')
    I1 e'                 -> I1 (eval e')
    I2 e'                 -> I2 (eval e')
    Absurd e'             -> Absurd (eval e')
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
