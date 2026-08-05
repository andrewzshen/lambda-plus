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

substitute :: String -> Expr -> Expr -> Expr
substitute x e c = case c of
    IntLit n              -> IntLit n
    BoolLit b             -> BoolLit b
    ListNil t             -> ListNil t
    Unit                  -> Unit
    Var y                 -> if x == y then e else Var y

    Binop binop lhs rhs   -> Binop binop (substitute x e lhs) (substitute x e rhs)
    Comp relop lhs rhs    -> Comp relop (substitute x e lhs) (substitute x e rhs)
    App f arg             -> App (substitute x e f) (substitute x e arg)
    IfThenElse cond tt ff -> IfThenElse (substitute x e cond) (substitute x e tt) (substitute x e ff)
    ListCons head tail    -> ListCons (substitute x e head) (substitute x e tail)
    Both e1 e2            -> Both (substitute x e e1) (substitute x e e2)
    E1 e'                 -> E1 (substitute x e e')
    E2 e'                 -> E2 (substitute x e e')
    I1 e'                 -> I1 (substitute x e e')
    I2 e'                 -> I2 (substitute x e e')
    Absurd e'             -> Absurd (substitute x e e')
    Annot e' t            -> Annot (substitute x e e') t

    Lambda e' (y, body)   -> Lambda e' (y, body')
        where body' = if x == y || y `Vars.member` (freeVars e) then body else substitute x e body

    Fix e' (y, body)      -> Fix e' (y, body')
        where body' = if x == y then body else substitute x e body

    Let e' (y, body)      -> Let (substitute x e e') (y, body')
        where body' = if x == y then body else substitute x e body

    ListMatch e' nil (head, (tail, cons)) -> ListMatch e'' nil' (head, (tail, cons'))
        where
            e'' = substitute x e e'
            nil' = substitute x e nil
            cons' = if x == head || x == tail then cons else substitute x e cons

    Either e' (y, b1) (z, b2)             -> undefined