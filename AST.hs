module AST where

data Ty
    = TVar String
    | TInt
    | TBool
    | TList Ty
    | TFun Ty Ty
    | TProd Ty Ty
    | TSum Ty Ty
    | TUnit
    | TVoid
    deriving (Eq, Ord, Show)

data BinOp = Add | Sub | Mult deriving (Eq, Show)

data RelOp = Eq | Lt | Gt deriving (Eq, Show)

type Binder a = (String, a)

-- AST for Lambda+ expressions
data Expr 
    -- Arithmetic 
    = IntLit Int
    | Binop BinOp Expr Expr
    -- Binding
    | Var String
    -- Lambda Calculus
    | Lambda (Maybe Ty) (Binder Expr)
    | App Expr Expr
    -- Let Binding
    | Let Expr (Binder Expr)
    -- Booleans
    | BoolLit Bool 
    | IfThenElse Expr Expr Expr
    | Comp RelOp Expr Expr
    -- Lists
    | ListNil (Maybe Ty)
    | ListCons Expr Expr
    | ListMatch Expr Expr (Binder (Binder Expr))
    | Fix (Maybe Ty) (Binder Expr)
    -- Cases
    | Either Expr (Binder Expr) (Binder Expr)
    | E1 Expr
    | E2 Expr
    -- Product
    | Both Expr Expr
    | I1 Expr
    | I2 Expr
    -- Type Annotations
    | Annot Expr Ty
    | Unit
    -- Void
    | Absurd Expr
    deriving (Eq, Show)
