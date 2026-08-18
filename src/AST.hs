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

data BinOp = Add | Sub | Mul deriving (Eq, Show)

data RelOp = Eq | Lt | Gt deriving (Eq, Show)

type Binder a = (String, a)

data Expr 
    = IntLit Int
    | Binop BinOp Expr Expr
    | Var String
    | Lambda (Maybe Ty) (Binder Expr)
    | App Expr Expr
    | Let Expr (Binder Expr)
    | BoolLit Bool 
    | IfThenElse Expr Expr Expr
    | Comp RelOp Expr Expr
    | ListNil (Maybe Ty)
    | ListCons Expr Expr
    | ListMatch Expr Expr (Binder (Binder Expr))
    | Fix (Maybe Ty) (Binder Expr)
    | Either Expr (Binder Expr) (Binder Expr)
    | E1 Expr
    | E2 Expr
    | Both Expr Expr
    | I1 Expr
    | I2 Expr
    | Annot Expr Ty
    | Unit
    | Absurd Expr
    deriving (Eq, Show)