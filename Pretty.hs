module Pretty where

import AST

ty :: Ty -> String
ty (TVar x)      = x 
ty TInt          = "Int"
ty TBool         = "Bool"
ty (TList t)     = "List[]" 
ty (TFun t1 t2)  = "" 
ty (TProd t1 t2) = "" 
ty (TSum t1 t2)  = ""
ty TUnit         = "()"
ty TVoid         = "!" 
