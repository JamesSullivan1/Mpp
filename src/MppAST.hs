module MppAST where

import PrettyPrint
import ParserPos

{- AST for the M++ language. From M++ language spec. -}

data M_prog = M_prog ([M_decl],[M_stmt]) deriving (Eq)
data M_decl = M_var (String,[M_expr],M_type) Pos
    | M_fun (String,[(String,Int,M_type)],M_type,[M_decl],[M_stmt]) Pos
    | M_data (String,[(String,[M_type])]) Pos deriving (Eq)
data M_stmt = M_ass (String,[M_expr],M_expr) Pos
    | M_while (M_expr,M_stmt) Pos
    | M_cond (M_expr,M_stmt,M_stmt) Pos
    | M_read (String,[M_expr]) Pos
    | M_print M_expr Pos
    | M_return M_expr Pos
    | M_block ([M_decl],[M_stmt]) Pos
    | M_case (M_expr,[(String,[String],M_stmt,Pos)]) Pos deriving (Eq)
data M_type = M_int | M_bool | M_real | M_char | M_strtype String deriving (Eq, Show)
data M_expr = M_ival Int Pos
    | M_rval Float Pos 
    | M_bval Bool Pos
    | M_cval Char Pos 
    | M_size (String,Int) Pos
    | M_id (String,[M_expr]) Pos
    | M_app (M_operation,[M_expr]) Pos deriving (Eq) 
data M_operation = M_fn String
    | M_cid String
    | M_add | M_mul | M_sub | M_div | M_neg
    | M_lt | M_le | M_gt | M_ge | M_eq | M_not | M_and | M_or
    | M_float | M_floor | M_ceil  deriving (Eq, Show)

ePos :: M_expr -> Pos
ePos (M_ival _ p) = p
ePos (M_rval _ p) = p
ePos (M_bval _ p) = p
ePos (M_cval _ p) = p
ePos (M_size _ p) = p
ePos (M_id   _ p) = p
ePos (M_app  _ p) = p

instance Show M_prog where
    show (M_prog (ds,ss)) = "M_prog (\n" ++ indent ("M_decl (\n" ++ 
        (indent (showlist "\n"  ds)) ++ "\n) M_stmt (\n" ++ 
        (indent (showlist "\n"  ss)) ++ "\n)") ++ "\n)\n"

instance Show M_decl where
    show (M_var v _) = "M_var (" ++ show v ++ ")"
    show (M_fun (nm,args, t, ds, ss) _) = "M_fun (" ++ show nm ++ " " ++
        show args ++ " " ++ show t ++ "\n" ++ indent ("M_decl (\n" ++ 
        (indent (showlist "\n" ds)) ++ "\n) M_stmt (\n" ++
        (indent (showlist "\n" ss)) ++ "\n)") ++ "\n)"
    show (M_data (nm, ss) _) = "M_data (" ++ show nm ++ "\n" ++
        (indent (showlist "\n" ss )) ++ "\n)"

instance Show M_stmt where
    show (M_ass (nm, exprs, val) _) = "M_ass (" ++ show nm ++ ", " ++ 
        (showlist ", " exprs) ++ " " ++ show val ++ ")"
    show (M_while (e, st) _) = "M_while (" ++ show e ++ (show st) ++
        ")"
    show (M_cond (e, s1, s2) _) = "M_cond (" ++ show e ++ "\n" ++ 
        (indent (showlist "\n" [s1, s2])) ++ "\n)"
    show (M_read (s, exprs) _) = "M_read (" ++ show s ++ "\n" ++
        (indent (showlist ", " exprs)) ++ ")"
    show (M_print e _) = "M_print (" ++ show e ++ ")"
    show (M_return e _) = "M_return (" ++ show e ++ ")"
    show (M_block (ds,ss) _) = "M_block (\n" ++ indent ("M_decl (\n" ++
        (indent (showlist "\n"  ds)) ++ "\n) M_stmt (\n" ++
        (indent (showlist "\n"  ss)) ++ "\n)") ++ "\n)"
    show (M_case (e, cs) _) = "M_case (" ++ show e ++ 
        (indent ("\n" ++ (showlist "\n" cs))) ++ "\n)"
    
instance Show M_expr where
    show (M_ival i _) = "M_ival (" ++ tidy i ++ ")"
    show (M_rval r _) = "M_rval (" ++ tidy r ++ ")"
    show (M_bval b _) = "M_bval (" ++ tidy b ++ ")"
    show (M_cval c _) = "M_cval (" ++ tidy c ++ ")"
    show (M_size (nm,sz) _) = "M_size (" ++ tidy (nm,sz) ++ ")"
    show (M_id (nm, []) _) = "M_id (" ++ tidy nm ++ " [])"
    show (M_id (nm, exprs) _) = "M_id (" ++ tidy nm ++ " " ++
        (showlist ", " exprs) ++ ")"
    show (M_app (op, []) _) = "M_app (" ++ tidy op ++ " [])"
    show (M_app (op, es) _) = "M_app (" ++ tidy op ++  " " ++
        (showlist ", " es) ++ ")"


