module MppIR where

import PrettyPrint

{- Intermediate Repr. for the M++ language. From M++ language spec. -}

data I_prog = I_PROG ([I_fbody],Int,[(Int,[I_expr])],[I_stmt]) deriving (Eq)
-- functions, number of local variables, array descriptions, body.
data I_fbody = I_FUN (String,[I_fbody],Int,Int,[(Int,[I_expr])],[I_stmt]) deriving (Eq)
-- functions, number of local variables, number of arguments
-- array descriptions, body
data I_stmt = I_ASS (Int,Int,[I_expr],I_expr)
-- level, offset, array indexes, expressions
    | I_WHILE (I_expr,I_stmt)
    | I_COND (I_expr,I_stmt,I_stmt)
    | I_CASE (I_expr,[(Int,Int,I_stmt)])
-- each case branch has a constructor number, a number of arguments,
-- and the code statements
    | I_READ_B (Int,Int,[I_expr])
-- level, offset, array indexes
    | I_PRINT_B I_expr
    | I_READ_I (Int,Int,[I_expr])
    | I_PRINT_I I_expr
    | I_READ_F (Int,Int,[I_expr])
    | I_PRINT_F I_expr
    | I_READ_C (Int,Int,[I_expr])
    | I_PRINT_C I_expr
    | I_RETURN I_expr
    | I_BLOCK ([I_fbody],Int,[(Int,[I_expr])],[I_stmt]) deriving (Eq)
-- functions, number of local variables, array descriptions, body.

data I_expr = I_IVAL Int
    | I_RVAL Float
    | I_BVAL Bool
    | I_CVAL Char
    | I_ID (Int,Int,[I_expr])
-- level, offset, array indices
    | I_APP (I_opn,[I_expr])
    | I_REF (Int,Int)
-- for passing an array reference as an argument
-- of a function: level, offset
    | I_SIZE (Int,Int,Int) deriving (Eq)
-- for retrieving the dimension of an array: level,offset,dimension
data I_opn = I_CALL (String,Int)
-- label and level
    | I_CONS (Int,Int)
-- constructor number and number of arguments
    | I_ADD_I | I_MUL_I | I_SUB_I | I_DIV_I | I_NEG_I
    | I_ADD_F | I_MUL_F | I_SUB_F | I_DIV_F | I_NEG_F
    | I_LT_I | I_LE_I | I_GT_I | I_GE_I | I_EQ_I
    | I_LT_F | I_LE_F | I_GT_F | I_GE_F | I_EQ_F
    | I_LT_C | I_LE_C | I_GT_C | I_GE_C | I_EQ_C
    | I_NOT | I_AND | I_OR | I_FLOAT | I_FLOOR | I_CEIL deriving (Eq,Show)

statementClosure :: I_stmt -> [I_stmt]
statementClosure s = case s of
    (I_WHILE (e,s1))    -> s:(statementClosure s1)
    (I_COND (e,s1,s2))  -> s:((statementClosure s1) ++
                          (statementClosure s2))
    (I_CASE (e,cs))     -> s:(concat (map statementClosure (caseStmts cs)))
    (I_BLOCK (fs,_,_,ss)) -> s:((concat (map statementClosure 
                                    (concat (map funStmts fs))))
                            ++ (concat (map statementClosure ss)))
    _                   -> []

caseStmts :: [(Int,Int,I_stmt)] -> [I_stmt]
caseStmts cs = (concat (map (\(_,_,s) -> [s]) cs))

funStmts :: I_fbody -> [I_stmt]
funStmts (I_FUN (_,fs,_,_,_,ss))
    = ss ++ (concat (map funStmts fs))

instance Show I_prog where
    show (I_PROG (fs,nV,as,ss)) = "I_PROG (\n" ++ indent ((showlist ",\n" fs)
        ++ show nV ++ ",\n" ++ show as ++ ",\n" ++ (showlist ",\n" ss)) ++ "\n)\n"

instance Show I_fbody where
    show (I_FUN (nm,fs,nV,nA,as,ss)) = "I_FUN " ++ nm ++ "(\n" ++ 
        indent ((showlist "\n" fs) ++ show nV ++ ",\n" ++ show nA ++ ",\n" 
        ++ show as ++ ",\n" ++ (showlist ",\n" ss))  ++ "\n)"

instance Show I_stmt where
    show (I_ASS (lv,off,ai,e)) = "I_ASS (" ++ show (lv,off,ai) ++ ",\n" ++ 
        indent (show e) ++ ")"
    show (I_WHILE (e,s)) = "I_WHILE (" ++ show e ++ ",\n" ++ indent (show s) ++ "\n)"
    show (I_COND (e,s1,s2)) = "I_COND (" ++ show e ++ ",\n" ++ 
            (indent (showlist ",\n" [s1,s2])) ++ "\n)"
    show (I_CASE (e,cs)) = "I_CASE (" ++ show e ++ "\n" ++ indent ((showlist ",\n" cs)) ++ "\n)"
    show (I_READ_B (cn,na,es)) = "I_READ_B (" ++ show (cn,na,es) ++ ")"
    show (I_PRINT_B e) = "I_PRINT_B (" ++ show e ++ ")"
    show (I_READ_I (cn,na,es)) = "I_READ_I (" ++ show (cn,na,es) ++ ")"
    show (I_PRINT_I e) = "I_PRINT_I (" ++ show e ++ ")"
    show (I_READ_F (cn,na,es)) = "I_READ_F (" ++ show (cn,na,es) ++ ")"
    show (I_PRINT_F e) = "I_PRINT_F (" ++ show e ++ ")"
    show (I_READ_C (cn,na,es)) = "I_READ_C (" ++ show (cn,na,es) ++ ")"
    show (I_PRINT_C e) = "I_PRINT_C (" ++ show e ++ ")"
    show (I_RETURN e) = "I_RETURN (" ++ show e ++ ")"
    show (I_BLOCK (fs,nV,as,ss)) = "I_BLOCK (\n" ++ indent ((showlist ",\n" fs)
        ++ show nV ++ ",\n" ++ show as ++ ",\n" ++ (showlist ",\n" ss)) ++ "\n)"

instance Show I_expr where
    show (I_IVAL i) = "I_IVAL " ++ show i
    show (I_RVAL i) = "I_RVAL " ++ show i
    show (I_BVAL i) = "I_BVAL " ++ show i
    show (I_CVAL i) = "I_CVAL " ++ show i
    show (I_ID (l,o,[])) = "I_ID (" ++ show l ++ "," ++ show o  ++ ",[])"
    show (I_ID (l,o,es)) = "I_ID (" ++ show l ++ "," ++ show o  ++ "," ++
        (showlist ", " es) ++ ")"
    show (I_APP (o,[])) = "I_APP (" ++ show o ++ ",[])"
    show (I_APP (o,es)) = "I_APP (" ++ show o ++ "," ++ 
        (showlist ", " es) ++ ")"
    show (I_REF (l,o)) = "I_REF " ++ show (l,o)
    show (I_SIZE a) = "I_SIZE " ++ show a

