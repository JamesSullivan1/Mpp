module MppCodeGen (gen_code) where

{- Code Generation for M++
   =======================

    Generates AM Stack machine code for M++ programs, given their
    intermediate representation.

    Refer to the AM specification for details of the code generation.

    Usage
    =====

    gen_code ir : Return the AM source code for the given program.

-}

import MppIR
import Debug.Trace

type LabelCounter = (Int,Int)

-- Top-level code generation function.
-- Returns the AM source code for the given IR. 
gen_code :: I_prog -> String
gen_code p = gen_prog p (0,0)

-- Generate a label, updating the label reference counter.
make_label :: LabelCounter -> (String, LabelCounter)
make_label (n,c) = ("label" ++ show (n+1) ++ ":", (n+1,c))

-- Generate a function label.
fun_label :: String -> String
fun_label nm = "fun_" ++ nm ++ ":"

-- Generate the AM source for the given IR.
gen_prog :: I_prog -> LabelCounter -> String
gen_prog (I_PROG (fs,nV,ads,ss)) l
    =  prog_prologue nV ads
    ++ stmtsCode
    ++ prog_epilogue nV ++ "\n"
    ++ funsCode ++ "\n"
    ++ innerFunsCode
    where
    (stmtsCode, l1) = gen_stmts ss l
    (funsCode, l2)  = gen_funs fs l1
    (innerFunsCode, _) = gen_innerfuns ss l2

-- Program header to set up the initial AR.
prog_prologue :: Int -> [(Int, [I_expr])] -> String
prog_prologue nV ads
    =  "\tLOAD_R %sp\n"
    ++ "\tLOAD_R %sp\n"
    ++ "\tSTORE_R %fp\n"
    ++ "\tALLOC " ++ show nV ++ "\n"
    ++ "\tLOAD_I " ++ show (nV+2) ++ "\n"
    ++ gen_arys nV ads

-- Program footer to take down the initial AR.
prog_epilogue :: Int -> String
prog_epilogue nV
    =  "\tALLOC -" ++ show nV ++ "\n"
    ++ "\tHALT\n"

-- Generate the source code to allocate stack space for all given
-- array descriptors.
gen_arys :: Int -> [(Int, [I_expr])] -> String
gen_arys nV as = ga nV as (length as) 0 where
    ga _ [] nAs _ = ""
    ga nV (a:as) nAs an 
        =  gen_ary nV a an
        ++ ga nV as nAs (an+1)

-- Generate the source code to allocate stack space for a given
-- array descriptor. Upon completion the total allocated array size
-- is added to the prior deallocation record and stored in the 
-- deallocation record's stack space.
gen_ary :: Int -> (Int, [I_expr]) -> Int -> String
gen_ary nV (o,[]) anum = ""
gen_ary nV (o,(e:es)) anum
    =  gen_expr e
    ++ gen_aryref o
    ++ gen_exprs es
    ++ gen_arysize 0 o (length (e:es))
    ++ gen_dealloc o nV anum (length (e:es))
    ++ "\tALLOC_S\n"

-- Compute the base address for the array at the given offset.
gen_aryref :: Int -> String
gen_aryref o
    =  "\tLOAD_R %sp\n"
    ++ "\tLOAD_R %fp\n"
    ++ "\tSTORE_O " ++ show o ++ "\n" 

-- Compute the total array size for the array on the TOS,
-- putting the value onto the TOS above the array dimension descriptors.
gen_arysize :: Int -> Int -> Int -> String
gen_arysize l o d
    =  _gen_arysize l o d 0
    ++ concat (replicate (d-1) "\tAPP MUL\n") where
    _gen_arysize l o d c
        | d <= c   = ""
        | otherwise  = gen_access_ptr l
                    ++ "\tLOAD_O " ++ show o ++ "\n"
                    ++ "\tLOAD_O " ++ show c ++ "\n"
                    ++ _gen_arysize l o d (c+1)

-- Generate the source code to update the deallocation record with
--  the newly added array, adding its size (currently on the TOS)
--  to the deallocation record.
gen_dealloc :: Int -> Int -> Int -> Int -> String
gen_dealloc o nV anum nDim
    =  "\tLOAD_R %fp\n"
    ++ "\tLOAD_O " ++ show (nV + 1) ++ "\n"
    ++ "\tLOAD_I " ++ show (nDim) ++ "\n"
    ++ "\tLOAD_R %fp\n"
    ++ "\tLOAD_O " ++ show o ++ "\n"
    ++ "\tLOAD_O " ++ show (nDim) ++ "\n"
    ++ "\tAPP ADD\n"
    ++ "\tAPP ADD\n"
    ++ "\tLOAD_R %fp\n"
    ++ "\tSTORE_O " ++ show (nV + 1) ++ "\n"

-- Follow back the link pointers `level` times, computing a reference
-- to the frame pointer of that past AR.
gen_access_ptr :: Int -> String
gen_access_ptr level
    =  "\tLOAD_R %fp\n"
    ++ (concat $ replicate level "\tLOAD_O -2\n")

-- Generate the source code for the given function list.
gen_funs :: [I_fbody] -> LabelCounter -> (String,LabelCounter)
gen_funs [] l = ("",l)
gen_funs (f:fs) l
    =  (funCode
    ++ funsCode,
    l2) where
    (funCode, l1) = gen_fun f l
    (funsCode, l2) = gen_funs fs l1

-- Generate the source code for the **CLOSURE** of the statement list
-- given. I.e. this walks the IR rooted at each given statement
-- and extracts all function definitions.
gen_innerfuns :: [I_stmt] -> LabelCounter -> (String, LabelCounter)
gen_innerfuns ss l = _gen_innerfuns (concat (map statementClosure ss)) l
_gen_innerfuns [] l = ("",l)
_gen_innerfuns ((I_BLOCK (fs,_,_,_)):ss) l
    =  (funCode
    ++ funsCode,
    l') where
    (funCode, l1) = gen_funs fs l
    (funsCode, l') = _gen_innerfuns ss l1
_gen_innerfuns (_:ss) l = _gen_innerfuns ss l

-- Generate the source code for the given statement list. Function
-- declarations are ignored.
gen_stmts :: [I_stmt] -> LabelCounter -> (String,LabelCounter)
gen_stmts [] l = ("",l)
gen_stmts (s:ss) l
    =  (stmtCode
    ++ stmtsCode,
    l2) where
    (stmtCode, l1) = gen_stmt s l
    (stmtsCode, l2) = gen_stmts ss l1

-- Generate the source code for the given expression list.
gen_exprs :: [I_expr] -> String
gen_exprs [] = ""
gen_exprs (e:[]) = gen_expr e
gen_exprs (e:es)
    =  gen_expr e
    ++ gen_exprs es

-- Generate the source code for the given function, included all
-- contained function declarations.
gen_fun :: I_fbody -> LabelCounter -> (String,LabelCounter)
gen_fun (I_FUN (nm,fs,nV,nA,ads,ss)) l
    =  (fun_label nm
    ++ fun_prologue nV nA ads
    ++ stmtsCode
    ++ fun_epilogue nV nA ++ "\n"
    ++ funsCode
    ++ innerFunsCode,
    l') where
    (stmtsCode, l1) = gen_stmts ss l
    (funsCode, l2) = gen_funs fs l1
    (innerFunsCode, l') = gen_innerfuns ss l2
    
-- Complete the initialization of the function AR.
fun_prologue :: Int -> Int -> [(Int,[I_expr])] -> String
fun_prologue nV nA ads
    =  "\tLOAD_R %sp\n"
    ++ "\tSTORE_R %fp\n"
    ++ "\tALLOC " ++ show nV ++ "\n"
        ++ "\tLOAD_I " ++ show (nV+2) ++ "\n"
    ++ gen_arys nV ads

-- Deallocate the function AR, and return control to the stored caller
-- address. Assumes the return value is at TOS.
fun_epilogue :: Int -> Int -> String
fun_epilogue nV nA
    =  "\tLOAD_R %fp\n"
    ++ "\tSTORE_O " ++ show (-(nA+3)) ++ "\n"
    ++ "\tLOAD_R %fp\n"
    ++ "\tLOAD_O 0\n"
    ++ "\tLOAD_R %fp\n"
    ++ "\tSTORE_O " ++ show (-(nA+2)) ++ "\n"
    ++ "\tLOAD_R %fp\n"
    ++ "\tLOAD_O " ++ show (nV+1) ++ "\n"
    ++ "\tAPP NEG\n"
    ++ "\tALLOC_S\n"
    ++ "\tSTORE_R %fp\n"
    ++ "\tALLOC -" ++ show nA ++ "\n"
    ++ "\tJUMP_S\n"

-- Generate the IR for the given statement, ignoring contained function
-- definitions.
gen_stmt :: I_stmt -> LabelCounter -> (String,LabelCounter)
gen_stmt (I_ASS (lv,o,[],e)) l
    =  (gen_expr e
    ++ gen_access_ptr lv
    ++ "\tSTORE_O " ++ show o ++ "\n",
    l)
gen_stmt (I_ASS (lv,o,aes,e)) l
    =  (gen_expr e
    ++ gen_access_ptr lv
    ++ "\tLOAD_O " ++ show o ++ "\n"
    ++ gen_aryoffset lv o aes
    ++ "\tSTORE_OS\n",
    l)
gen_stmt (I_WHILE (e,s)) l
    =  ("\tJUMP label" ++ show (fst lCNum) ++ "\n" 
    ++ startLabel
    ++ stmtCode
    ++ condLabel
    ++ exprCode
    ++ "\tJUMP_C label" ++ show (fst lSNum) ++ "\n", 
    lCNum) where
    (startLabel, lSNum) = make_label l
    (stmtCode, l1) = gen_stmt s lSNum
    (condLabel, lCNum) = make_label l1
    exprCode = gen_expr e
gen_stmt (I_COND (e,s1,s2)) l
    =  (exprCode
    ++ "\tJUMP_C label" ++ show (fst lINum) ++ "\n"
    ++ stmt2Code
    ++ "\tJUMP label" ++ show (fst lONum) ++ "\n"
    ++ ifLabel
    ++ stmt1Code
    ++ outLabel,
    lONum) where
    exprCode = gen_expr e
    (stmt2Code, l1) = gen_stmt s1 l
    (ifLabel, lINum) = make_label l1
    (stmt1Code, l2) = gen_stmt s2 lINum
    (outLabel, lONum) = make_label l2
gen_stmt (I_CASE (e,cs)) l
    =  (gen_expr e
    ++ "\tLOAD_H\n"
    ++ "\tJUMP_O\n"
    ++ cases_jumps cs l
    ++ casesCode
    ++ "return_case" ++ show (snd l + 1) ++ ":",
    l') where
    (casesCode, l') = gen_cases (reverse cs) l
gen_stmt (I_READ_B (lv,o,[])) l
    =  ("\tREAD_B\n"
    ++ gen_access_ptr lv
    ++ "\tSTORE_O " ++ show o ++ "\n",
    l)
gen_stmt (I_READ_B (lv,o,es)) l
    =  ("\tREAD_B\n"
    ++ gen_access_ptr lv
    ++ "\tLOAD_O " ++ show o ++ "\n"
    ++ gen_aryoffset lv o es
    ++ "\tSTORE_OS\n",
    l)
gen_stmt (I_PRINT_B e) l
    =  (gen_expr e
    ++ "\tPRINT_B\n",
    l)
gen_stmt (I_READ_I (lv,o,[])) l
    =  ("\tREAD_I\n"
    ++ gen_access_ptr lv
    ++ "\tSTORE_O " ++ show o ++ "\n",
    l)
gen_stmt (I_READ_I (lv,o,es)) l
    =  ("\tREAD_I\n"
    ++ gen_access_ptr lv
    ++ "\tLOAD_O " ++ show o ++ "\n"
    ++ gen_aryoffset lv o es
    ++ "\tSTORE_OS\n",
    l)
gen_stmt (I_PRINT_I e) l
    =  (gen_expr e
    ++ "\tPRINT_I\n",
    l)
gen_stmt (I_READ_F (lv,o,[])) l
    =  ("\tREAD_F\n"
    ++ gen_access_ptr lv
    ++ "\tSTORE_O " ++ show o ++ "\n",
    l)
gen_stmt (I_READ_F (lv,o,es)) l
    =  ("\tREAD_F\n"
    ++ gen_access_ptr lv
    ++ "\tLOAD_O " ++ show o ++ "\n"
    ++ gen_aryoffset lv o es
    ++ "\tSTORE_OS\n",
    l)
gen_stmt (I_PRINT_F e) l
    =  (gen_expr e
    ++ "\tPRINT_F\n",
    l)
gen_stmt (I_READ_C (lv,o,[])) l
    =  ("\tREAD_C\n"
    ++ gen_access_ptr lv
    ++ "\tSTORE_O " ++ show o ++ "\n",
    l)
gen_stmt (I_READ_C (lv,o,es)) l
    =  ("\tREAD_C\n"
    ++ gen_access_ptr lv
    ++ "\tLOAD_O " ++ show o ++ "\n"
    ++ gen_aryoffset lv o es
    ++ "\tSTORE_OS\n",
    l)
gen_stmt (I_PRINT_C e) l
    =  (gen_expr e
    ++ "\tPRINT_C\n",
    l)
gen_stmt (I_RETURN e) l
    =  (gen_expr e,
    l)
gen_stmt (I_BLOCK (fs,nV,ads,ss)) l
    =  (block_prologue nV ads
    ++ stmtCode
    ++ block_epilogue nV,
    l') where
    (stmtCode, l') = gen_stmts ss l

-- Compute the offset from the array pointer's base (i.e. the first
-- dimension) for the given set of indexing expressions. Place this
-- value on the TOS.
gen_aryoffset :: Int -> Int -> [I_expr] -> String 
gen_aryoffset l o es 
    =  "\tLOAD_I " ++ show (length es) ++ "\n"
    ++ aof l o es 0 (length es)
    ++ concat (replicate (length es) "\tAPP ADD\n") where
    aof l o [] d _ = ""
    aof l o (e:es) d (nD)
        =  gen_expr e
        ++ gen_dims l o (d+1) nD
        ++ concat (replicate (nD - (d+1)) "\tAPP MUL\n")
        ++ aof l o es (d+1) nD

-- Load all of the dimensions from d up to (nD-1) from the array's
-- stored dimension table.
gen_dims :: Int -> Int -> Int -> Int -> String
gen_dims l o d nD
    =  concat (map (\x -> gen_dim l o x) [d..(nD-1)])

-- Load the single dimension d from the stored array dimension table.
gen_dim :: Int -> Int -> Int -> String
gen_dim l o d
    =  gen_access_ptr l
    ++ "\tLOAD_O " ++ show o ++ "\n"
    ++ "\tLOAD_O " ++ show d ++ "\n"

-- Generate a case trampoline, i.e. a list of JUMPs to a particular case
-- index.
cases_jumps :: [(Int,Int,I_stmt)] -> LabelCounter -> String
cases_jumps cs l = cj cs 0 l where
    cj [] cn l = ""
    cj (c:cs) cn l
        =  "\tJUMP case" ++ show (snd l + 1) ++ "_" ++ show cn ++ "\n" 
        ++ cj cs (cn+1) l

-- Generate the source code for the given list of cases, ignoring
-- function declarations. 
gen_cases :: [(Int,Int,I_stmt)] -> LabelCounter -> (String,LabelCounter)
gen_cases cs (ln,cn) = gc cs (ln,cn+1) (cn+1) where
    gc [] l _ = ("",l)
    gc ((conn,nA,s):cs) l cn
        =  ("case" ++ show cn ++ "_" ++ show conn ++ ":" 
        ++ caseCode
        ++ casesCode,
        l')
        where
            (caseCode, l1) = gen_case (conn,nA,s) l cn 
            (casesCode, l') = gc cs l1 cn

-- Generate the source code for the given case, ignoring function
-- declarations. Sets up and takes down a case AR.
gen_case :: (Int,Int,I_stmt) -> LabelCounter -> Int -> (String,LabelCounter)
gen_case (conn,nA,s) l cn
    =  (case_prologue nA
    ++ stmtCode
    ++ case_epilogue nA
    ++ "\tJUMP return_case" ++ show cn ++ "\n",
    l') where
    (stmtCode, l') = gen_stmt s l

-- Set up a block AR.
block_prologue :: Int -> [(Int, [I_expr])] -> String
block_prologue nV ads
    =  "\tLOAD_R %fp\n"
    ++ "\tALLOC 2\n"
    ++ "\tLOAD_R %sp\n"
    ++ "\tSTORE_R %fp\n"
    ++ "\tALLOC " ++ show nV ++ "\n"
    ++ "\tLOAD_I " ++ show (nV+3) ++ "\n"
    ++ gen_arys nV ads

-- Tear down a block AR.
block_epilogue :: Int -> String
block_epilogue nV
    =  "\tLOAD_R %fp\n"
    ++ "\tLOAD_O " ++ show (nV+1) ++ "\n"
    ++ "\tAPP NEG\n"
    ++ "\tALLOC_S\n"
    ++ "\tSTORE_R %fp\n"

-- Set up a case AR.
case_prologue :: Int -> String
case_prologue nA
    =  "\tLOAD_R %fp\n"
    ++ "\tLOAD_R %fp\n"
    ++ "\tALLOC 2\n"
    ++ "\tLOAD_R %sp\n"
    ++ "\tSTORE_R %fp\n"

-- Tear down a case AR.
case_epilogue :: Int -> String
case_epilogue nA
    =  "\tALLOC -3\n"
    ++ "\tSTORE_R %fp\n"
    ++ "\tALLOC " ++ show (-nA) ++ "\n"

-- Generate the source code for the given expression.
gen_expr :: I_expr -> String
gen_expr (I_IVAL v)
    = "\tLOAD_I " ++ show v ++ "\n"
gen_expr (I_RVAL v)
    = "\tLOAD_F " ++ show v ++ "\n"
gen_expr (I_BVAL True)
    = "\tLOAD_B false\n"
gen_expr (I_BVAL False)
    = "\tLOAD_B true\n"
gen_expr (I_CVAL v)
    = "\tLOAD_C \"" ++ (mkChar v) ++ "\"\n" where
    mkChar '\n' = "\\n"
    mkChar '\t' = "\\t"
    mkChar '\r' = "\\r"
    mkChar c    = [c]
gen_expr (I_ID (l,o,[]))
    =  gen_access_ptr l
    ++ "\tLOAD_O " ++ show o ++ "\n"
gen_expr (I_ID (l,o,es))
    =  gen_access_ptr l
    ++ "\tLOAD_O " ++ show o ++ "\n"
    ++ gen_aryoffset l o es
    ++ "\tLOAD_OS\n"
gen_expr (I_APP ((I_CONS c), es))
    =  gen_exprs (reverse es)
    ++ gen_op (I_CONS c)
gen_expr (I_APP ((I_CALL f), es))
    =  gen_exprs (reverse es)
    ++ gen_op (I_CALL f)
gen_expr (I_APP (op, es))
    =  gen_exprs es
    ++ gen_op op
gen_expr (I_REF (l,o))
    =  gen_access_ptr l
    ++ "\tLOAD_O " ++ show o ++ "\n"
gen_expr (I_SIZE (l,o,d))
    =  gen_arysize l o d

-- Generate the source code for the given operand.
gen_op :: I_opn -> String
gen_op (I_CALL (nm,l))
    =  "\tALLOC 1\n"
    ++ gen_access_ptr l
    ++ "\tLOAD_R %fp\n"
    ++ "\tLOAD_R %cp\n"
    ++ "\tJUMP fun_" ++ nm ++ "\n"
gen_op (I_CONS (cn,nA))
    =  "\tLOAD_I " ++ show (cn+1) ++ "\n"
    ++ "\tSTORE_H " ++ show (nA + 1) ++ "\n"
gen_op I_ADD_I = "\tAPP ADD\n"
gen_op I_MUL_I = "\tAPP MUL\n"
gen_op I_SUB_I = "\tAPP SUB\n"
gen_op I_DIV_I = "\tAPP DIV\n"
gen_op I_NEG_I = "\tAPP NEG\n"
gen_op I_ADD_F = "\tAPP ADD_F\n"
gen_op I_MUL_F = "\tAPP MUL_F\n"
gen_op I_SUB_F = "\tAPP SUB_F\n"
gen_op I_DIV_F = "\tAPP DIV_F\n"
gen_op I_NEG_F = "\tAPP NEG_F\n"
gen_op I_LT_I  = "\tAPP LT\n"
gen_op I_LE_I  = "\tAPP LE\n"
gen_op I_GT_I  = "\tAPP GT\n"
gen_op I_GE_I  = "\tAPP GE\n"
gen_op I_EQ_I  = "\tAPP EQ\n"
gen_op I_LT_F  = "\tAPP LT_F\n"
gen_op I_LE_F  = "\tAPP LE_F\n"
gen_op I_GT_F  = "\tAPP GT_F\n"
gen_op I_GE_F  = "\tAPP GE_F\n"
gen_op I_EQ_F  = "\tAPP EQ_F\n"
gen_op I_LT_C  = "\tAPP LT_C\n"
gen_op I_LE_C  = "\tAPP LE_C\n"
gen_op I_GT_C  = "\tAPP GT_C\n"
gen_op I_GE_C  = "\tAPP GE_C\n"
gen_op I_EQ_C  = "\tAPP EQ_C\n"
gen_op I_NOT   = "\tAPP NOT\n"
gen_op I_AND   = "\tAPP AND\n"
gen_op I_OR    = "\tAPP OR\n"
gen_op I_FLOAT = "\tAPP FLOAT\n"
gen_op I_FLOOR = "\tAPP FLOOR\n"
gen_op I_CEIL  = "\tAPP CEIL\n"

