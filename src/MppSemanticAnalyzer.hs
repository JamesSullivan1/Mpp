module MppSemanticAnalyzer (ir) where

{- Semantic Analysis for M++ 
   =========================

    Generates the IR for valid programs in M++ from their AST.

    During generation, the following conditions are checked for, and
    if they fail, an error is instead raised.

    1) All variables are declared before use.
    2) All variable uses match the declared type and dimension.
    3) All functions are declared before use.
    4) All data types and constructors are declared before use.
    5) All operations are applied to correctly typed arguments,
        and have the correct number of arguments.

    Usage
    ===== 

    Just call `ir ast`, it'll handle it all for you.

-}
 

import MppAST
import MppException
import MppIR
import MppSymbolTable
import MppSymbolTypes
import ParserPos

{----------------------------
 
    Declaration Collection 
    
    Populate symbol table with contents of decls.
    Has the following effects:
        1) Returns a symbol table that includes all declarations.
        2) Returns the number of declared variables in the declarations.
        3) Returns the number of declared arguments in the declarations.
        4) Returns the array descriptions in the declarations.
    Fails if any of the declarations are ill-formed or are duplicate
    definitions.

----------------------------}

coll_decls :: [M_decl] -> Sym_Table -> E (Sym_Table,Int,Int,[(Int,[I_expr])])
coll_decls [] t = Ok (t,nV,nA,[]) where
    (_,nV,nA,_,_) = sym_getlevel t
coll_decls (d:ds) t = case d of
    (M_var (s,es,p) pos) -> case sym_insert (S_Var (s,p,length es)) t of
        Ok t'    -> case coll_decls ds t' of
            Ok (t2,nV,nA,as) -> case ir_exprs es t of
                Ok is   -> case newArrays of 
                    Ok as'   -> Ok (t2,nV,nA,as')
                    Except e -> Except e
                    where
                        newArrays = if length es == 0 then Ok as 
                                    else if isOkay b then Ok as'
                                    else Except (err b)
                        b = allInt $ type_exprs es t
                        as' = ((i,is):as)
                        v = tkVal $ sym_lookup s t'
                        i = (\(S_VarI (_,o,_,_)) -> o) v
                Except e -> Except e
            Except e -> Except e
        Except e    -> semError pos ("Error in declaration " ++
                       show (M_var (s,es,p) pos) ++ ": " ++ e)
    (M_fun (s,as,p,ps,ss) pos) -> case sym_insert (S_Fun (s,as',p)) t of
        Ok t'    -> coll_decls ds (snd $ sym_removescope t')
        Except e -> semError pos e
        where
            as' = (map (\(_,i,t) -> (t,i)) as)
    (M_data (s,cs) pos) -> case sym_insert (S_Dat s) t of
        Ok t'    -> case coll_cons cs s t' of
            Ok t2    -> coll_decls ds t2
            Except e -> Except e
        Except e -> semError pos e 

coll_args :: [(String, Int, M_type)] -> Sym_Table -> E Sym_Table
coll_args [] t           = Ok t
coll_args ((s,i,p):as) t = case sym_insert (S_Arg (s,p,i)) t of
    Ok t'    -> coll_args as t'
    Except e -> Except e

coll_cons :: [(String,[M_type])] -> String -> Sym_Table -> E Sym_Table
coll_cons [] _ t          = Ok t
coll_cons ((s,ts):cs) n t = case sym_insert (S_Con (s,ts,n)) t of
    Ok t'    -> coll_cons cs n t'
    Except e -> Except e

{----------------------------
 
    Well-Formedness Tests
    
    Test if statements of G are well formed, incl. type checking and
    declaration checking.
    A well-formed statement satisfies the following:
        1) All local variables are defined before use.
        2) All local variables match the type of their past definition.
        3) All operations are applied to the correct number of arguments.
        4) All operations are applied to the correct argument types.
        5) Any return statements match the return type of the current
            function block.

----------------------------}

wf_stmt :: M_stmt -> Sym_Table -> E Bool
wf_stmt (M_ass (s,es,e) pos) t = case sym_lookup s t of
    Ok (S_VarI (l,o,p,ds)) -> good
        where
            a = wf_exprs es t
            b = sAllInt (type_exprs es t) (M_ass (s,es,e) pos)
            c = wf_expr e t
            d = case type_expr e t of
                Ok p' -> if p' == (p,ds - length es) then Ok True 
                         else semError pos (sTypeErr (M_ass (s,es,e) pos) 
                                            (p,ds) p')
                Except e -> Except e
            good = if allOkay [a,b,c,d] then Ok True
                   else Except (firstErr [a,b,c,d])
    Except _ -> semError pos (sUndefErr (M_ass (s,es,e) pos))
    _        -> semError pos ("Error in statement " ++ 
                        show (M_ass (s,es,e) pos) ++ 
                        ": Invalid type for symbol" ++ s ++ ".")
wf_stmt (M_while (e,s) pos) t = good where
    a = wf_expr e t
    b = wf_stmt s t
    good = if allOkay [a,b] then Ok True
           else semError pos (firstErr [a,b])
wf_stmt (M_cond (e,s1,s2) pos) t = good where
    a = wf_expr e t
    b = wf_stmt s1 t
    c = wf_stmt s2 t
    good = if allOkay [a,b,c] then Ok True
           else semError pos (firstErr [a,b,c])
wf_stmt (M_read (s,es) pos) t = case sym_lookup s t of
    Ok (S_VarI (l,o,p,d)) -> case (p,d- length es) of
        (M_int,0)  -> Ok $ True
        (M_real,0) -> Ok $ True
        (M_bool,0) -> Ok $ True
        (M_char,0) -> Ok $ True
        _          -> semError pos ("Error in statement " ++ 
                            show (M_read (s,es) pos) ++ ": Unsupported type " 
                            ++ show (p,d)  ++ " for reading.")
    Except _ -> semError pos (sUndefErr (M_read (s,es) pos))
    _        -> semError pos ("Error in statement " ++ 
                    show (M_read (s,es) pos) ++
                    ": Invalid type for symbol" ++ s ++ ".")
wf_stmt (M_print e pos) t = case wf_expr e t of
    Ok _     -> case type_expr e t of
        Ok (M_int,0)  -> Ok $ True
        Ok (M_real,0) -> Ok $ True
        Ok (M_bool,0) -> Ok $ True
        Ok (M_char,0) -> Ok $ True
        Except e      -> Except e
        _             -> semError pos ("Error in statement " ++ 
                            show (M_print e pos) ++ ": Unsupported type " ++ 
                            show (tkVal $ type_expr e t)  ++ " for printing.")
    Except e -> Except e
wf_stmt (M_return e pos) t = case type_expr e t of
    Ok (p,0)    -> case sym_returntype t of 
        Ok p'    -> if p == p' then Ok True 
                    else semError pos (sRetErr (M_return e pos) p' p)
        Except e -> Except e
    Ok (p,_)    -> semError  pos ("Error in statement " ++ 
                        show (M_return e pos) ++
                        ": Returning an array is unsupported.")
    Except e -> Except e
wf_stmt (M_block (ds,ss) pos) t = case coll_decls ds (sym_blockscope_up t) of
    Ok (t1,_,_,_)    -> wf_stmts ss t1
    Except e -> Except e
wf_stmt (M_case (e,cs) pos) t
    | isOkay p =
        if isOkay n then 
            if allOkay [a,b,c] then Ok True
            else Except $ firstErr [a,b,c]
        else Except $ err n
    | otherwise = Except $ err p where
        p = type_expr e t
        n = nameOf p
        a = wf_expr e t
        b = matches p (type_cases cs (tkVal $ n) t)
        c = wf_cases cs (tkVal $ n) t
        matches m1 m2 = case (m1,m2) of
            (Ok (s1,0),Ok s2) -> if (s1 == s2) then Ok True
                             else semError pos ((sTypeErr
                                            (M_case (e,cs) pos) s2 (s1,0)))
            (Ok (s1,ds),Ok s2) -> semError pos ((sTypeErr 
                                            (M_case (e,cs) pos) s2 (s1,ds)))
            (Except e,_)      -> Except e
            (_,Except e)      -> Except e
        stmtsOf = map (\(_,_,s) -> s)
        nameOf (Ok (M_strtype s,_)) = Ok s
        nameOf (Ok _) = semError pos (sUTypeErr (M_case (e,cs) pos))

wf_stmts :: [M_stmt] -> Sym_Table -> E Bool
wf_stmts es t
    | allGood   = Ok True
    | otherwise = Except (firstErr all) where
        all = map (\e -> wf_stmt e t) es
        allGood = allOkay all

wf_case :: (String,[String],M_stmt,Pos) -> String -> Sym_Table -> E Bool
wf_case (c,as,s,pos) n t = case sym_lookup n t of
    Ok (S_TypI cs) -> case sym_lookup c t of
        Ok (S_ConI (_,_,ps,u)) -> case tmpInsert ps as (sym_blockscope_up t)  of
            Ok t' -> 
                if allOkay [a,b,x] then Ok True 
                else Except $ firstErr [a,b,x] 
                where
                    a = if c `elem` cs then Ok True 
                        else Except (conErr c n)
                    b = wf_stmt s t'
                    x = if length ps == length as then Ok True
                        else semError pos (conArgErr c (length ps) 
                                           (length as))
            Except e -> Except e
        Except e -> Except e
        _        -> semError pos (conErr c n)
    Except e -> Except e
    _        -> semError pos (conErr c n)
    where
        tmpInsert [] [] t = Ok t
        tmpInsert (p:[]) (a:[]) t = sym_insert (S_Arg (a,p,0)) t
        tmpInsert (p:ps) (a:as) t = case sym_insert (S_Arg (a,p,0)) t of
            Ok t'    -> tmpInsert ps as t'
            Except e -> Except e

wf_cases :: [(String,[String],M_stmt,Pos)] -> String -> Sym_Table -> E Bool
wf_cases cs n t
    | allGood   = Ok True
    | otherwise = Except (allErr all) where
        all = map (\c -> wf_case c n t) cs
        allGood = allOkay all

wf_expr :: M_expr -> Sym_Table -> E Bool
wf_expr (M_ival _ _) _ = Ok True
wf_expr (M_bval _ _) _ = Ok True
wf_expr (M_rval _ _) _ = Ok True
wf_expr (M_cval _ _) _ = Ok True
wf_expr (M_size (s,i) pos) t = case (sym_lookup s t) of
    Ok (S_VarI (_,_,_,d)) 
             -> case (type_expr (M_size (s,i) pos) t) of
                Ok _        -> Ok True
                Except e    -> Except e
    Except e -> Except e
    _        -> semError pos (typeErr (M_size (s,i) pos))
wf_expr (M_id (s,es) pos) t = b where
    a1 = case (sym_lookup s t) of
        Ok (S_VarI (l,o,p,d)) -> case type_expr (M_id (s,es) pos) t of
                Ok _     -> Ok True
                Except e -> Except e
        Except e -> Except e
        _        -> semError pos (typeErr (M_id (s,es) pos))
    a2 = wf_exprs es t
    b  = if (allOkay [a1,a2]) then Ok True 
         else Except (allErr [a1,a2])
wf_expr (M_app (o,es) pos) t = b where
    a1 = wf_opn o t
    a2 = wf_exprs es t
    b = if (allOkay [a1,a2]) then Ok True
        else Except (allErr [a1,a2])

wf_exprs :: [M_expr] -> Sym_Table -> E Bool
wf_exprs es t 
    | allGood   = Ok True
    | otherwise = Except (allErr all) where
        all = map (\e -> wf_expr e t) es
        allGood = allOkay all

wf_opn :: M_operation -> Sym_Table -> E Bool
wf_opn (M_fn s) t = case (sym_lookup s t) of
    Ok (S_FunI _)   -> Ok True
    Except e        -> Except e
    _               -> Except (wfOpErr (M_fn s) ++
                               "Symbol " ++ s ++ " is not a function")
wf_opn (M_cid s) t = case (sym_lookup s t) of
    Ok (S_ConI _)   -> Ok True
    Except e        -> Except e
    _               -> Except (wfOpErr (M_cid s) ++
                               "Symbol " ++ s ++ " is not a constructor")
wf_opn _ _ = Ok True

wfExprErr :: M_expr -> String
wfExprErr e = "Error in expression " ++ show e ++ ": "
wfOpErr :: M_operation -> String
wfOpErr e = "Error in operation " ++ show e ++ ": "

{----------------------------
 
    Type inference 
    
    Establish the type of an expression based on the current symbol
    table's entries, checking for typing exceptions.

----------------------------}

type_expr :: M_expr -> Sym_Table -> E (M_type,Int)
type_expr (M_ival _ _) _ = Ok (M_int,0)
type_expr (M_rval _ _) _ = Ok (M_real,0)
type_expr (M_cval _ _) _ = Ok (M_char,0)
type_expr (M_bval _ _) _ = Ok (M_bool,0)
type_expr (M_size (n,s) pos) t = case o of
    Ok (S_VarI (_,_,p,d))   -> Ok (M_int,0)
    _                       -> semError pos (typeErr $ M_size (n,s) pos)
    where
        o = sym_lookup n t
type_expr (M_id (n,es) pos) t 
    | isOkay $ allInt (type_exprs es t) = 
        case o of
            Ok (S_VarI (_,_,p,d))   -> Ok (p,d-(length es))
            _                       -> semError pos 
                                        (typeErr $ M_id (n,es) pos)
    | otherwise = Except (nonIntErr (M_id (n,es) pos))
    where
        o = sym_lookup n t
type_expr (M_app (op,es) pos) t = case type_exprs es t of
    Ok is    -> case type_opn op is t of
        Ok p     -> Ok (p,0)
        Except e -> semError pos ("Error in statement " ++ 
                        show (M_app (op,es) pos) ++ ": " ++ e)
    Except e -> Except e

type_exprs :: [M_expr] -> Sym_Table -> E [(M_type, Int)]
type_exprs [] _ = Ok []
type_exprs (e:es) t = case type_expr e t of
    Ok p     -> case type_exprs es t of 
        Ok rst   -> Ok (p:rst)
        Except e -> Except e
    Except e -> Except e

type_case :: (String,[String],M_stmt,Pos) -> String -> Sym_Table -> E M_type
type_case (c,as,s,pos) n t = case sym_lookup n t of
    Ok (S_TypI cs) -> if c `elem` cs then Ok (M_strtype n)
                      else semError pos ("Invalid constructor " ++ c ++ 
                        " for usertype " ++ n ++ ".")
    Ok _           -> semError pos ("Usertype " ++ n ++ 
                        " is not defined in scope.")
    Except e       -> Except e


type_cases :: [(String,[String],M_stmt,Pos)] -> String -> Sym_Table 
    -> E M_type
type_cases [] _ _ = error "Empty case block"
type_cases (c:[]) n t = type_case c n t
type_cases (c:cs) n t = case type_case c n t of
    Ok (M_strtype p) -> case type_cases cs n t of
        Ok (M_strtype p')    
                 -> if p == p' then Ok (M_strtype p) 
                    else Except "Invalid usertype in a case block."
        Except e -> Except e
    Except e     -> Except e
    _            -> Except "Non-usertype used in a case block."

type_opn :: M_operation -> [(M_type,Int)] -> Sym_Table -> E M_type
type_opn (M_cid s) es st = case sym_lookup s st of
    Ok (S_ConI (_,_,ps,n)) -> case args_match ps es of
        Ok _     -> Ok $ (M_strtype n)
        Except e -> Except e
        where
            args_match [] [] = Ok True
            args_match (p:ps) ((e,d):es) = if d==0&&p==e then args_match ps es
                                       else Except (conerrmsg s p e)
            args_match _ _ = Except (conNumErr s)
            conNumErr c = "Constructor " ++ c ++ " expected " ++ show (length ps) ++ 
                          " arguments, given " ++ show (length es) ++"."
    Except e      -> Except e
    _        -> Except ("Symbol " ++ s ++ " is not a constructor.")
    where
        op = (M_cid s)
        conerrmsg c e a = "Argument for constructor " ++ c ++ 
                          " has invalid type (expected " ++ show e ++ 
                          ", given " ++ show a ++ ")."
type_opn (M_fn s) es st = case sym_lookup s st of
    Ok (S_FunI (_,b,ps,r)) 
             -> case args_match ps es of
        Ok _     -> Ok $ r
        Except e -> Except e
        where
            args_match [] [] = Ok True
            args_match (p:ps) (e:es) = if p == e then args_match ps es
                                       else Except (funerrmsg s p e)
            args_match _ _ = Except (funNumErr s)
            funNumErr f = "Function " ++ f ++ " expected " ++ show (length ps) ++ 
                          " arguments, given " ++ show (length es) ++"."
    Except e -> Except e
    _        -> Except ("Symbol " ++ s ++ " is not a function.")
    where
        op = (M_fn s)
        funerrmsg f e a = "Argument for function " ++ f ++ 
                          " has invalid type (expected " ++ show e ++ 
                          ", given " ++ show a ++ ")."
type_opn op [] st = Except ("Atomic operation " ++ show op ++
                 " applied to zero arguments.")
type_opn op es st = case arithType es of
    Except e -> Except e
    Ok t     ->
        if op `elem` [M_add,M_mul,M_sub,M_div,M_neg] then case t of
            M_int   -> Ok M_int
            M_real  -> Ok M_real
            _       -> Except (errmsg "numeric" t)
        else
        if op `elem` [M_lt, M_le, M_gt, M_ge, M_eq] then case t of
            M_int   -> Ok M_int
            M_real  -> Ok M_real
            M_char  -> Ok M_char
            M_bool  -> Ok M_bool
            _       -> Except (errmsg "comparable" t)
        else
        if op `elem` [M_and, M_not, M_or] then case t of
            M_bool  -> Ok M_bool
            _       -> Except (errmsg (M_bool) t)
        else 
        if op `elem` [M_float] then case t of
            M_int   -> Ok M_real
            M_real  -> Ok M_real
            _       -> Except (errmsg "numeric" t)
        else
        if op `elem` [M_floor, M_ceil] then case t of
            M_int   -> Ok M_int
            M_real -> Ok M_int
            _       -> Except (errmsg "numeric" t)
        else error ("unimplemented operation "++ show op)
    where
    arithType :: [(M_type,Int)] -> E M_type
    arithType ((t,0):[]) = Ok t
    arithType ((t,d):[]) = Except ("Atomic operation " ++ show op ++ 
                                   " applied to array of type " ++
                                   show (t,d) ++ ".")
    arithType (t:as) = case (t,arithType as) of
        ((M_int,0), Ok M_int)    -> Ok M_int
        ((M_int,0), Ok M_real)   -> Ok M_real
        ((M_real,0), Ok M_int)   -> Ok M_real
        ((M_bool,0), Ok M_bool)  -> Ok M_bool
        ((M_char,0), Ok M_char)  -> Ok M_char
        ((M_strtype s1,0), Ok (M_strtype s2)) 
                             -> if s1 == s2 then Ok (M_strtype s1)
                                else Except ("Operation " ++ show op ++ 
                                     " applied to non-matching usertypes" 
                                     ++ show s1 ++ ", " 
                                     ++ show s2 ++ ".")
        _                    -> Except (errmsg "usertype" "non-usertype")
    errmsg e a = "Operation " ++ show op ++ " applied to illegal types (expected " 
         ++ show e ++ ", given " ++ show a ++ ")"

{----------------------------
 
    IR Generation 
    
    Produce the IRs of (well formed) sentences of G. Raises an error
    if a sentence is ill-formed.

----------------------------}

ir :: M_prog -> E I_prog
ir (M_prog (ds,ss)) = case (coll_decls ds sym_empty) of
    Ok (t1,nV,nA,ads) -> case ir_stmts ss t1 of
        Ok is    -> case ir_funs ds t1 of
            Ok fis -> Ok $ I_PROG (fis,nV,ads,is)
            Except e -> Except e
        Except e -> Except e
    Except e -> Except e

ir_fun :: M_decl -> Sym_Table -> E I_fbody
ir_fun (M_fun (s,as,p,ds,ss) pos) t = case sym_lookup s t of
    Ok (S_FunI (l,b,_,_)) -> case coll_args as t' of
        Ok t1    -> case coll_decls ds t1 of
            Ok (t2,nV,nA,ads)    -> case ir_stmts ss t2 of
                Ok is -> case ir_funs ds t2 of
                    Ok fs -> Ok $ I_FUN (b,fs,nV,nA,ads,is)
                    Except e -> Except e
                Except e -> Except e
            Except e -> Except e
        Except e -> Except e
    Except e -> semError pos e
    _ -> semError pos (sUndefDErr (M_fun (s,as,p,ds,ss) pos))
    where t' = sym_funscope_up t p

ir_funs :: [M_decl] -> Sym_Table -> E [I_fbody]
ir_funs [] _ = Ok []
ir_funs (f:fs) t = case f of
    (M_var _ _) -> ir_funs fs t
    (M_data _ _) -> ir_funs fs t
    (M_fun f pos) -> case ir_fun (M_fun f pos) t of
        Ok fb -> case ir_funs fs t of
            Ok rst -> Ok (fb:rst)
            Except e -> Except e
        Except e -> Except e

ir_stmt :: M_stmt -> Sym_Table -> E I_stmt
ir_stmt (M_ass (s, es, e) pos) t = case sym_lookup s t of
    Ok (S_VarI (l,o,p,d)) -> case ir_exprs es t of
        Ok is    -> case ir_expr e t of
            Ok i     -> Ok $ I_ASS ((sym_curlevel t) - l,o,is,i)
            Except e -> Except e
        Except e -> Except e
    Except e -> semError pos e
    _ -> semError pos (sUndefErr (M_ass (s,es,e) pos))
ir_stmt (M_while (e,s) pos) t = case ir_expr e t of
    Ok ei    -> case ir_stmt s t of
        Ok si    -> Ok $ I_WHILE (ei, si)
        Except e -> Except e
    Except e -> Except e
ir_stmt (M_cond (e,s1,s2) pos) t = case ir_expr e t of
    Ok ei     -> case ir_stmt s1 t of
        Ok si1   -> case ir_stmt s2 t of
            Ok si2   -> Ok $ I_COND (ei,si1,si2)
            Except e ->Except e
        Except e -> Except e
    Except e -> Except e 
ir_stmt (M_read (v,es) pos) t = case sym_lookup v t of
    Ok (S_VarI (l,o,p,d)) -> case ir_exprs es t of
        Ok is    -> case (p,d - length is) of
            (M_int,0)  -> Ok $ I_READ_I $ ((sym_curlevel t) - l,o,is)
            (M_real,0) -> Ok $ I_READ_F $ ((sym_curlevel t) - l,o,is)
            (M_bool,0) -> Ok $ I_READ_B $ ((sym_curlevel t) - l,o,is)
            (M_char,0) -> Ok $ I_READ_C $ ((sym_curlevel t) - l,o,is)
            _          -> error "Checking should have caught this."
        Except e -> Except e
    Except e -> semError pos e
    _ -> semError pos (sUndefErr (M_read (v,es) pos))
ir_stmt (M_print e pos) t = case ir_expr e t of
    Ok i     -> case type_expr e t of
        Ok (M_int,0)  -> Ok $ I_PRINT_I $ i
        Ok (M_real,0) -> Ok $ I_PRINT_F $ i
        Ok (M_bool,0) -> Ok $ I_PRINT_B $ i
        Ok (M_char,0) -> Ok $ I_PRINT_C $ i 
        _             -> error "Checking should have caught this."
    Except e -> Except e 
ir_stmt (M_return e pos) t = case ir_expr e t of
    Ok i     -> Ok $ I_RETURN i
    Except e -> Except e
ir_stmt (M_block (ds,ss) pos) t = case coll_decls ds (sym_blockscope_up t) of
    Ok (t1,nV,_,as) -> case ir_funs ds t1 of
        Ok fis   -> case ir_stmts ss t1 of
            Ok sis  -> Ok $ I_BLOCK (fis,nV,as,sis)
        Except e -> Except e
    Except e        -> Except e
ir_stmt (M_case (e,cs) pos) t = case ir_expr e t of
    Ok i     -> case ir_cases cs n t of
        Ok cis   -> Ok $ I_CASE (i,cis)
        Except e -> Except e
        where
            p = type_expr e t
            n = nameOf p
            nameOf (Ok (M_strtype s,_)) = s
            nameOf _ = error "Should have been caught during checks"
    Except e        -> Except e
        
ir_stmts :: [M_stmt] -> Sym_Table -> E [I_stmt]
ir_stmts [] _ = Ok []
ir_stmts (s:ss) t = case wf_stmt s t of
    Ok _     -> case ir_stmt s t of
        Ok i     -> case ir_stmts ss t of
            Ok rst   -> Ok (i:rst)
            Except e -> Except e
        Except e -> Except e
    Except e -> Except e

ir_case :: (String, [String], M_stmt,Pos) -> Sym_Table -> E (Int,Int,I_stmt)
ir_case (c,as,s,pos) t = case sym_lookup c t of
    Ok (S_ConI (l,cn,ps,u)) -> case tmpInsert ps as (sym_blockscope_up t) of
        Ok t'    -> case ir_stmt s t' of
            Ok is    -> Ok $ (cn, nA, is)
            Except e -> Except e
            where
                (_,_,nA,_,_) = sym_getlevel t'
        Except e -> semError pos e
    Except e -> semError pos e
    where
        tmpInsert [] [] t = Ok t
        tmpInsert (p:[]) (a:[]) t = sym_insert (S_Arg (a,p,0)) t
        tmpInsert (p:ps) (a:as) t = case sym_insert (S_Arg (a,p,0)) t of
            Ok t'    -> tmpInsert ps as t'
            Except e -> Except e

ir_cases :: [(String,[String],M_stmt,Pos)] -> String -> Sym_Table 
    -> E [(Int,Int,I_stmt)]
ir_cases [] _ _ = Ok []
ir_cases (c:cs) n t = case wf_case c n t of
    Ok _     -> case ir_case c t of
        Ok i     -> case ir_cases cs n t of
            Ok rst   -> Ok (i:rst)
            Except e -> Except e
        Except e -> Except e
    Except e -> Except e


ir_expr :: M_expr -> Sym_Table -> E I_expr
ir_expr (M_ival i _) _ = Ok $ I_IVAL i
ir_expr (M_rval i _) _ = Ok $ I_RVAL i
ir_expr (M_bval i _) _ = Ok $ I_BVAL i
ir_expr (M_cval i _) _ = Ok $ I_CVAL i
ir_expr (M_size (s,n) pos) t = case sym_lookup s t of
    Ok (S_VarI (l,o,p,d))
             -> Ok $ I_SIZE ((sym_curlevel t) - l,o,d)
    Except e -> semError pos e
    _        -> semError pos (undefErr (M_size (s,n) pos)) 
ir_expr (M_id (s,es) pos) t = case sym_lookup s t of
    Ok (S_VarI (l,o,p,d))
             -> case ir_exprs es t of
                Ok is    -> if d == (length is) then
                                Ok $ I_ID ((sym_curlevel t) - l,o,is)
                            else
                                semError pos (aryLenErr (M_id (s,es) pos))
                Except e -> Except e
    Except e -> semError pos e
    _        -> semError pos (undefErr (M_id (s,es) pos)) 
ir_expr (M_app (op,es) pos) t = case type_exprs es t of
    Ok ts    -> case type_opn op ts t of
        Ok p     -> case ir_opn op p t of
            Ok i     -> case ir_args es op t of
                Ok is    -> Ok $ I_APP (i,is) 
                Except e -> Except e
            Except e -> Except e
        Except e -> error "Checking should have caught this" 
    Except e -> error "Checking should have caught this"

{- Special case - generating I_REF for arrays passed into function calls -}
ir_arg (M_id (s,es) pos) t = case sym_lookup s t of
    Ok (S_VarI (l,o,p,d)) -> case ir_exprs es t of
        Ok is    -> if d > 0 && length es == 0 then
                        Ok $ I_REF ((sym_curlevel t) - l,o)
                    else ir_expr (M_id (s,es) pos) t
        Except e -> Except e
    Except e -> semError pos e
    _        -> semError pos (undefErr (M_id (s,es) pos)) 
ir_arg m t = ir_expr m t

{- Uses ir_arg if the operation is a function call, otherwise calls ir_exprs 
 -}
ir_args [] _ _ = Ok []
ir_args (e:es) o t = case o of
    (M_fn s) -> case ir_arg e t of
        Ok i    -> case ir_args es o t of
            Ok rst   -> Ok (i:rst)
            Except e -> Except e
        Except e-> Except e
    _ -> ir_exprs (e:es) t

ir_exprs [] _ = Ok []
ir_exprs (e:es) t = case wf_expr e t of
    Ok _    -> case ir_expr e t of
        Ok i     -> case ir_exprs es t of
            Ok rst   -> Ok (i:rst)
            Except e -> Except e
        Except e -> error "This should have been caught by checking."
    Except e -> Except e

ir_opn (M_fn s) _ t = case sym_lookup s t of
    Ok (S_FunI (l,b,_,_)) -> Ok $ I_CALL (b,(sym_curlevel t) - l)
    _ -> Except (undefOpErr (M_fn s))
ir_opn (M_cid s) _ t = case sym_lookup s t of
    Ok (S_ConI (l,c,as,u)) -> Ok $ I_CONS (c, length as)
    _ -> Except (undefOpErr (M_cid s))
ir_opn op p t = Ok opn where 
    opn = case op of
        M_add -> if p == M_int then I_ADD_I else I_ADD_F
        M_mul -> if p == M_int then I_MUL_I else I_MUL_F
        M_sub -> if p == M_int then I_SUB_I else I_SUB_F
        M_div -> if p == M_int then I_DIV_I else I_DIV_F
        M_neg -> if p == M_int then I_NEG_I else I_NEG_F
        M_lt  -> if p == M_int then I_LT_I else
                    if p == M_real then I_LT_F else I_LT_C
        M_le  -> if p == M_int then I_LE_I else
                    if p == M_real then I_LE_F else I_LE_C
        M_gt  -> if p == M_int then I_GT_I else
                    if p == M_real then I_GT_F else I_GT_C
        M_ge  -> if p == M_int then I_GE_I else
                    if p == M_real then I_GE_F else I_GE_C
        M_eq  -> if p == M_int then I_EQ_I else
                    if p == M_real then I_EQ_F else I_EQ_C
        M_not -> I_NOT
        M_and -> I_AND
        M_or  -> I_OR
        M_float -> I_FLOAT
        M_floor -> I_FLOOR
        M_ceil  -> I_CEIL

allInt :: E [(M_type,Int)] -> E Bool
allInt (Except e) = Except e 
allInt (Ok []) = Ok True
allInt (Ok (e:es)) = case e of
    (M_int,0) -> allInt (Ok es)
    _         -> Except "Non-integer used as an array index."

sAllInt (Except e) _ = Except e 
sAllInt (Ok []) _ = Ok True
sAllInt (Ok (e:es)) s = case e of
    (M_int,0) -> sAllInt (Ok es) s
    _          -> Except (sNonIntErr s)

typeErr :: M_expr -> String
typeErr e = "Type error in expression " ++ show e ++ ": " ++ t where 
    t = case e of
        (M_size (n,s) _)  -> "Invalid type for symbol " ++ show n
        (M_id   (n,es) _) -> "Invalid type for symbol " ++ show n

nonIntErr e = "Type error in expression " ++ show e ++ 
    ": non-integer index applied to array " ++ t where
    t = case e of
        (M_size (n,s) _) -> n
        (M_id (n,es) _) -> n
        _ -> error "unsupported type" 

sNonIntErr e = "Type error in statement " ++ show e ++
    ": non-integer index applied to array " ++ t where
    t = case e of
        (M_ass (s,_,_) _)-> s
        _ -> error "unsupported type" 

undefErr :: M_expr -> String
undefErr (M_size (nm,d) pos) = "Error in expression " ++ 
    show (M_size (nm,d) pos) ++ ": Variable " ++ nm ++ 
    " is not defined in scope."
undefErr (M_id (nm,es) pos) = "Error in expression " ++ 
    show (M_id (nm,es) pos) ++ ": Variable " ++ nm ++ 
    " is not defined in scope."

sUndefErr s = "Error in statement " ++ show s ++ ": Variable " ++ nm ++
    " is not defined in scope." where
    nm = case s of
        (M_ass (a,_,_) _) -> a
        (M_read (a,_) _)  -> a
        _ -> error "Unsupported type"
sUndefDErr s = "Error in declaration " ++ show s ++ ": Variable " ++ nm ++
    " is not defined in scope." where
    nm = case s of
        (M_var (a,_,_) _) -> a
        (M_fun (a,_,_,_,_) _)  -> a
        (M_data (a,_) _) -> a

undefOpErr (M_fn nm) = "Error in expression " ++ show (M_fn nm)
    ++ ": Function " ++ nm ++ " is not defined in scope."
undefOpErr (M_cid nm) = "Error in expression " ++ show (M_cid nm)
    ++ ": Constructor " ++ nm ++ " is not defined in scope."

aryLenErr (M_id (nm,es) pos) = "Error in expression " ++ 
    show (M_id (nm,es) pos) ++ 
    ": Incompatible number of indices for array " ++nm ++ "."

sTypeErr (M_ass (s,es,e) pos) p a  = "Type error in statement " 
    ++ show (M_ass (s,es,e) pos) ++ ": Invalid type for symbol " ++ s
    ++ " (expected " ++ show p ++ ", given " ++ show a ++ ")."
sTypeErr (M_case (s,cs) pos) p a = "Type error in statement "
    ++ show (M_case (s,cs) pos) ++ ": Invalid type for symbol " ++ show s
    ++ " (expected " ++ show p ++ ", given " ++ show a ++ ")."
    

sUTypeErr (M_case (e,cs) pos) = "Type error in statement " ++ 
    show (M_case (e,cs) pos) ++ ": Invalid usertype in case block."

sRetErr (M_return (s) pos) e a = "Error in statement " ++ 
    show (M_return s pos) ++ 
    ": Invalid return type for this function scope (expected " ++ show e ++ 
    ", given " ++ show a ++ ")."

conErr c n = "Error: " ++ c ++ " is not a valid constructor for usertype "
    ++ n ++ "."

conArgErr c e a = "Error in constructor " ++ c ++ ": Expected " ++ show e
    ++ " arguments, received " ++ show a ++ "."

semError :: Pos -> String -> E a
semError p s = Except (show p ++ ": " ++ s)

