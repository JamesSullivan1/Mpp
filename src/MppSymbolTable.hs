module MppSymbolTable (Sym_Table, sym_empty, sym_insert, sym_lookup,
                       sym_removescope, sym_blockscope_up, sym_returntype,
                        sym_getlevel, sym_funscope_up, sym_curlevel)
where

import qualified Data.HashMap.Lazy as M
import Debug.Trace
import MppException
import MppSymbolTypes
import MppAST

type NumV = Int
type NumA = Int
type Level = (Int, NumV, NumA, [Sym_Type], (Maybe M_type)) 

data Sym_Attr = S_Attr (Int, Sym_Info) deriving (Eq,Show)
data Sym_Table = S_Tab ([Level], Int, (M.HashMap String [Sym_Attr])) 

rtype :: Level -> E M_type
rtype (_,_,_,_,t) = case t of
    Just p -> Ok p
    _      -> Except "No return type in the current scope."

levelOf :: Sym_Attr -> Int

levelOf (S_Attr (a,_)) = a

sym_getlevel :: Sym_Table -> Level
sym_getlevel (S_Tab ((l:ls),_, _)) = l

sym_curlevel :: Sym_Table -> Int
sym_curlevel (S_Tab ((n,_,_,_,_):ls,_,_)) = n

info :: Sym_Attr -> Sym_Info
info (S_Attr (_,i)) = i

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

value :: Maybe a -> a
value (Just a)  = a
value Nothing   = error "ERROR no value"

defined :: Int -> [Level] -> [String]
defined x ls = foldr (\(_,_,_,s,_) l -> (map name s)++l) [] ls

name :: Sym_Type -> String
name (S_Arg (n,_,_)) = n
name (S_Var (n,_,_)) = n
name (S_Dat (n))     = n
name (S_Con (n,_,_)) = n
name (S_Fun (n,_,_)) = n

inLevel :: Int -> [Level] -> [String]
inLevel n ls = defined n (filter (\(x,_,_,_,_) -> x==n) ls)

sym_lookup :: String -> Sym_Table -> E Sym_Info 
sym_lookup k (S_Tab (_,_,m)) =
    case (M.lookup k m) of  
        Just (a:_)  -> Ok (info a) 
        Nothing -> exceptE ("symbol " ++ k ++ " undeclared in this block")
        _ -> error "Empty symbol lookup value"

sym_insert :: Sym_Type -> Sym_Table -> E Sym_Table
sym_insert _ (S_Tab ([],_,_)) = error "Invalid scope"
sym_insert (S_Con (nm,ms,t)) (S_Tab (l:ls,nc,m))
    | not $ nm `elem` (defined n (l:ls))
        = Ok (S_Tab (ls', nc+1,M.insert nm [a] m'))
    | otherwise
        = _sym_insert n (S_Con (nm,ms,t)) a (S_Tab (ls',nc+1,m'))
    where
        k = (S_Con (nm,ms,t))
        (n,numV,numA,ts,p) = l
        a = ((S_Attr (n, S_ConI (n, nc,ms,t))))
        ls' = (n,numV,numA,k:ts,p):ls
        as = value $ M.lookup t m
        m' = M.insert t (addTo nm as) m
        addTo a [(S_Attr (x,(S_TypI cls)))] = [(S_Attr (x,(S_TypI (cls++[a]))))]
sym_insert k (S_Tab (l:ls,nc,m))
    | not $ (name k) `elem` (defined n (l:ls))
        = Ok (S_Tab (ls', nc,M.insert ins [a] m))
    | otherwise
        = _sym_insert n' k a (S_Tab ((l:ls),nc,m))
    where
        (n,numV,numA,ts,p) = l
        n' = case k of
            (S_Fun _) -> n+1
            _         -> n
        (a,ls',ins) = case k of {- Set up stored attr and level list -}
            (S_Arg (nm,t,d)) 
                -> ((S_Attr (n, (S_VarI (n, (-(numA+4)),t,d)))),
                    (n,numV,numA+1,k:ts,p):ls, nm)
            (S_Var (nm,t,d)) 
                -> ((S_Attr (n, (S_VarI (n, (numV+1),t,d)))),
                    (n,numV+1,numA,k:ts,p):ls, nm)
            (S_Dat (nm)) 
                -> ((S_Attr (n, (S_TypI []))),
                    (n,numV,numA,k:ts,p):ls, nm)
            (S_Fun (nm,as,t))
                -> ((S_Attr (n, (S_FunI (n,nm,as,t)))),
                    (n+1,0,0,[],Just t):((n,numV,numA,k:ts,p):ls), nm)
_sym_insert n k a (S_Tab (ls,nc,m))
    | not $ (name k)  `elem` (inLevel n ls) -- Append to record
        = Ok (S_Tab (l',nc,m'))
    | otherwise -- Symbol already defined in scope, fail
        = exceptE ("Symbol " ++ show (name k) ++ 
                   " has multiple definitions in this block.")
    where
        as = value $ M.lookup (name k) m
        l = head ls
        (_,numV,numA,ts,r) = l
        l' = case k of
            (S_Arg _) -> (n,numV,numA+1,k:ts,r):(tail ls)
            (S_Var _) -> (n,numV+1,numA,k:ts,r):(tail ls)
            (S_Dat _) -> (n,numV,numA,k:ts,r):(tail ls)
            (S_Fun (_,_,p)) -> (n+1,0,0,[],Just p):((n,numV,numA,k:ts,r):(tail ls))
            (S_Con _) -> (n,numV,numA,k:ts,r):(tail ls)
        m' = M.insert (name k) (a:as) m

sym_delete :: Level -> Sym_Table -> Sym_Type -> Sym_Table 
sym_delete _ (S_Tab ([],nc,m)) _ = error "Invalid scope"
sym_delete l (S_Tab (ls,nc,m)) k
    | not in_lev    = (S_Tab (ls,nc,m))
    | otherwise     = (S_Tab (ls,nc,m')) 
    where
        (i,_,_,def,_) = l
        in_lev = k `elem` def
        a = value $ M.lookup (name k) m
        m' = M.insert (name k) (filter (\z -> levelOf z /= i) a) m 

sym_deletelev :: Level -> Sym_Table -> (Int,Sym_Table)
sym_deletelev _ (S_Tab ([],nc,m)) = error "Invalid scope"
sym_deletelev i (S_Tab (ls,nc,m)) = (numV, t')
    where
        t = (S_Tab (ls,nc,m))
        (n,numV,_,ts,_) = i
        def = foldr1 (++) (map (\(_,_,_,d,_) -> d) ls)
        t' = deleteAll def t
        deleteAll [] t = t
        deleteAll (a:as) t = deleteAll as (sym_delete i t a)

sym_removescope :: Sym_Table -> (Int,Sym_Table)
sym_removescope (S_Tab ([],nc,_)) = error "Empty scope"
sym_removescope (S_Tab (_:[],nc,_)) = error "Removing bottom scope"
sym_removescope (S_Tab (l:ls,nc,m)) = sym_deletelev l (S_Tab (ls,nc,m))

sym_blockscope_up :: Sym_Table -> Sym_Table
sym_blockscope_up (S_Tab ([],_,_)) = error "Empty scope"
sym_blockscope_up (S_Tab (l:ls,nc,m)) = (S_Tab (l':(l:ls),nc,m)) where
    (n, numV, numA, ts, p) = l
    l' = (n+1, 0, 0, [], Nothing)

sym_funscope_up :: Sym_Table -> M_type -> Sym_Table
sym_funscope_up (S_Tab ([],nc,_)) _ = error "Empty scope"
sym_funscope_up (S_Tab (l:ls,nc,m)) r = (S_Tab (l':(l:ls),nc,m)) where
    (n, numV, numA, ts, p) = l
    l' = (n+1, 0, 0, [], Just r)

sym_returntype :: Sym_Table -> E M_type
sym_returntype (S_Tab ([],_,_)) = error "Empty scope"
sym_returntype (S_Tab (l:ls,_,m)) = rtype l

level_empty :: Level
level_empty = (0,0,0,[], Nothing)

sym_empty :: Sym_Table
sym_empty = S_Tab ([level_empty],0,(M.empty))

