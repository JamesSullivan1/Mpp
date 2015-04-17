module MppSymbolTypes where

import MppAST
import MppException

{-                     name    type    arysize -}
data Sym_Type = S_Arg (String, M_type, Int)
{-                     name    type    arysize -}
              | S_Var (String, M_type, Int)
{-                    name -}
              | S_Dat String
{-                     name    types     utype -}
              | S_Con (String, [M_type], String)
{-                     name    argtypes        type -}
              | S_Fun (String, [(M_type,Int)], M_type) deriving (Eq, Show)

{-                      lev  off  type    dims  -}
data Sym_Info = S_VarI (Int, Int, M_type, Int)
{-                      lev  cnum argtypes  utype -}
              | S_ConI (Int, Int, [M_type], String)
{-                      lev  label   (arg,dim)       type -}
              | S_FunI (Int, String, [(M_type,Int)], M_type)
{-                      conlabels -}
              | S_TypI [String] deriving (Eq,Show)

typeOf :: Sym_Info -> E M_type
typeOf (S_VarI (_,_,t,_)) = Ok t
typeOf (S_ConI _) = exceptE "No type defined for S_ConI"
typeOf (S_FunI (_,_,_,t)) = Ok t
typeOf (S_TypI _) = exceptE "No type defined for S_TypI"

