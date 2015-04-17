{
module MppParser where

import MppAST
import MppLexer
import MppException
import ParserPos
}

%name parse
%monad { E } { bindE } { returnE } -- Exception monad
%error { parseError }
%tokentype { Token }
%token
    '+'             { Token _ T_Add }
    '-'             { Token _ T_Sub }
    '*'             { Token _ T_Mul }
    '/'             { Token _ T_Div }
    "=>"            { Token _ T_Arrow }
    '&&'            { Token _ T_And }
    '||'            { Token _ T_Or }
    not             { Token _ T_Not }
    '='             { Token _ T_Equal }
    '<'             { Token _ T_LT }
    '>'             { Token _ T_GT }
    '=<'            { Token _ T_LE }
    '>='            { Token _ T_GE }
    ":="            { Token _ T_Assign }
    '('             { Token _ T_Lpar }
    ')'             { Token _ T_Rpar }
    '{'             { Token _ T_Clpar }
    '}'             { Token _ T_Crpar }
    '['             { Token _ T_Slpar }
    ']'             { Token _ T_Srpar }
    '|'             { Token _ T_Bar }
    ':'             { Token _ T_Colon }
    ';'             { Token _ T_Semicolon }
    ','             { Token _ T_Comma }
    if              { Token _ T_If }
    then            { Token _ T_Then }
    while           { Token _ T_While }
    do              { Token _ T_Do }
    read            { Token _ T_Read }
    else            { Token _ T_Else }
    begin           { Token _ T_Begin }
    end             { Token _ T_End }
    case            { Token _ T_Case }
    of              { Token _ T_Of }
    print           { Token _ T_Print }
    id              { Token _ (T_Identifier _) }
    constr          { Token _ (T_CIdentifier _) }
    rval            { Token _ (T_Rval _) }
    ival            { Token _ (T_Ival _) }
    cval            { Token _ (T_Cval _) }
    bval            { Token _ (T_Bval _) }
    true            { Token _ (T_Bval True) }
    false           { Token _ (T_Bval False) }
    int             { Token _ T_Int }
    real            { Token _ T_Real }
    bool            { Token _ T_Bool }
    char            { Token _ T_Char }
    var             { Token _ T_Var }
    data            { Token _ T_Data }
    size            { Token _ T_Size }
    float           { Token _ T_Float }
    floor           { Token _ T_Floor }
    ceil            { Token _ T_Ceil }
    fun             { Token _ T_Fun }
    return          { Token _ T_Return }

%%

------------------------------
-- Program flow, statements --
------------------------------

Prog : Block                                        { M_prog $1 }
Block : Decls Prog_Body                             { ($1,$2) }
Prog_Body : begin Prog_Stmts end                    { $2 }
          | Prog_Stmts                              { $1 }
Prog_Stmts : Prog_Stmt ';' Prog_Stmts               { $1 : $3 }
           |                                        { [] }
Prog_Stmt : if Expr then Prog_Stmt else Prog_Stmt   { M_cond ($2,$4,$6)
                                                        (tkPos $1) }
          | while Expr do Prog_Stmt                 { M_while ($2,$4) 
                                                        (tkPos $1) }
          | read Location                           { M_read $2 
                                                        (tkPos $1) }
          | Location ":=" Expr                      { M_ass (fst $1, snd
                                                        $1, $3) 
                                                        (tkPos $2) }
          | print Expr                              { M_print $2 
                                                        (tkPos $1) }
          | '{' Block '}'                           { M_block $2 
                                                        (tkPos $1) }
          | case Expr of '{' Case_List '}'          { M_case ($2,$5) 
                                                        (tkPos $1) }
Case_List : Case More_Case                          { $1 : $2 }
More_Case : '|' Case More_Case                      { $2 : $3 }
          |                                         { [] }
Case : constr Var_List "=>" Prog_Stmt               { (pops $1,$2,$4,
                                                        (tkPos $1)) }
Var_List : '(' Var_List1 ')'                        { $2 }
         |                                          { [] }
Var_List1 : id More_Var_List1                       { (pops $1) : $2 }
More_Var_List1 : ',' id More_Var_List1              { (pops $2) : $3 }
               |                                    { [] }
Location : id Array_Dims                            { (pops $1,$2) }

-----------------------------
--       Declarations      --
-----------------------------

Decls : Decl ';' Decls                              { $1 ++ $3 }
      |                                             { [] }
Decl  : Var_Decl                                    { $1 }
      | Fun_Decl                                    { [$1] }
      | Data_Decl                                   { [$1] }
Fun_Decl : fun id Param_List ':' Type '{' Fun_Block '}' 
                                { M_fun (pops $2, $3, $5, fst $7, snd $7) 
                                    (tkPos $1) }
Fun_Block : Decls Fun_Body                          { ($1,$2) }
Fun_Body : begin Prog_Stmts return Expr ';' end     { $2 ++ [(M_return $4
                                                        (tkPos $3))] }
         | Prog_Stmts return Expr ';'               { $1 ++ [(M_return $3
                                                        (tkPos $2))] }
Param_List : '(' Params ')'                         { $2 }
Params : Basic_Decl More_Params                     { $1 : $2 }
       |                                            { [] }
More_Params : ',' Basic_Decl More_Params            { $2 : $3 }
            |                                       { [] }
Basic_Decl : id Basic_Array_Dims ':' Type           { (pops $1, $2, $4) }
Basic_Array_Dims : '[' ']' Basic_Array_Dims         { 1 + $3 }
                 |                                  { 0 }
Var_Decl : var Var_Specs ':' Type                   { map (\sp -> M_var
                                                        (fst sp, snd sp, $4)
                                                        (tkPos $1)) $2 }
Var_Specs : Var_Spec More_Var_Specs                 { $1 : $2 }
More_Var_Specs : ',' Var_Spec More_Var_Specs        { $2 : $3 }
               |                                    { [] }
Var_Spec : id Array_Dims                            { (pops $1,$2) }
Array_Dims : '[' Expr ']' Array_Dims                { $2 : $4 }
           |                                        { [] }
Data_Decl : data id '=' Cons_Decls                  { M_data (pops $2,$4) 
                                                        (tkPos $1) }
Cons_Decls : Cons_Decl More_Cons_Decl               { $1 : $2 }
More_Cons_Decl : '|' Cons_Decl More_Cons_Decl       { $2 : $3 }
               |                                    { [] }
Cons_Decl : constr of Type_List                     { (pops $1,$3) }
          | constr                                  { (pops $1,[]) }
Type_List : Type More_Type                          { $1 : $2 } 
More_Type : '*' Type More_Type                      { $2 : $3 }
          |                                         { [] }
  
-----------------------------
--          Types          --
-----------------------------

Type : int                                          { M_int }
     | real                                         { M_real }
     | bool                                         { M_bool }
     | char                                         { M_char }
     | id                                           { usertype $1 }
-----------------------------
--       Expressions       --
-----------------------------

Expr : Expr '||' BInt_Term                          { M_app (M_or,[$1,$3]) 
                                                        (tkPos $2) }
     | BInt_Term                                    { $1 }
BInt_Term : BInt_Term '&&' BInt_Factor              { M_app (M_and,[$1,$3]) 
                                                        (tkPos $2) }
          | BInt_Factor                             { $1 }
BInt_Factor : not BInt_Factor                       { M_app (M_not,[$2])
                                                        (tkPos $1) }
            | Int_Expr Compare_Op Int_Expr          { M_app ((fst $2),
                                                        [$1,$3])
                                                        (snd $2) }
            | Int_Expr                              { $1 }
Compare_Op : '='                                    { (M_eq,tkPos $1) }
           | '<'                                    { (M_lt,tkPos $1) }
           | '>'                                    { (M_gt,tkPos $1) }
           | '=<'                                   { (M_le,tkPos $1) }
           | '>='                                   { (M_ge,tkPos $1) }
Int_Expr : Int_Expr AddOp Int_Term                  { M_app ((fst $2),
                                                        [$1,$3]) 
                                                        (snd $2) }
         | Int_Term                                 { $1 }
AddOp : '+'                                         { (M_add,tkPos $1) }
      | '-'                                         { (M_sub,tkPos $1) }
Int_Term : Int_Term MulOp Int_Factor                { M_app ((fst $2),
                                                        [$1,$3]) 
                                                        (snd $2) }
         | Int_Factor                               { $1 }
MulOp : '*'                                         { (M_mul,tkPos $1) }
      | '/'                                         { (M_div,tkPos $1) }
Int_Factor : '(' Expr ')'                           { $2 }
           | size '(' id Basic_Array_Dims ')'       { M_size (pops $3,$4)
                                                        (tkPos $1) }
           | float '(' Expr ')'                     { M_app (M_float,[$3])
                                                        (tkPos $1) }
           | floor '(' Expr ')'                     { M_app (M_floor,[$3])
                                                        (tkPos $1) }
           | ceil '(' Expr ')'                      { M_app (M_ceil,[$3])
                                                        (tkPos $1) }
           | constr Cons_Arg_List                   { M_app (M_cid 
                                                        (pops $1),$2) 
                                                        (tkPos $1)  }
           | id Modifier_List                       { idOrFn $2 (tkPos
                                                        $1) (pops $1) }
           | ival                                   { M_ival (popi $1)
                                                        (tkPos $1) }
           | rval                                   { M_rval (popr $1) 
                                                        (tkPos $1) }
           | bval                                   { M_bval (popb $1) 
                                                        (tkPos $1) }
           | cval                                   { M_cval (popc $1) 
                                                        (tkPos $1) }
           | true                                   { M_bval True 
                                                        (tkPos $1) }
           | false                                  { M_bval False 
                                                        (tkPos $1) }
           | '-' Int_Factor                         { M_app ((M_mul),
                                                        [(M_ival (-1) 
                                                        (tkPos $1)) ,$2]) 
                                                        (tkPos $1) }
Modifier_List : Fun_Arg_List                        { M_app ((M_fn "tmp"),
                                                        $1) (tmpPos) }
              | Array_Dims                          { M_id ("tmp",$1)
                                                        (tmpPos) }
Cons_Arg_List : Fun_Arg_List                        { $1 }
              |                                     { [] }
Fun_Arg_List : '(' Args ')'                         { $2 }
Args : Expr More_Args                               { $1 : $2 }
     |                                              { [] }
More_Args : ',' Expr More_Args                      { $2 : $3 }
          |                                         { [] }

{

-- Horrible hack to apply a string to either an M_id or an M_app
-- (both of which can return from Modifier_List), replacing the 
-- placeholder string in the AST node. 
--
-- Why would anyone do this? Well, until we've applied the Modifier_List
--  parse to the input, we can't know if we should make this AST node
--  an M_id or M_app. Without doing this, we can't know whether the
--  initial string is an identifier or a function, so we let
--  Modifier_List's return type tell us which to use.
--
-- Of course, since Modifier_List is going to return the same AST type
--  as we want _this_ to return, we also need to give that inner node
--  a temporary string identifying the {function|identifier} and replace
--  it with this function.
idOrFn (M_id (_,ary) _) p n  = (M_id (n,ary) p)
idOrFn (M_app (_,ary) _) p n = (M_app ((M_fn n), ary) p)

tmpPos = Pos (Line 0) (Column 0)

-- Pop the literal value from a literal-holding token.
popr (Token _ (T_Rval r)) = r :: Float
popb (Token _ (T_Bval b)) = b :: Bool
popc (Token _ (T_Cval c)) = c :: Char
popi (Token _ (T_Ival i)) = i :: Int
pops (Token _ (T_Identifier s)) = s :: String
pops (Token _ (T_CIdentifier s)) = s :: String

usertype (Token _ (T_Identifier s)) = (M_strtype s)

-- Returns an instance of the Exception monad recording the source and
--  location of a grammatical error.
parseError ts = exceptE $ genParseErrorMsg ts

-- Generates an error message for a grammatical error on the head of
--  the unparsed token list.
genParseErrorMsg ((Token pos cls):ts) = "Parsing error on token '" ++
    show cls ++ "' at " ++ show pos ++ "."

}

