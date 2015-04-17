{
module MppLexer where
import Data.List
import ParserPos 
}

%wrapper "monadUserState"

$alpha = [a-zA-Z]
$digit = 0-9
$quote = \"
$char = [\ a-zA-Z0-9] 

tokens :-
    <0> "%" [^\n]*              ;
    <comment> "%" [^\n]*        ;
    <0> "/*"                    { incLevel `andBegin` comment }
    <comment> "/*"              { incLevel }
    <comment> "*/"              { decLevel }
    <comment> . | \n            ;
    $white+                     ;
    <0> "+"                     { mkT T_Add }
    <0> "-"                     { mkT T_Sub }
    <0> "*"                     { mkT T_Mul }
    <0> "/"                     { mkT T_Div }
    <0> "=>"                    { mkT T_Arrow }
    <0> "&&"                    { mkT T_And }
    <0> "||"                    { mkT T_Or }
    <0> "not"                   { mkT T_Not }
    <0> "="                     { mkT T_Equal }
    <0> "<"                     { mkT T_LT }
    <0> ">"                     { mkT T_GT }
    <0> "=<"                    { mkT T_LE }
    <0> ">="                    { mkT T_GE }
    <0> ":="                    { mkT T_Assign }
    <0> "("                     { mkT T_Lpar }
    <0> ")"                     { mkT T_Rpar }
    <0> "{"                     { mkT T_Clpar }
    <0> "}"                     { mkT T_Crpar }
    <0> "["                     { mkT T_Slpar }
    <0> "]"                     { mkT T_Srpar }
    <0> "|"                     { mkT T_Bar }
    <0> ":"                     { mkT T_Colon }
    <0> ";"                     { mkT T_Semicolon }
    <0> ","                     { mkT T_Comma }
    <0> "if"                    { mkT T_If }
    <0> "then"                  { mkT T_Then }
    <0> "while"                 { mkT T_While }
    <0> "do"                    { mkT T_Do }
    <0> "read"                  { mkT T_Read }
    <0> "else"                  { mkT T_Else }
    <0> "begin"                 { mkT T_Begin }
    <0> "end"                   { mkT T_End }
    <0> "case"                  { mkT T_Case }
    <0> "of"                    { mkT T_Of }
    <0> "print"                 { mkT T_Print }
    <0> "int"                   { mkT T_Int }
    <0> "bool"                  { mkT T_Bool }
    <0> "char"                  { mkT T_Char }
    <0> "real"                  { mkT T_Real }
    <0> "true"                  { booleanT }
    <0> "false"                 { booleanF }
    <0> "var"                   { mkT T_Var }
    <0> "data"                  { mkT T_Data }
    <0> "size"                  { mkT T_Size }
    <0> "float"                 { mkT T_Float }
    <0> "floor"                 { mkT T_Floor }
    <0> "ceil"                  { mkT T_Ceil }
    <0> "fun"                   { mkT T_Fun }
    <0> "return"                { mkT T_Return }
    <0> "#"[_ $digit $alpha]*   { constructor }
    <0> $alpha [_ $digit $alpha]* { identifier }
    <0> $digit+ "." $digit+     { real }
    <0> $digit+                 { integer }
    <0> "false"                 { mkT $ T_Bval False }
    <0> "true"                  { mkT $ T_Bval True }
    <0> $quote $char $quote     { character }
    <0> $quote "\n" $quote      { mkT $ T_Cval '\n' }
    <0> $quote "\t" $quote      { mkT $ T_Cval '\t' }
    <0> .                       { lexError }

{

-- Extracts the token type, returning as an Alex Token datum.
mkT :: TokenClass -> AlexInput -> Int -> Alex Token 
mkT c (p, _, _, _) _ = return $
    Token (toPos p) c

-- Constructs a T_CIdentifier token from the currently parsed string.
constructor :: AlexInput -> Int -> Alex Token
constructor (p, _, _, inp) len = return $ 
    Token (toPos p) (T_CIdentifier (take len inp))

-- Constructs a T_Identifier token from the currently parsed string.
identifier :: AlexInput -> Int -> Alex Token
identifier (p, _, _, inp) len = return $ 
    Token (toPos p) (T_Identifier (take len inp))

-- Constructs a T_RVal token from the currently parsed string.
real :: AlexInput -> Int -> Alex Token
real (p, _, _, inp) len = return $ 
    Token (toPos p) (T_Rval (read $ take len inp :: Float))

-- Constructs a T_IVal token from the currently parsed string.
integer :: AlexInput -> Int -> Alex Token
integer (p, _, _, inp) len = return $ 
    Token (toPos p) (T_Ival (read $ take len inp :: Int))

-- Constructs a T_CVal token from the currently parsed string.
character :: AlexInput -> Int -> Alex Token
character (p, _, _, inp) len = return $
    Token (toPos p) (T_Cval ((take len inp)!!1 :: Char))
    
-- Constructs a T_BVal token from a boolean literal.
booleanT :: AlexInput -> Int -> Alex Token
booleanT (p, _, _, inp) len = return $ Token (toPos p) (T_Bval True)
booleanF (p, _, _, inp) len = return $ Token (toPos p) (T_Bval False)

data Token = Token Pos TokenClass deriving (Eq)
data TokenClass = 
      T_Ival Int
    | T_Rval Float 
    | T_CIdentifier String
    | T_Identifier String
    | T_Bval Bool
    | T_Cval Char
    | T_Add 
    | T_Sub 
    | T_Mul 
    | T_Div 
    | T_Arrow 
    | T_And 
    | T_Or 
    | T_Not 
    | T_Equal 
    | T_LT 
    | T_GT 
    | T_LE 
    | T_GE 
    | T_Assign 
    | T_Lpar 
    | T_Rpar 
    | T_Clpar 
    | T_Crpar 
    | T_Slpar 
    | T_Srpar 
    | T_Bar 
    | T_Colon 
    | T_Semicolon 
    | T_Comma 
    | T_If 
    | T_Then 
    | T_While 
    | T_Do 
    | T_Read 
    | T_Else 
    | T_Begin 
    | T_End 
    | T_Case 
    | T_Of 
    | T_Print 
    | T_Int 
    | T_Bool 
    | T_Char 
    | T_Real 
    | T_Var 
    | T_Data 
    | T_Size 
    | T_Float 
    | T_Floor 
    | T_Ceil 
    | T_Fun 
    | T_Return 
    | T_EOF
    deriving (Eq)

instance Show Token where
    show (Token p t) = "('" ++ show t ++ "', (" ++ show p ++ "))"

instance Show TokenClass where 
    show (T_Ival n) = show n
    show (T_Rval d) = show d 
    show (T_CIdentifier s) = show s
    show (T_Identifier s) = show s
    show (T_Bval b) = show b
    show (T_Cval c) = show c
    show (T_Add) = "+"
    show (T_Sub) = "-"
    show (T_Mul) = "*"
    show (T_Div) = "/"  
    show (T_Arrow) = "=>"  
    show (T_And) = "&&"  
    show (T_Or) = "||"  
    show (T_Not) = "not"
    show (T_Equal) = "="  
    show (T_LT) = "<"  
    show (T_GT) = ">"  
    show (T_LE) = "=<"  
    show (T_GE) = ">="  
    show (T_Assign) = ":="  
    show (T_Lpar) = "("  
    show (T_Rpar) = ")"  
    show (T_Clpar) = "{"  
    show (T_Crpar) = "}"
    show (T_Slpar) = "[" 
    show (T_Srpar) = "]" 
    show (T_Bar) = "|" 
    show (T_Colon) = ":" 
    show (T_Semicolon) = ";" 
    show (T_Comma) = "," 
    show (T_If) = "if" 
    show (T_Then) = "then" 
    show (T_While) = "while"
    show (T_Do) = "do"
    show (T_Read) = "read"
    show (T_Else) = "else"
    show (T_Begin) = "begin"
    show (T_End) = "end"
    show (T_Case) = "case"
    show (T_Of) = "of"
    show (T_Print) = "print"
    show (T_Int) = "int"
    show (T_Bool) = "bool"
    show (T_Char) = "char"
    show (T_Real) = "real"
    show (T_Var) = "var"
    show (T_Data) = "data"
    show (T_Size) = "size"
    show (T_Float) = "float"
    show (T_Floor) = "floor"
    show (T_Ceil) = "ceil"
    show (T_Fun) = "fun"
    show (T_Return) = "return"
    show (T_EOF) = "<EOF>"

alexEOF :: Alex Token
alexEOF = do
    (p,_,_,_) <- alexGetInput
    return $ Token (toPos p) T_EOF

toPos :: AlexPosn -> Pos
toPos (AlexPn _ l c) = Pos (Line l) (Column c)

-- Monad state data type. We only need to keep track of comment depth.
data AlexUserState = AlexUserState { 
    commentDepth :: Int 
}

-- Initialize the comment depth to 0
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { commentDepth = 0 }

-- Set the comment depth to the given integer.
setCommentDepth :: Int -> Alex ()
setCommentDepth new = Alex $
    (\st -> Right (st{alex_ust=(alex_ust st){commentDepth = new}}, ()))

-- Return the current comment depth
getCommentDepth :: Alex Int
getCommentDepth = Alex $
    (\st@AlexState{alex_ust=ust} -> Right (st, commentDepth ust))

-- Increases the comment depth by 1.
incLevel :: AlexInput -> Int -> Alex Token 
incLevel a d =
    do  cdepth <- getCommentDepth    -- Get comment depth
        setCommentDepth (cdepth + 1) -- Increment comment depth
        skip a d

-- Decreases the comment depth by 1. If this goes below 1, then
-- the '0' start code is entered and the comment depth is set to 0.
decLevel :: AlexInput -> Int -> Alex Token 
decLevel a d =
    do  cdepth <- getCommentDepth    -- Get comment depth
        setCommentDepth (cdepth - 1) -- Decrement comment depth
        if (cdepth <= 1) then do 
            alexSetStartCode 0       -- Reset start code
            setCommentDepth  0       -- Reset comment depth
            skip a d 
        else 
            skip a d

-- Raises an alexError with a useful error message indicating the
-- line and the column of the error, as well as printing the invalid
-- string.
lexError :: AlexInput -> Int -> Alex Token 
lexError (p, _, _, inp) len = alexError $ 
    "unexpected token '" ++ (take len inp) ++ "' at " ++ showPos p

-- Raises an alexError with an error message indiicating that the file
-- ended while the lexer is within a multiline comment block.
commentError = alexError $
    "file ended before the end of a multiline comment block"

-- Prints a string representation of the line and column of the given
-- AlexPn type.
showPos (AlexPn _ l c) = show l ++ ":" ++ show c

tkPos (Token p _) = p

-- Tokenize the input string according to the above token definitions.
-- On failure, 
gettokens str = runAlex str $ do
    let loop tokPairs = do 
        tok <- alexMonadScan; 
        case tok of
            Token _ T_EOF   -> do cdepth <- getCommentDepth
                                  if (cdepth > 0) then 
                                      commentError
                                  else
                                      return (reverse tokPairs)
            _               -> do loop (tok:tokPairs)
    loop []

}

