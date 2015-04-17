{-
    M++ Parser 
    ================
    James Sullivan
    <sullivan.james.f@gmail.com>

    Usage
    =====

    ./mpp file output

    Emits the IR for the given M++ source to 'output'. 
    Displays an error message if the source code is invalid.

-}

module Main where


import Control.Exception
import Data.List.Split
import Data.Either
import MppAST
import MppCodeGen
import MppException
import MppIR
import MppLexer
import MppParser
import MppSemanticAnalyzer
import MppSymbolTable
import ParserPos
import System.Environment
import System.IO

main = do
    args <- getArgs 
    if length args < 2 then
        error "Usage: ./mpp file output"
    else do 
        let fn = head args
        let on = head $ tail args
        file  <- openFile fn ReadMode
        s <- hGetContents file
        let t = gettokens s
        case t of
            Left e  -> error $ fn ++ ": " ++ "Lexical error: " ++ 
                posErr e (lines s) 
            Right l -> do
                let pt = parse l
                case pt of
                    Ok ast   -> case ir ast of
                        Ok ir    -> do
                            writeFile on $ (gen_code ir)
                        Except e -> error $ (fn ++ ": " ++ posErr e (lines s))
                    Except e -> error (fn ++ ": " ++ "Grammatical error: " 
                        ++ posErr e (lines s))

posErr :: String -> [String] -> String
posErr e ls = e ++ "\n\nAt this line:\n" ++ getSourceLine (getPos e) ls

