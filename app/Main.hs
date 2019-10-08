module Main where

import Lib
import qualified Lexer
import qualified Parser
import AST

-- Wrapper for making String an instance of PrettyPrintable
newtype Wrapper = Wrapper String

-- Typeclass that has function `ppList` for showing cleaner a list of Tokens, Errors, etc.
ppList :: (Show a) => [a] -> String
ppList [] = ""
ppList [x] = show x
ppList (x:xs) = show x ++ "\n" ++ ppList xs


-- Main function. Currently it is only testing the lexer.
main :: IO ()
main = do
    srcFile <- getContents
    let lexResult = Lexer.runAlexScan srcFile in (
        case lexResult of
            Left err -> error err
            Right state -> case Lexer.matches state of
                Left errors -> error $ "Lexical errors:\n" ++ ppList errors
                Right tokens -> printNode 0 $ Parser.parse tokens)