module Main where

import Lib
import qualified Lexer
import qualified Parser.Parser as Parser
import AST
import Util.Error

-- Main function. Currently it is only testing the lexer.
main :: IO ()
main = do
    srcFile <- getContents
    let lexResult = Lexer.runAlexScan srcFile in (
        case lexResult of
            Left err -> error err
            Right state -> case Lexer.matches state of
                Left errors -> throwCompilerError srcFile (reverse errors)
                Right tokens -> case Parser.parse tokens of
                    Left errors -> throwCompilerError srcFile errors
                    Right ast -> printNode 0 ast)
