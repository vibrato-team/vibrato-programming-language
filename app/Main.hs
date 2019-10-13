module Main where

import Lib
import qualified Lexer
import qualified Parser
import AST
import Util.Error

showErrors srcFile = map (\err -> showError srcFile (show err) (Lexer.errLine err) (Lexer.errCol err))

-- Main function. Currently it is only testing the lexer.
main :: IO ()
main = do
    srcFile <- getContents
    let lexResult = Lexer.runAlexScan srcFile in (
        case lexResult of
            Left err -> error err
            Right state -> case Lexer.matches state of
                Left errors -> putStr $ unlines (showErrors srcFile $ reverse errors)
                Right tokens -> printNode 0 $ Parser.parse tokens)