module Main where

import Lib
import qualified Lexer
import qualified Parser.Parser as Parser
import qualified Parser.Monad as PMonad
import AST
import Util.Error
import qualified Control.Monad.RWS.Lazy as RWS
import Control.Monad.Trans
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map


-- Main function. Currently it is only testing the lexer.
main :: IO ()
main = do
    srcFile <- getContents
    let lexResult = Lexer.runAlexScan srcFile in (
        case lexResult of
            Left err -> error err
            Right state -> case Lexer.matches state of
                Left errors -> throwCompilerError srcFile (reverse errors)
                Right tokens -> do
                    (ast, _) <- RWS.evalRWST (Parser.parse tokens) srcFile PMonad.initialState
                    printNode 0 ast)
