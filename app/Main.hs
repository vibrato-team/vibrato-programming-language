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

import System.Environment
import System.Exit
import System.IO

-- Main function. Currently it is only testing the lexer.
main :: IO ()
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode  
    srcFile <- hGetContents handle
    let lexResult = Lexer.runAlexScan srcFile in (
        case lexResult of
            Left err -> error err
            Right state -> case Lexer.matches state of
                Left errors -> throwCompilerError srcFile (reverse errors)
                Right tokens -> do
                    (table, _) <- RWS.execRWST (Parser.parse tokens) srcFile PMonad.initialState
                    print table)
    
    hClose handle
