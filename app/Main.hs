module Main where

import Lib
import qualified Lexer
import qualified Parser.Parser as Parser
import qualified Parser.PreParser as PreParser
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

-- Main function.
main :: IO ()
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode  
    srcFile <- hGetContents handle
    let lexResult = Lexer.runAlexScan srcFile
    case lexResult of
        Left err -> error err
        Right state -> case Lexer.matches state of
            Left errors -> throwCompilerError srcFile (reverse errors)
            Right tokens -> do
                (prestate, _) <- RWS.execRWST (PreParser.preparse tokens) srcFile PMonad.initialState
                (pstate, _) <- RWS.execRWST (Parser.parse tokens) srcFile prestate {PMonad.state_lvl = 1}
                printTable $ PMonad.state_table pstate
    
    hClose handle

printTable ::(Show a, Show b) => Map.Map a [b] -> IO ()
printTable table = mapM_ (\(str, entry) -> print str >> mapM_ (\a -> putStr "\t" >> print a) entry ) (Map.toList table)
