module Main where

import qualified Frontend.Lexer as Lexer
import qualified Frontend.Parser.Parser as Parser
import qualified Frontend.Parser.PreParser as PreParser
import qualified Frontend.Parser.Monad as PMonad
import AST
import Util.Error
import qualified Control.Monad.RWS.Lazy as RWS
import Control.Monad.Trans
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import qualified Semantic.Data as Sem
import qualified Backend.TAC.Monad as TACMonad

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
                -- Pre parser for declarations
                (prestate, _) <- RWS.execRWST (PreParser.preparse tokens) srcFile PMonad.initialState

                -- Parser
                (pstate, _) <- RWS.execRWST (Parser.parse tokens) srcFile prestate {PMonad.state_lvl = 1}

                -- If there is an error:
                case PMonad.state_errors pstate of
                    errs@(_:_) -> throwCompilerError srcFile $ reverse errs
                    [] -> do
                        let table = PMonad.state_table pstate
                            Just [moderatoEntry] = Map.lookup "moderato" table
                            Just astBlock = Sem.function_block $ Sem.entry_category moderatoEntry
                        (_, tac) <- RWS.execRWST (TACMonad.genForBlock astBlock) () TACMonad.initialState
                        mapM_ print tac
                
    
    hClose handle

printTable ::(Show a, Show b) => Map.Map a [b] -> IO ()
printTable table = mapM_ (\(str, entry) -> print str >> mapM_ (\a -> putStr "\t" >> print a) entry ) (Map.toList table)
