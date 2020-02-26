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
import qualified Backend.TAC.Monad as TACMonad
import qualified Backend.TAC.TAC as TAC
import Control.Monad
import Data.Maybe

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
                            functionNames = PMonad.state_functions prestate
                            functionEntries = map (\name -> head $ fromJust $ Map.lookup name table) functionNames
                            concatStates (state, tac) (state', tac') = return (state', tac ++ tac')
                            acc p@(state, tac) entry = RWS.execRWST (TACMonad.genForFunction entry) () state >>= concatStates p
                            acc' p@(state, tac) genForFunction = RWS.execRWST genForFunction () state >>= concatStates p
                            functionGenerators = [TACMonad.genForMallocFunction, TACMonad.genForFreeFunction]
                        (state, tac) <- foldM acc' (TACMonad.initialState table, []) functionGenerators
                        (state, tac) <- foldM acc (state, tac) functionEntries

                        -- Backpatching
                        let bpMap = TACMonad.bp_map state
                            finalTAC = TACMonad.backpatchAll bpMap tac
                        print "Jejeje"
                        -- printTAC finalTAC
                
    
    hClose handle

printTable :: (Show a, Show b) => Map.Map a [b] -> IO ()
printTable table = mapM_ (\(str, entry) -> print str >> mapM_ (\a -> putStr "\t" >> print a) entry ) (Map.toList table)

printTAC :: [TAC.Instruction] -> IO ()
printTAC [] = return ()
printTAC (inst:insts) = do
    print inst
    printTAC insts
