module Main where

import qualified Frontend.Lexer as Lexer
import qualified Frontend.Parser.Parser as Parser
import qualified Frontend.Parser.PreParser as PreParser
import qualified Frontend.Parser.Monad as PMonad
import qualified Backend.FlowGraph.FlowGraph as FGraph
import qualified Backend.FlowGraph.Block as Block
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
                            preliminarTAC = (TAC.entryNode : TAC.ThreeAddressCode TAC.GoTo Nothing Nothing (Just $ TAC.Label "moderato") : TACMonad.backpatchAll bpMap tac) ++ [TAC.exitNode]
                            (finalTAC, blockLeaders') = FGraph.getBlockLeaders preliminarTAC
                            blockLeaders = Set.insert (length finalTAC - 1) blockLeaders'

                        putStrLn $ "Block Leaders:\n" ++ show blockLeaders ++ "\n\nThree Address Code:"

                        let labelMap = FGraph.getLabelIdxs finalTAC 0 Map.empty
                            blocksList = reverse $ FGraph.getBlocks finalTAC 0 (-1) labelMap blockLeaders Set.empty [] []

                        mapM_ (\block@Block.Block{Block.insts=tac, Block.from_idx=fromIdx} -> putStrLn "\n" >> printDelimiter >> print block >> printTAC tac fromIdx blockLeaders >> printDelimiter) blocksList

    hClose handle

printTable :: (Show a, Show b) => Map.Map a [b] -> IO ()
printTable table = mapM_ (\(str, entry) -> print str >> mapM_ (\a -> putStr "\t" >> print a) entry ) (Map.toList table)

printTAC :: [TAC.Instruction] -> Int -> Set.Set Int -> IO ()
printTAC [] _ _ = return ()
printTAC (inst:insts) line blockLeaders = do
    when (Set.member line blockLeaders) printDelimiter
    putStr $ show line ++ "\t" 
    print inst
    printTAC insts (line+1) blockLeaders

printDelimiter = putStrLn "-------------------------------------------------------------------------------"
