module Utils where

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
import qualified Test.Hspec as TH
import Data.Maybe (fromJust)
import Semantic.Data

import System.Environment
import System.Exit
import System.IO

testValidProgram :: String -> IO Block
testValidProgram srcFile = do
    let lexResult = Lexer.runAlexScan srcFile
    case lexResult of
        Left err -> error err
        Right state -> case Lexer.matches state of
            Left errors -> throwCompilerError srcFile (reverse errors)
            Right tokens -> do
                -- Pre parser for declarations
                (prestate, _) <- RWS.execRWST (PreParser.preparse tokens) srcFile PMonad.initialState

                -- Parser
                (PMonad.ParserState _ table _ _ _, _) <- RWS.execRWST (Parser.parse tokens) srcFile prestate {PMonad.state_lvl = 1}
                let main = Map.lookup "moderato" table
                return $ fromJust $ function_block $ entry_category $ head $ fromJust main


