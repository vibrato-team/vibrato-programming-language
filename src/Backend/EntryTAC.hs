module Backend.EntryTAC where

import qualified Backend.TAC as TAC
import qualified Semantic.Data as Sem
import qualified AST
import Data.Maybe

data EntryTAC = EntryTAC {
    entry_name  :: String,
    entry_type  :: AST.Type
} deriving (Eq, Show)

instance TAC.SymEntryCompatible EntryTAC where
    getSymID = entry_name

entryToTAC :: Sem.Entry -> EntryTAC
entryToTAC e = EntryTAC (Sem.entry_name e) (fromJust $ Sem.entry_type e)