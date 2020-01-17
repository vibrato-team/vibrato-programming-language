module Backend.TAC.Entry where

import qualified Backend.TAC.TAC as TAC
import qualified Semantic.Data as Sem
import qualified AST
import Data.Maybe

data Entry = Entry {
    entry_name  :: String,
    entry_type  :: AST.Type
} deriving (Eq, Show)

instance TAC.SymEntryCompatible Entry where
    getSymID = entry_name

entryToTAC :: Sem.Entry -> Entry
entryToTAC e = Entry (Sem.entry_name e) (fromJust $ Sem.entry_type e)