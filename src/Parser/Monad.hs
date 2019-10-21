{-|
Module      : Parser.Monad
Description : All monadic operations for the parser.
License     : BSD 3-Clause "New" or "Revised" License
Maintainer  : gustavoaca1997@outlook.com, dave00dark@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Parser.Monad where

import qualified Semantic.Data as Sem
import qualified Control.Monad.RWS.Lazy as RWS
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map


-- | State of the parser
type ParserState = (Sem.ScopeSet, Sem.SymbolTable, Int)

-- | Monad of the parser
type ParserMonad = RWS.RWST String () ParserState IO

-- | Insert a new entry into the SymbolTable
insertEntry :: Sem.Entry -> ParserMonad ()
insertEntry entry = do
    (scopeSet, table, lvl) <- RWS.get
    let table' = Map.insertWith (++) (Sem.entry_name entry) [entry] table
    RWS.put (scopeSet, table', lvl)

-- | Insert a new scope/level into the set of scopeSet
insertScope :: Int -> ParserMonad ()
insertScope newScope = do
    (scopeSet, table, lvl) <- RWS.get
    let scopeSet' = Set.insert newScope scopeSet
    RWS.put (scopeSet', table, lvl)

-- | Delete a scope from the set of scopeSet
deleteScope :: Int -> ParserMonad ()
deleteScope scope = do
    (scopeSet, table, lvl) <- RWS.get
    let scopeSet' = Set.delete scope scopeSet
    RWS.put (scopeSet', table, lvl)

-- | Look up for a symbol in the symbol table and return its scope
lookup :: String -> ParserMonad (Maybe Sem.Entry)
lookup symbol = do
    (scopeSet, table, _) <- RWS.get
    -- Get chain of matching entries
    case Map.lookup symbol table of
        Nothing     -> return Nothing
        Just chain  ->
            -- Get entries that their scope is active
            case filter (\e -> Set.member (Sem.entry_scope e) scopeSet) chain of
                [] -> return Nothing
                entries -> do
                    -- Return entry with maximum scope
                    let getBest e1 e2 = if Sem.entry_scope e1 > Sem.entry_scope e2 then e1 else e2
                    return $ Just $ foldl1 getBest entries

-- | Increment level of scope
incrementScope :: ParserMonad ()
incrementScope = do
    (scopeSet, table, lvl) <- RWS.get
    RWS.put (scopeSet, table, lvl+1)