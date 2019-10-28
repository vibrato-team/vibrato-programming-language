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
import Control.Monad.Trans
import Tokens
import qualified AST

-- | State of the parser
type ParserState = (Sem.Scopes, Sem.SymbolTable, Int)

-- | Monad of the parser
type ParserMonad = RWS.RWST String () ParserState IO

-- | Initial state with the level pervasive.
initialState :: ParserState
initialState = (Sem.Scopes (Set.fromList [1, 0]) [1, 0], initialMap, 1)
    where 
        wholeEntry          =    ("whole",      [Sem.Entry "whole"      Sem.Type                    0 Nothing                       Nothing])
        halfEntry           =    ("half",       [Sem.Entry "half"       Sem.Type                    0 Nothing                       Nothing])
        quarterEntry        =    ("quarter",    [Sem.Entry "quarter"    Sem.Type                    0 Nothing                       Nothing])
        eightEntry          =    ("eight",      [Sem.Entry "eight"      Sem.Type                    0 Nothing                       Nothing])
        thirySecondEntry    =    ("32th",       [Sem.Entry "32th"       Sem.Type                    0 Nothing                       Nothing])
        sixtyFourthEntry    =    ("64th",       [Sem.Entry "64th"       Sem.Type                    0 Nothing                       Nothing])
        melodyEntry         =    ("Melody",     [Sem.Entry "Melody"     Sem.Constructor             0 Nothing                       Nothing])
        sampleEntry         =    ("Sample",     [Sem.Entry "Sample"     Sem.Constructor             0 Nothing                       Nothing])
        lengthEntry         =    ("length",     [Sem.Entry "length"     Sem.Prelude                 0 (Just $ Sem.Simple "eight")   Nothing ])
        initialMap          =    Map.fromList   [wholeEntry, halfEntry, quarterEntry, eightEntry, thirySecondEntry, sixtyFourthEntry, melodyEntry, sampleEntry, lengthEntry]

-- | Insert a new entry into the SymbolTable
insertEntry :: Sem.Entry -> ParserMonad ()
insertEntry entry = do
    (scopeSet, table, lvl) <- RWS.get
    let table' = Map.insertWith (++) (Sem.entry_name entry) [entry] table
    RWS.put (scopeSet, table', lvl)

-- | Update entry
updateEntry :: ([Sem.Entry] -> Maybe [Sem.Entry]) -> String -> ParserMonad ()
updateEntry f k = do
    (s, table, l) <- RWS.get
    RWS.put (s, Map.update f k table, l)

-- | Insert a new scope/level into the set of scopeSet
pushScope :: ParserMonad ()
pushScope = do
    (Sem.Scopes scopeSet scopeStack, table, lvl) <- RWS.get
    let lvl' = lvl+1
    let scopes = Sem.Scopes (Set.insert lvl' scopeSet) (lvl' : scopeStack)

    RWS.put (scopes, table, lvl')


-- | Remove scope
popScope :: ParserMonad ()
popScope = do
    (Sem.Scopes scopeSet (h:t), table, lvl) <- RWS.get
    let scopes = Sem.Scopes (Set.delete h scopeSet) t
    RWS.put (scopes, table, lvl)

-- | Get chain from a symbol
getChain :: String -> ParserMonad (Maybe [Sem.Entry])
getChain symbol = do
    (_, table, _) <- RWS.get
    -- Get chain of matching entries
    return $ Map.lookup symbol table

-- | Look for a symbol in the symbol table and return its scope
lookup :: String -> ParserMonad (Maybe Sem.Entry)
lookup symbol = do
    (Sem.Scopes scopeSet _, _, _) <- RWS.get
    chainMaybe <- getChain symbol
    case chainMaybe of
        Nothing     -> return Nothing
        Just chain  ->
            -- Get entries that their scope is active
            case filter (\e -> Set.member (Sem.entry_scope e) scopeSet) chain of
                [] -> return Nothing
                entries -> do
                    -- Return entry with maximum scope
                    let getBest e1 e2 = if Sem.entry_scope e1 > Sem.entry_scope e2 then e1 else e2
                    return $ Just $ foldl1 getBest entries

-- | Find if a symbol is in the scope given. This is mainly
--  used for looking fields up.    
lookupField :: String -> Int -> ParserMonad (Maybe Sem.Entry)
lookupField symbol level = do
    chainMaybe <- getChain symbol
    case chainMaybe of
        Nothing     -> return Nothing
        Just chain  ->
            -- Get entry that matches symbol
            case filter (\e -> Sem.entry_scope e == level) chain of
                [] -> return Nothing
                [e] -> return $ Just e
                _ -> return Nothing

-- | Increment level of scope
incrementScope :: ParserMonad ()
incrementScope = do
    (scopeSet, table, lvl) <- RWS.get
    RWS.put (scopeSet, table, lvl+1)

-- | Entry of a Type
typeEntry :: Sem.Type -> ParserMonad (Maybe Sem.Entry)
typeEntry = Parser.Monad.lookup . Sem.type_str

-- | Get current scope
currScope :: ParserMonad Int
currScope = do
    (Sem.Scopes _ (curr:_), _, _) <- RWS.get
    return curr