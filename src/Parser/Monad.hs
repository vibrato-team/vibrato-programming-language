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
import Util.Error

-- | State of the parser
data ParserState = ParserState {
    state_scopes :: Sem.Scopes,
    state_table :: Sem.SymbolTable,
    state_lvl :: Int,
    state_ret_type :: Maybe AST.Type,
    state_errors :: [Error]
}

-- | Monad of the parser
type ParserMonad = RWS.RWST String () ParserState IO

-- | Initial state with the level pervasive.
initialState :: ParserState
initialState = ParserState (Sem.Scopes (Set.fromList [1, 0]) [1, 0]) initialMap 1 Nothing []
    where 
        wholeEntry          =    ("whole",      [Sem.Entry "whole"      Sem.Type            0 Nothing                       Nothing])
        halfEntry           =    ("half",       [Sem.Entry "half"       Sem.Type            0 Nothing                       Nothing])
        quarterEntry        =    ("quarter",    [Sem.Entry "quarter"    Sem.Type            0 Nothing                       Nothing])
        eighthEntry         =    ("eighth",     [Sem.Entry "eighth"     Sem.Type            0 Nothing                       Nothing])
        thirySecondEntry    =    ("32th",       [Sem.Entry "32th"       Sem.Type            0 Nothing                       Nothing])
        sixtyFourthEntry    =    ("64th",       [Sem.Entry "64th"       Sem.Type            0 Nothing                       Nothing])
        nullTypeEntry       =    ("null",       [Sem.Entry "null"       Sem.Type            0 Nothing                       Nothing])
        emptyListEntry      =    ("empty_list", [Sem.Entry "empty_list" Sem.Type            0 Nothing                       Nothing])
        melodyEntry         =    ("Melody",     [Sem.Entry "Melody"     Sem.Constructor     0 Nothing                       Nothing])
        sampleEntry         =    ("Sample",     [Sem.Entry "Sample"     Sem.Constructor     0 Nothing                       Nothing])
        trueEntry           =    ("maj",        [Sem.Entry "maj"        Sem.Literal         0 (Just $ AST.Simple "whole")   Nothing])
        falseEntry          =    ("min",        [Sem.Entry "min"        Sem.Literal         0 (Just $ AST.Simple "whole")   Nothing])
        nullEntry           =    ("TT",         [Sem.Entry "TT"         Sem.Literal         0 (Just $ AST.Simple "null")    Nothing])
        initialMap          =    Map.fromList   [wholeEntry, halfEntry, quarterEntry, eighthEntry, thirySecondEntry, 
                                                sixtyFourthEntry, nullTypeEntry, melodyEntry, sampleEntry, 
                                                trueEntry, falseEntry, nullEntry]

-- | Insert a new entry into the SymbolTable
insertEntry :: Sem.Entry -> ParserMonad ()
insertEntry entry = do
    state@(ParserState _ table _ _ _) <- RWS.get
    let table' = Map.insertWith (++) (Sem.entry_name entry) [entry] table
    RWS.put $ state { state_table = table' }

-- | Update entry
updateEntry :: ([Sem.Entry] -> Maybe [Sem.Entry]) -> String -> ParserMonad ()
updateEntry f k = do
    state@(ParserState _ table _ _ _) <- RWS.get
    RWS.put $ state{ state_table = Map.update f k table }

-- | Insert a new scope/level into the set of scopeSet
pushScope :: ParserMonad ()
pushScope = do
    state@(ParserState (Sem.Scopes scopeSet scopeStack) _ lvl _ _) <- RWS.get
    let lvl' = lvl+1
    let scopes = Sem.Scopes (Set.insert lvl' scopeSet) (lvl' : scopeStack)

    RWS.put $ state{ state_scopes = scopes, state_lvl = lvl' }


-- | Remove scope
popScope :: ParserMonad ()
popScope = do
    state@(ParserState (Sem.Scopes scopeSet (h:t)) _ _ _ _) <- RWS.get
    let scopes = Sem.Scopes (Set.delete h scopeSet) t
    RWS.put $ state{ state_scopes = scopes }

-- | Get chain from a symbol
getChain :: String -> ParserMonad (Maybe [Sem.Entry])
getChain symbol = do
    (ParserState _ table _ _ _) <- RWS.get
    -- Get chain of matching entries
    return $ Map.lookup symbol table

-- | Look for a symbol in the symbol table and return its scope
lookup :: String -> ParserMonad (Maybe Sem.Entry)
lookup symbol = do
    (ParserState (Sem.Scopes scopeSet _) _ _ _ _) <- RWS.get
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
    state@(ParserState _ _ lvl _ _) <- RWS.get
    RWS.put $ state{state_lvl = lvl + 1}

-- | Entry of a Type
typeEntry :: AST.Type -> ParserMonad (Maybe Sem.Entry)
typeEntry = Parser.Monad.lookup . AST.type_str

-- | Get current scope
currScope :: ParserMonad Int
currScope = do
    (ParserState (Sem.Scopes _ (curr:_)) _ _ _ _) <- RWS.get
    return curr

-- | Add a return type to state
addReturnType :: Maybe AST.Type -> ParserMonad ()
addReturnType retType = do
    state <- RWS.get
    RWS.put $ state { state_ret_type = retType }

-- | Clear return type from state
clearReturnType = addReturnType Nothing

-- | Push an error to the state
pushError :: Error -> ParserMonad ()
pushError err = do
    state <- RWS.get
    let errs = state_errors state
    RWS.put $ state { state_errors = err : errs }