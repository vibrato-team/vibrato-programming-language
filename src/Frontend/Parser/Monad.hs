{-|
Module      : Frontend.Parser.Monad
Description : All monadic operations for the parser.
License     : BSD 3-Clause "New" or "Revised" License
Maintainer  : gustavoaca1997@outlook.com, dave00dark@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Frontend.Parser.Monad where

import qualified Control.Monad.RWS.Lazy as RWS
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Control.Monad.Trans
import Frontend.Tokens
import qualified AST
import Util.Error

-- | State of the parser
data ParserState = ParserState {
    state_scopes :: AST.Scopes,
    state_table :: AST.SymbolTable,
    state_lvl :: Int,
    state_ret_type :: Maybe AST.ASTType,
    state_errors :: [Error]
}

-- | Monad of the parser
type ParserMonad = RWS.RWST String () ParserState IO

-- | Initial state with the level pervasive.
initialState :: ParserState
initialState = ParserState (AST.Scopes (Set.fromList [1, 0]) [1, 0]) initialMap 1 Nothing []
    where 
        wholeEntry          =    ("whole",      [AST.Entry "whole"      (AST.Type Nothing Nothing)    0 Nothing                       Nothing])
        halfEntry           =    ("half",       [AST.Entry "half"       (AST.Type Nothing Nothing)    0 Nothing                       Nothing])
        quarterEntry        =    ("quarter",    [AST.Entry "quarter"    (AST.Type Nothing Nothing)    0 Nothing                       Nothing])
        eighthEntry         =    ("eighth",     [AST.Entry "eighth"     (AST.Type Nothing Nothing)    0 Nothing                       Nothing])
        thirySecondEntry    =    ("32th",       [AST.Entry "32th"       (AST.Type Nothing Nothing)    0 Nothing                       Nothing])
        sixtyFourthEntry    =    ("64th",       [AST.Entry "64th"       (AST.Type Nothing Nothing)    0 Nothing                       Nothing])
        nullTypeEntry       =    ("null",       [AST.Entry "null"       (AST.Type Nothing Nothing)    0 Nothing                       Nothing])
        emptyListEntry      =    ("empty_list", [AST.Entry "empty_list" (AST.Type Nothing Nothing)    0 Nothing                       Nothing])
        melodyEntry         =    ("Melody",     [AST.Entry "Melody"     AST.Constructor     0 Nothing                       Nothing])
        sampleEntry         =    ("Sample",     [AST.Entry "Sample"     AST.Constructor     0 Nothing                       Nothing])
        trueEntry           =    ("maj",        [AST.Entry "maj"        AST.Literal         0 (Just $ AST.Simple "whole")   Nothing])
        falseEntry          =    ("min",        [AST.Entry "min"        AST.Literal         0 (Just $ AST.Simple "whole")   Nothing])
        nullEntry           =    ("TT",         [AST.Entry "TT"         AST.Literal         0 (Just $ AST.Simple "null")    Nothing])
        initialMap          =    Map.fromList   [wholeEntry, halfEntry, quarterEntry, eighthEntry, thirySecondEntry, 
                                                sixtyFourthEntry, nullTypeEntry, melodyEntry, sampleEntry, 
                                                trueEntry, falseEntry, nullEntry]

-- | Insert a new entry into the SymbolTable
insertEntry :: AST.Entry -> ParserMonad ()
insertEntry entry = do
    state@(ParserState _ table _ _ _) <- RWS.get
    let table' = Map.insertWith (++) (AST.entry_name entry) [entry] table
    RWS.put $ state { state_table = table' }

-- | Update entry
updateEntry :: ([AST.Entry] -> Maybe [AST.Entry]) -> String -> ParserMonad ()
updateEntry f k = do
    state@(ParserState _ table _ _ _) <- RWS.get
    RWS.put $ state{ state_table = Map.update f k table }

-- | Insert a new scope/level into the set of scopeSet
pushScope :: ParserMonad ()
pushScope = do
    state@(ParserState (AST.Scopes scopeSet scopeStack) _ lvl _ _) <- RWS.get
    let lvl' = lvl+1
    let scopes = AST.Scopes (Set.insert lvl' scopeSet) (lvl' : scopeStack)

    RWS.put $ state{ state_scopes = scopes, state_lvl = lvl' }


-- | Remove scope
popScope :: ParserMonad ()
popScope = do
    state@(ParserState (AST.Scopes scopeSet (h:t)) _ _ _ _) <- RWS.get
    let scopes = AST.Scopes (Set.delete h scopeSet) t
    RWS.put $ state{ state_scopes = scopes }

-- | Get chain from a symbol
getChain :: String -> ParserMonad (Maybe [AST.Entry])
getChain symbol = do
    (ParserState _ table _ _ _) <- RWS.get
    -- Get chain of matching entries
    return $ Map.lookup symbol table

-- | Look for a symbol in the symbol table and return its scope
lookup :: String -> ParserMonad (Maybe AST.Entry)
lookup symbol = do
    (ParserState (AST.Scopes scopeSet _) _ _ _ _) <- RWS.get
    chainMaybe <- getChain symbol
    case chainMaybe of
        Nothing     -> return Nothing
        Just chain  ->
            -- Get entries that their scope is active
            case filter (\e -> Set.member (AST.entry_scope e) scopeSet) chain of
                [] -> return Nothing
                entries -> do
                    -- Return entry with maximum scope
                    let getBest e1 e2 = if AST.entry_scope e1 > AST.entry_scope e2 then e1 else e2
                    return $ Just $ foldl1 getBest entries

-- | Find if a symbol is in the scope given. This is mainly
--  used for looking fields up.    
lookupField :: String -> Int -> ParserMonad (Maybe AST.Entry)
lookupField symbol level = do
    chainMaybe <- getChain symbol
    case chainMaybe of
        Nothing     -> return Nothing
        Just chain  ->
            -- Get entry that matches symbol
            case filter (\e -> AST.entry_scope e == level) chain of
                [] -> return Nothing
                [e] -> return $ Just e
                _ -> return Nothing

-- | Increment level of scope
incrementScope :: ParserMonad ()
incrementScope = do
    state@(ParserState _ _ lvl _ _) <- RWS.get
    RWS.put $ state{state_lvl = lvl + 1}

-- | Entry of a Type
typeEntry :: AST.ASTType -> ParserMonad (Maybe AST.Entry)
typeEntry = Frontend.Parser.Monad.lookup . AST.type_str

-- | Get current scope
currScope :: ParserMonad Int
currScope = do
    (ParserState (AST.Scopes _ (curr:_)) _ _ _ _) <- RWS.get
    return curr

-- | Add a return type to state 
addReturnType :: Maybe AST.ASTType -> ParserMonad ()
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

-- | Get list of Fields of Type
-- typeFields :: String -> ParserMonad [a]
-- typeFields tkString = do
--     entry <- lookup tkString
--     case entry of 
--         Nothing -> return []
--         Just (AST.Entry _ _ scope _ _) -> 
--         _ -> return []