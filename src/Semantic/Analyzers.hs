module Semantic.Analyzers where
import Tokens
import qualified AST
import Parser.Monad (ParserMonad)
import Parser.Monad as PMonad
import qualified Semantic.Data as Sem
import Data.Maybe

-- | Throws a semantic error
semError :: Token -> String -> ParserMonad a
semError tk msg = do
    srcFile <- RWS.ask
    throwCompilerError srcFile [Error (line tk) (col tk) msg]
   
-- | Throw a semantic error if Maybe is Nothing
throwIfNothing :: Maybe a -> Token -> String ->  ParserMonad a
throwIfNothing Nothing tk msg = semError tk msg
throwIfNothing (Just a) _ _ = return a

-----------------------------------------------------------------------------------------------
-- Semantic analysers
-----------------------------------------------------------------------------------------------

-- | Checks if variable is declared, throws an error if it is not
analyzeVar :: Token -> ParserMonad Sem.Entry
analyzeVar tk = do
    let tkString = token tk
    entryMaybe <- PMonad.lookup tkString
    throwIfNothing tk entryMaybe "Variable not in scope:"
    return $ fromJust entryMaybe

-- | Analizes if field correspond to the type of the entry and throws an error if it is not
analyzeField :: Token -> Int -> ParserMoand Sem.Entry
analyzeField tk level = do
    let tkString = token tk
    entryMaybe <- PMonad.lookupField tkString level
    throwIfNothing tk entryMaybe "Field not in type " ++ show (Sem.entry_name entry) ++ ":"
    return $ fromJust entryMaybe

-- | Analizes LValue expression and return its type's entry
analyzeLValue :: AST.Expression -> ParserMonad Sem.Type
analyzeLValue (AST.IdExp (AST.Id tk)) = do
    entry <- analyzeVar tk
    return $ Sem.entry_type entry

analyzeLValue (AST.IndexingExp lval _ bracket) = do
    lType <- analyzeLValue lval
    -- if it is not a melody, error
    case lType of
        Simple _ -> semError bracket "Expression is not a melody:"
        Compound _ innerType -> return innerType

analyzeLValue (AST.DereferenceExp lval) = analyzeLValue lval

analyzeLValue (AST.DotExp lval (AST.Id tk)) = do
    lType <- analyzeLValue lval                 -- Type of the left expression
    let typeEntry   = Sem.type_entry lType      -- Entry of the type
        levelMaybe  = Sem.entry_level typeEntry -- Level opened by record
    
    throwIfNothing tk levelMaybe "Left expression is not a record:"
    fieldEntry <- analyzeField tk (fromJust levelMaybe)
    return $ Sem.entry_type fieldEntry