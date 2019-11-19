module Semantic.Analyzers where
import Tokens
import qualified AST
import Parser.Monad (ParserMonad)
import Parser.Monad as PMonad
import qualified Semantic.Data as Sem
import Data.Maybe
import qualified Control.Monad.RWS.Lazy as RWS
import Control.Monad.Trans
import Util.Error

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
checkVarIsDeclared :: Token -> ParserMonad Sem.Entry
checkVarIsDeclared tk = do
    let tkString = token tk
    entryMaybe <- PMonad.lookup tkString

    throwIfNothing entryMaybe tk "Variable not in scope:"
    return $ fromJust entryMaybe

-- | Analizes if field correspond to the type of the entry and throws an error if it is not
analyzeField :: Token -> Int -> ParserMonad Sem.Entry
analyzeField tk level = do
    let tkString = token tk
    entryMaybe <- PMonad.lookupField tkString level

    throwIfNothing entryMaybe tk (show tkString ++ " is not a valid field:")
    return $ fromJust entryMaybe
