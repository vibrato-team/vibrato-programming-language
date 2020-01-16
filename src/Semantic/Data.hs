module Semantic.Data where
import qualified AST
import qualified Frontend.Tokens
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map

-- | Chord or Legato Data
data ADT = Chord | Legato deriving (Eq, Show)

-- | Type that commtis an error occurred
errorType = AST.Simple "Error"
errorExp = AST.ErrorExp errorType
-- | Category of each symbol with additional info
data Category =
    Function         { function_block :: Maybe AST.Block, function_params :: [AST.VarDeclaration] }     |
    Var              |
    Const            |
    Type             { type_fields :: Maybe [AST.VarDeclaration], type_adt :: Maybe ADT } |
    Constructor      | -- ^ Melody and Sample  
    Field            | -- ^ Member of a struct/union
    Param            { param_ref :: Bool }| -- ^ Param of a function
    Literal          | -- ^ For True and False
    Prelude          [AST.Type]  -- ^ A prelude function with a list of params' types.
    deriving (Eq, Show)

-- | Entry, con scope, categoria, token, tipo, cualquier otra cosa chupalo
data Entry = Entry {
    entry_name          :: String,          -- Nombre del símbolog
    entry_category      :: Category,    
    entry_scope         :: Int,             -- Nivel donde fue declarado
    entry_type          :: Maybe AST.Type,
    entry_level         :: Maybe Int        -- Nivel correspondiente al tipo (registro) de la variable
} deriving (Eq, Show)

type SymbolTable = Map.Map String [Entry]

type ScopeSet = Set.Set Int
type ScopeStack = [Int]

-- | Stack of the scopes
data Scopes = Scopes { 
    scope_set :: ScopeSet, 
    scope_stack :: ScopeStack }
    deriving (Eq, Show)
