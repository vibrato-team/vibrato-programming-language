module Semantic.Data where
import qualified AST
import qualified Tokens
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map

-- | Category of each symbol with additional info
data Category =
    Function        { function_block :: Maybe AST.Block, function_params :: [AST.VarDeclaration] }     |
    Var              |
    Type             |
    Constructor      | -- ^ Melody and Sample  
    Field            | -- ^ Member of a struct/union
    Param            | -- ^ Param of a function
    Literal          | -- ^ For True and False
    Prelude            -- ^ A prelude function
    deriving (Eq, Show)

-- | Type of an entry
data Type =
    Simple      { type_str :: String } |
    Compound    { type_str :: String, type_type :: Type }
    deriving (Eq, Show)

-- | Entry, con scope, categoria, token, tipo, cualquier otra cosa chupalo
data Entry = Entry {
    entry_name          :: String,          -- Nombre del símbolog
    entry_category      :: Category,    
    entry_scope         :: Int,             -- Nivel donde fue declarado
    entry_type          :: Maybe Type,
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
