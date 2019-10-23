module Semantic.Data where
import qualified AST
import qualified Tokens
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map

-- Category con info adicional
data Category =
    Function        { ast_function :: AST.FunctionDeclaration }     |
    Var             |
    Type            |
    Constructor     |     -- Melody and Sample  
    Field           |     -- of a record
    Param                 -- of a function                                          
    deriving (Eq, Show)

-- Para el campo `type` de Entry
data Type =
    Simple      { type_entry :: Entry } |
    Compound    { type_entry :: Entry, type_type :: Type }
    deriving (Eq, Show)

-- TODO: Quitar el type de aquí y agregarlo a las categorías
-- Entry, con scope, categoria, token, tipo, cualquier otra cosa chupalo
data Entry = Entry {
    entry_name          :: String,          -- Nombre del símbolog
    entry_category      :: Category,        
    entry_scope         :: Int,             -- Nivel donde fue declarado
    entry_type          :: Maybe Type,      -- Tipo del símbolo
    entry_level         :: Maybe Int        -- Nivel correspondiente al tipo (registro) de la variable
} deriving (Eq, Show)

-- Alias SymbolTable para un mapa de String -> [Entry]
type SymbolTable = Map.Map String [Entry]

-- Alias Scopes para un set de int
type ScopeSet = Set.Set Int
