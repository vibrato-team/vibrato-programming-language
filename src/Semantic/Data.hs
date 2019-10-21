module Semantic.Data where
import qualified AST
import qualified Token
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Category con info adicional
data Category =
    Function        { ast_function :: AST.FunctionDeclaration }     |
    Var                                                             |
    Type                                                            |
    Compound                                                        |       
    Field                                                           |
    Param                                                           
    deriving (Eq, Show)

-- Para el campo `type` de Entry
data Type =
    Simple Entry |
    Compound Entry Type
    deriving (Eq, Show)

-- Entry, con scope, categoria, token, tipo, cualquier otra cosa chupalo
data Entry = Entry {
    entry_name          :: String,          -- Nombre del símbolog
    entry_category      :: Category,        
    entry_scope         :: Int,             -- Nivel donde fue declarado
    entry_type          :: Maybe Type,      -- Tipo del símbolo
    entry_level         :: Maybe Int        -- Nivel correspondiente al tipo (registro) de la variable
}

-- Alias SymbolTable para un mapa de String -> [Entry]
type SymbolTable = Map.Map String [Entry]

-- Alias Scopes para un set de int
type Scope = Set.Set Int