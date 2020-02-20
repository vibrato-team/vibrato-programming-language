module AST where
import Frontend.Tokens
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map

type Indent = Int

putTabs :: Indent -> String
putTabs indent = replicate (4*indent) '.'

-- Function Declaration
data FunctionDeclaration =
    FunctionDec {   func_id :: Id, func_type :: Maybe ASTType, 
                    func_params :: [VarDeclaration], 
                    func_body :: Block  }
    deriving (Eq, Show)

-- Variable declaration
data VarDeclaration =
    VarDec {    var_id :: Id, var_type :: ASTType }
    deriving (Eq, Show)

-- Identifier
newtype Id = 
    Id    { id_token :: Token }
    deriving (Eq, Show)

data ASTType =
    Simple      { type_str :: String } |
    Compound    { type_str :: String, type_type :: ASTType }
    deriving (Eq)

instance Show ASTType where
    show (Simple str) = str
    show (Compound str type') = str ++ "<" ++ show type' ++ ">"
        
numberTypes     =   [AST.Simple "quarter", AST.Simple "eighth", AST.Simple "32th", AST.Simple "64th"]
primitiveASTTypes  =   [AST.Simple "whole", AST.Simple "half"] ++ numberTypes
simpleASTTypes     =   [AST.Simple "void", AST.Simple "null", AST.Simple "empty_list"] ++ primitiveASTTypes

instance Ord ASTType where
    compare a b =
        if a == b
            then EQ
            else case fmap (\x -> (>x)) (a `elemIndex` simpleASTTypes) <*> (b `elemIndex` simpleASTTypes) of
                Nothing -> EQ
                Just True -> LT
                Just False -> GT

-- Expression
data Expression = 

    -- | Error Expression
    ErrorExp        {   exp_type :: ASTType }                                                      |

    -- | Literal expression
    LiteralExp         {   exp_token :: Token, exp_type :: ASTType, exp_offset :: Int }                                  |

    -- | ASTType constructor for unions and records
    ChordLiteral        { exp_exps :: [Expression], exp_type :: ASTType, exp_offset :: Int }                          |
    LegatoLiteral       { exp_id :: Id, exp_exp :: Expression, exp_type :: ASTType, exp_offset :: Int }               |
    
    MelodyLiteral   {   exp_exps :: [Expression], exp_type :: ASTType, exp_offset :: Int }                          |
    MelodyLiteral'   {   exp_size :: Expression, exp_type :: ASTType, exp_offset :: Int }                              |

    -- | Identifier
    IdExp           {   exp_id :: Id, exp_type :: ASTType, exp_entry :: Maybe Entry, exp_offset :: Int }                      |

    -- | Call function
    CallExp         {   exp_id :: Id, exp_params :: [Expression],
                        exp_type :: ASTType, exp_offset :: Int,
                        exp_entry :: Maybe Entry }            |

    -- | Dereference an expression
    DereferenceExp  {   exp_exp :: Expression, exp_token :: Token, exp_type :: ASTType, exp_offset :: Int }                               |
    
    -- | Allocate memory
    NewExp          {   exp_init :: Maybe Expression, exp_type :: ASTType, exp_offset :: Int }                              |

    -- | Indexing an array
    IndexingExp     {   exp_left :: Expression, exp_right :: Expression, 
                        exp_bracket :: Token, exp_type :: ASTType, exp_offset :: Int }                                |

    LengthExp       { exp_exp :: Expression, exp_type :: ASTType, exp_offset :: Int }                                 |

    -- | Accessing a struct field
    DotExp          {   exp_left :: Expression, exp_id :: Id, exp_type :: ASTType, exp_offset :: Int }                |

    -- | Negation
    NotExp          {   exp_exp :: Expression, exp_type :: ASTType, exp_offset :: Int }                               |
    
    -- | Conjunction
    AndExp          {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |
    
    -- | Disjunction
    OrExp           {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |

    -- Arithmetic expressions
    NegativeExp     {   exp_exp :: Expression, exp_type :: ASTType, exp_offset :: Int }                               |
    SubstractionExp {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |
    AdditionExp     {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |
    ModExp          {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |
    MultExp         {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |
    DivExp          {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |
    PowExp          {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |

    -- Relational
    EqualExp        {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |
    NotEqualExp     {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |
    LessExp         {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |
    GreaterExp      {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |
    LessEqualExp    {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |
    GreaterEqualExp {   exp_left :: Expression, exp_right :: Expression, exp_type :: ASTType, exp_offset :: Int }     |          

    -- Cast expression
    CastExp         {   exp_exp :: Expression, exp_from :: ASTType, exp_type :: ASTType, exp_offset :: Int }
    deriving (Eq, Show)

-- instance Show Expression where
--     show (AST.Literal expToken _) = token expToken

-- Instructions
data Instruction =
    VarDecInst      {   inst_dec :: VarDeclaration  }                         |
    AssignInst      {   inst_left :: Expression, inst_right :: Expression }   |

    CallFuncInst    { inst_call :: Expression }                               |

    ReturnInst      {   inst_maybe_exp :: Maybe Expression }                  |
    NextInst                                                                  |
    StopInst                                                                  |

    RecordInst      {   inst_exps :: [Expression] }                           |
    PlayInst        {   inst_exps :: [Expression] }                           |
    
    IfInst          {   inst_exp :: Expression, inst_inst :: Instruction,
                        inst_else :: Maybe Instruction  }                     |
    
    ForInst         {   inst_id :: Id,
                        inst_type :: Maybe ASTType,
                        inst_entry :: Entry,
                        inst_block :: Block,
                        inst_start :: Maybe Expression,
                        inst_end :: Expression,
                        inst_step :: Maybe Expression   }                     |

    WhileInst       {   inst_exp :: Expression,
                        inst_block :: Block    }                              |

    FreeInst        {   inst_exp :: Expression  }                                      |

    IncrementInst   {   inst_exp :: Expression  }                             |
    DecrementInst   {   inst_exp :: Expression  }                             |

    BlockInst       {   inst_block :: Block }                                 |

    ChordDec                                                                  |
    LegatoDec                                                                 |

    -- Bemoles y Sostenidos
    SharpExp        {   inst_exp :: Expression }                              |
    FlatExp         {   inst_exp :: Expression }                              
    deriving (Eq, Show)

-- Block (Scope)
newtype Block = 
    Block { statements :: [Instruction] }
    deriving (Eq, Show)

----------------------------------------------------------------------
----------------------------------------------------------------------
-- | Chord or Legato Data
data ADT = Chord | Legato deriving (Eq, Show)

-- | Type that commtis an error occurred
errorType = AST.Simple "Error"
errorExp = AST.ErrorExp errorType
-- | Category of each symbol with additional info
data Category =
    Function         { function_block :: Maybe AST.Block, function_params :: [AST.VarDeclaration] }     |
    Var              { offset :: Maybe Int }|
    Const            { offset :: Maybe Int }|
    Type             { type_fields :: Maybe [AST.VarDeclaration], type_adt :: Maybe ADT, type_size :: Int } |
    Constructor      | -- ^ Melody and Sample  
    Field            { offset :: Maybe Int }| -- ^ Member of a struct/union
    Param            { param_ref :: Bool, offset :: Maybe Int }| -- ^ Param of a function
    Literal          | -- ^ For True and False
    Prelude          [AST.ASTType]  -- ^ A prelude function with a list of params' types.
    deriving (Eq, Show)

-- | Entry, con scope, categoria, token, tipo, cualquier otra cosa chupalo
data Entry = Entry {
    entry_name          :: String,          -- Nombre del símbolog
    entry_category      :: Category,    
    entry_scope         :: Int,             -- Nivel donde fue declarado
    entry_type          :: Maybe AST.ASTType,
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
