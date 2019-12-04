module AST where
import Tokens
import Data.Maybe
import Data.List

type Indent = Int

class ASTNode a where
    printNode :: Indent -> a -> String

putTabs :: Indent -> String
putTabs indent = replicate (4*indent) '.'

-- Function Declaration
data FunctionDeclaration =
    FunctionDec {   func_id :: Id, func_type :: Maybe Type, 
                    func_params :: [VarDeclaration], 
                    func_body :: Block  }
    deriving (Eq, Show)

instance ASTNode FunctionDeclaration where
    printNode tabs funcDec =
        putTabs tabs ++ "Function Declaration: " ++ 
        (printNode (tabs+1) (func_id funcDec)) ++ 
        (concat $ map (printNode (tabs+1)) $ func_params funcDec) ++ 
        (maybe "" (printNode (tabs+1)) $ func_type funcDec) ++ 
        (printNode (tabs+1) $ func_body funcDec) ++ 
        "\n"

-- Variable declaration
data VarDeclaration =
    VarDec {    var_id :: Id, var_type :: Type }
    deriving (Eq, Show)

instance ASTNode VarDeclaration where
    printNode tabs varDec =
        putTabs tabs ++
        "Variable Declaration:" ++
        (printNode (tabs+1) $ var_id varDec )++
        (printNode (tabs+1) $ var_type varDec)

-- Identifier
newtype Id = 
    Id    { id_token :: Token }
    deriving (Eq, Show)

instance ASTNode Id where
    printNode tabs identifier =
        putTabs tabs ++
        "Identifier: " ++
        (show (token $ id_token identifier))

data Type =
    Simple      { type_str :: String } |
    Compound    { type_str :: String, type_type :: Type }
    deriving (Eq)

instance Show Type where
    show (Simple str) = str
    show (Compound str type') = str ++ "<" ++ show type' ++ ">"

instance ASTNode Type where
    printNode tabs tp =
        putTabs tabs ++ show tp
        
numberTypes = [AST.Simple "quarter", AST.Simple "eighth", AST.Simple "32th", AST.Simple "64th"]
primitiveTypes =  [AST.Simple "whole", AST.Simple "half"] ++ numberTypes
simpleTypes = [AST.Simple "void", AST.Simple "null", AST.Simple "empty_list"] ++ primitiveTypes

instance Ord Type where
    compare a b =
        if a == b
            then EQ
            else case fmap (\x -> (>x)) (a `elemIndex` simpleTypes) <*> (b `elemIndex` simpleTypes) of
                Nothing -> EQ
                Just True -> LT
                Just False -> GT

-- Expression
data Expression =

    -- | Literal expression
    Literal         {   exp_token :: Token, exp_type :: Type }                                  |

    -- | Type constructor for unions and records
    ChordLiteral        { exp_exps :: [Expression], exp_type :: Type }                          |
    LegatoLiteral       { exp_id :: Id, exp_exp :: Expression, exp_type :: Type }               |
    
    MelodyLiteral   {   exp_values :: [Expression], exp_type :: Type }                          |
    MelodyLiteral'   {   exp_size :: Expression, exp_type :: Type }                              |

    -- | Identifier
    IdExp           {   exp_id :: Id, exp_type :: Type }                                        |

    -- | Call function
    CallExp         {   exp_id :: Id, exp_params :: [Expression], exp_type :: Type }            |

    -- | Dereference an expression
    DereferenceExp  {   exp_exp :: Expression, exp_token :: Token, exp_type :: Type }                               |
    
    -- | Allocate memory
    NewExp          {   exp_init :: Maybe Expression, exp_type :: Type }                              |

    -- | Indexing an array
    IndexingExp     {   exp_left :: Expression, exp_right :: Expression, 
                        exp_bracket :: Token, exp_type :: Type }                                |

    -- | Accessing a struct field
    DotExp          {   exp_left :: Expression, exp_id :: Id, exp_type :: Type }                |

    -- | Negation
    NotExp          {   exp_exp :: Expression, exp_type :: Type }                               |
    
    -- | Conjunction
    AndExp          {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |
    
    -- | Disjunction
    OrExp           {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |

    -- Arithmetic expressions
    NegativeExp     {   exp_exp :: Expression, exp_type :: Type }                               |
    SubstractionExp {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |
    AdditionExp     {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |
    ModExp          {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |
    MultExp         {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |
    DivExp          {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |
    PowExp          {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |

    -- Relational
    EqualExp        {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |
    NotEqualExp     {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |
    LessExp         {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |
    GreaterExp      {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |
    LessEqualExp    {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |
    GreaterEqualExp {   exp_left :: Expression, exp_right :: Expression, exp_type :: Type }     |          

    -- Cast expression
    CastExp         {   exp_exp :: Expression, exp_from :: Type, exp_type :: Type }
    deriving (Eq, Show)

instance ASTNode Expression where
    printNode tabs exp =
        putTabs tabs ++ show exp

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
                        inst_type :: Maybe Type,
                        inst_block :: Block,
                        inst_start :: Maybe Expression,
                        inst_end :: Expression,
                        inst_step :: Maybe Expression   }                     |

    WhileInst       {   inst_exp :: Expression,
                        inst_block :: Block    }                              |

    FreeInst        {   inst_id :: Id  }                                      |

    IncrementInst   {   inst_exp :: Expression  }                             |
    DecrementInst   {   inst_exp :: Expression  }                             |

    BlockInst       {   inst_block :: Block }                                 |

    ChordDec                                                                  |
    LegatoDec                                                                 |

    -- Bemoles y Sostenidos
    SharpExp        {   inst_exp :: Expression }                              |
    FlatExp         {   inst_exp :: Expression }                              
    deriving (Eq, Show)

instance ASTNode Instruction where
    printNode tabs inst =
        putTabs tabs ++ (case inst of
            IfInst exp' inst' Nothing -> "If:\n" ++ (printNode (tabs+1) exp') ++ "\n" ++ (printNode (tabs+1) inst')
            IfInst exp' inst' (Just inst'') -> "If:\n" ++ (printNode (tabs+1) exp') ++ "\n" ++ (printNode (tabs+1) inst') ++ "\n" ++ (putTabs (tabs+1)) ++ "Else:\n" ++ (printNode (tabs+2) inst'')
            inst' -> show inst) ++ "\n"
            
-- Block (Scope)
newtype Block = 
    Block { statements :: [Instruction] }
    deriving (Eq, Show)

instance ASTNode Block where
    printNode tabs block =
        putTabs tabs ++ "Block of instructions:\n" ++
        (concat $ map (printNode (tabs+1)) $ statements block)
