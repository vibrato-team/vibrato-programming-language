module AST where
import Tokens
import Data.Maybe

type Indent = Int

class ASTNode a where
    printNode :: Indent -> a -> IO ()

putTabs :: Indent -> IO ()
putTabs indent = putStr $ replicate (4*indent) '.'

-- Function Declaration
data FunctionDeclaration =
    FunctionDec {   func_id :: Id, func_type :: Maybe Type, 
                    func_params :: [VarDeclaration], 
                    func_body :: Block  }
    deriving (Eq, Show)

instance ASTNode FunctionDeclaration where
    printNode tabs funcDec = do
        putTabs tabs
        putStrLn "Function Declaration: "
        printNode (tabs+1) (func_id funcDec)
        foldl (>>) (putStr "") $ map (printNode (tabs+1)) $ func_params funcDec
        maybe (putStr "") (printNode (tabs+1)) $ func_type funcDec
        printNode (tabs+1) $ func_body funcDec
        putStr "\n"

-- Variable declaration
data VarDeclaration =
    VarDec {    var_id :: Id, var_type :: Type }
    deriving (Eq, Show)

instance ASTNode VarDeclaration where
    printNode tabs varDec = do
        putTabs tabs
        putStrLn "Variable Declaration:"
        printNode (tabs+1) $ var_id varDec
        printNode (tabs+1) $ var_type varDec

-- Identifier
newtype Id = 
    Id    { id_token :: Token }
    deriving (Eq, Show)

instance ASTNode Id where
    printNode tabs identifier = do
        putTabs tabs
        putStrLn $ "Identifier: " ++ show (token $ id_token identifier)

-- Type
data Type = 
    Type    { type_token :: Token, type_type :: Maybe Type } 
    deriving (Eq, Show)

instance ASTNode Type where
    printNode tabs dataType = do
        putTabs tabs
        putStrLn $ "Data Type: " ++ show (token $ type_token dataType)

-- Expression
data Expression =
    -- | NUll expression
    NullExp                                                                   |

    -- | Literal expression
    Literal         {   exp_token :: Token }                                  |

    -- | Type constructor for scalars and records
    Literal'        { exp_exps :: [Expression], exp_type :: Type }            |
    
    LiteralMelody   {   exp_values :: [Expression] }                          |

    -- | Identifier
    IdExp           {   exp_id :: Id }                                        |

    -- | Call function
    CallExp         {   exp_id :: Id, exp_params :: [Expression] }            |

    -- | Dereference an expression
    DereferenceExp  {   exp_exp :: Expression, exp_token :: Token }                               |
    
    -- | Allocate memory
    NewExp          {   exp_init :: Expression }                              |

    -- | Indexing an array
    IndexingExp     {   exp_left :: Expression, exp_right :: Expression, 
                        exp_bracket :: Token }                                |

    -- | Accessing a struct field
    DotExp          {   exp_left :: Expression, exp_id :: Id }                |

    -- | Negation
    NotExp          {   exp_exp :: Expression }                               |
    
    -- | Conjunction
    AndExp          {   exp_left :: Expression, exp_right :: Expression }     |
    
    -- | Disjunction
    OrExp           {   exp_left :: Expression, exp_right :: Expression }     |

    -- Arithmetic expressions
    NegativeExp     {   exp_exp :: Expression }                               |
    SubstractionExp {   exp_left :: Expression, exp_right :: Expression }     |
    AdditionExp     {   exp_left :: Expression, exp_right :: Expression }     |
    ModExp          {   exp_left :: Expression, exp_right :: Expression }     |
    MultExp         {   exp_left :: Expression, exp_right :: Expression }     |
    DivExp          {   exp_left :: Expression, exp_right :: Expression }     |
    PowExp          {   exp_left :: Expression, exp_right :: Expression }     |

    -- Relational
    EqualExp        {   exp_left :: Expression, exp_right :: Expression }     |
    NotEqualExp     {   exp_left :: Expression, exp_right :: Expression }     |
    LessExp         {   exp_left :: Expression, exp_right :: Expression }     |
    GreaterExp      {   exp_left :: Expression, exp_right :: Expression }     |
    LessEqualExp    {   exp_left :: Expression, exp_right :: Expression }     |
    GreaterEqualExp {   exp_left :: Expression, exp_right :: Expression }            

    deriving (Eq, Show)

instance ASTNode Expression where
    printNode tabs exp = do
        putTabs tabs
        print exp

-- Instructions
data Instruction =
    VarDecInst      {   inst_dec :: VarDeclaration  }                         |
    AssignInst      {   inst_left :: Expression, inst_right :: Expression }   |

    ReturnInst      {   inst_exp :: Expression }                              |
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
    printNode tabs inst = do
        putTabs tabs
        print inst

-- Block (Scope)
newtype Block = 
    Block { statements :: [Instruction] }
    deriving (Eq, Show)

instance ASTNode Block where
    printNode tabs block = do
        putTabs tabs
        putStrLn "Block of instructions:"
        foldl (>>) (putStr "") $ map (printNode (tabs+1)) $ statements block
