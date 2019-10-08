module AST where
import Tokens
import Data.Maybe

type Indent = Int

class ASTNode a where
    printNode :: Indent -> a -> IO ()

putTabs :: Indent -> IO ()
putTabs indent = putStr $ replicate (4*indent) '.'

-- Start Symbol
newtype Program =
    Start { external_declarations :: [ExternalDeclaration] }
    deriving (Eq, Show)

instance ASTNode Program where
    printNode tabs (Start decs) = do
        putTabs tabs
        putStrLn "Program:"
        foldl1 (>>) $ map (printNode (tabs+1)) decs
        putStr "\n"

-- Global var declarations and function declarations
data ExternalDeclaration =
    ExternalFunctionDeclaration FunctionDeclaration |
    ExternalVarDeclaration VarDeclaration
    deriving (Eq, Show)

instance ASTNode ExternalDeclaration where
    printNode tabs (ExternalFunctionDeclaration funcDec) =
        printNode tabs funcDec

    printNode tabs (ExternalVarDeclaration varDec) =
        printNode tabs varDec

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
        foldl1 (>>) $ map (printNode (tabs+1)) $ func_params funcDec
        maybe (putStr "") (printNode (tabs+1)) $ func_type funcDec
        printNode (tabs+1) $ func_body funcDec
        putStr "\n"

-- Variable declaration
data VarDeclaration =
    VarDec {    var_id :: Id, var_type :: Type, 
                var_init :: Maybe Expression    }
    deriving (Eq, Show)

instance ASTNode VarDeclaration where
    printNode tabs varDec = do
        putTabs tabs
        putStrLn "Variable Declaration:"
        printNode (tabs+1) $ var_id varDec
        printNode (tabs+1) $ var_type varDec
        maybe (putStr "") (printNode (tabs+1)) $ var_init varDec

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
    -- Literal expression
    Literal         {   exp_token :: Token }                                  |

    Literal'        { exp_exp :: Expression, exp_type :: Type }               |
    
    LiteralMelody   {   exp_values :: [Expression] }                          |

    -- Identifier
    IdExp           {   exp_id :: Id }                                        |

    -- Call function
    CallExp         {   exp_id :: Id, exp_params :: [Expression] }            |

    -- Pointers expression
    DereferenceExp  {   exp_exp :: Expression }                               |
    NewExp          {   exp_init :: Expression }                              |

    -- array expression
    IndexingExp     {   exp_left :: Expression, exp_right :: Expression }     |

    -- Accessing a struct field
    DotExp          {   exp_left :: Expression, exp_id :: Id }                |

    -- Boolean expression
    NotExp          {   exp_exp :: Expression }                               |
    AndExp          {   exp_left :: Expression, exp_right :: Expression }     |
    OrExp           {   exp_left :: Expression, exp_right :: Expression }     |

    -- Arithmetic expression
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
    GreaterEqualExp {   exp_left :: Expression, exp_right :: Expression }     |

    -- Bemoles y Sostenidos
    SharpExp        {   exp_exp :: Expression }                               |
    FlatExp         {   exp_exp :: Expression }                                             

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

    RecordInst      {   inst_exps :: [Expression] }                              |
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

    FreeInst        {   inst_exp :: Expression  }                             |

    IncrementInst   {   inst_exp :: Expression  }                             |
    DecrementInst   {   inst_exp :: Expression  }                             |

    BlockInst       {   inst_block :: Block }

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
        foldl1 (>>) $ map (printNode (tabs+1)) $ statements block


newtype ChordLegatoDeclaracion =
    ChordLegatoDec        { list :: ParamsCL }

data ParamsCL =
    ParamsCL { chordlegato_id :: Id, var_params :: [VarDeclaration] }