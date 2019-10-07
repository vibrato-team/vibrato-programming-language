module AST where
import Tokens
import Data.Maybe

type Indent = Int

class ASTNode a where
    printNode :: Indent -> a -> IO ()

putTabs :: Indent -> IO ()
putTabs indent = putStr $ replicate (2*indent) '-'

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
    FunctionDec {   func_id :: Id, func_type :: Type, 
                    func_params :: [VarDeclaration], 
                    func_body :: Block  }
    deriving (Eq, Show)

instance ASTNode FunctionDeclaration where
    printNode tabs funcDec = do
        putTabs tabs
        putStr "Function Declaration: "
        putStr $ show (func_id funcDec) ++ "("
        foldl1 (>>) $ map (printNode (tabs+1)) $ func_params funcDec
        putStrLn $ ": " ++ show $ token $ type_token $ funct_type funcDec
        printNode (tabs+1) $ func_body funcDec
        putStr "\n"

-- Variable declaration
data VarDeclaration =
    VarDec {    var_id :: Id, var_type :: Type, 
                var_init :: Maybe Expression    }
    deriving (Eq, Show)

instance ASTNode VarDeclaration where
    printNode tabs varDec = do
        printNode (tabs+1) $ var_id varDec
        putStr ": "
        printNode (tabs+1) $ var_type varDec
        maybe (putStr "") (printNode (tabs+1)) $ var_init varDec
        putStr "\n"

-- Identifier
newtype Id = 
    Id    { id_token :: Token }
    deriving (Eq, Show)

instance ASTNode Id where
    printNode tabs identifier = do
        putTabs tabs
        putStrLn $ "Identifier: " ++ show $ token $ id_token identifier

-- Type
newtype Type = 
    Type { type_token :: Token } 
    deriving (Eq, Show)

instance ASTNode Type where
    printNode tabs dataType = do
        putTabs tabs
        putStrLn $ "Data Type: " ++ show $ token $ id_token identifier

-- Expression
data Expression =
    -- Identifier
    IdExp           {   exp_id :: Id }    |

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
    PowExp          {   exp_left :: Expression, exp_right :: Expression }
    deriving (Eq, Show)

-- Instructions
data Instruction =
    VarDecInst      {   inst_dec :: VarDeclaration  }                         |
    AssignInst      {   inst_left :: Expression, inst_right :: Expression }   |

    ReturnInst      {   inst_exp :: Expression }                              |
    NextInst                                                                  |
    StopInst                                                                  |

    RecordInst      {   inst_exp :: Expression }                              |
    PlayInst        {   inst_exps :: [Expression] }                           |
    
    IfInst          {   inst_exp :: Expression, inst_inst :: Instruction,
                        inst_else :: Maybe Instruction  }                     |
    
    ForInst         {   inst_block :: Block,
                        inst_start :: Maybe Expression,
                        inst_end :: Maybe Expression,
                        inst_step :: Maybe Expression   }                     |

    WhileInst       {   inst_exp :: Expression,
                        inst_block :: Block    }                              |

    FreeInst        {   inst_exp :: Expression  }                             |

    IncrementInst   {   inst_exp :: Expression  }                             |
    DecrementInst   {   inst_exp :: Expression  }                             |

    BlockInst       {   inst_block :: Block }
    deriving (Eq, Show)

-- Block (Scope)
newtype Block = 
    Block { statements :: [Instruction] }
    deriving (Eq, Show)