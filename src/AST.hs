module AST where
import Tokens

-- Start Symbol
newtype Program =
    Start { external_declarations :: [ExternalDeclaration] }
    deriving (Eq, Show)

data ExternalDeclaration =
    ExternalFunctionDeclaration FunctionDeclaration |
    ExternalVarDeclaration VarDeclaration
    deriving (Eq, Show)

-- Function Declaration
data FunctionDeclaration =
    FunctionDec {   func_id :: Id, func_type :: Type, 
                    func_params :: [(Id, Type)], 
                    func_body :: Block  }
    deriving (Eq, Show)

-- Variable declaration
data VarDeclaration =
    VarDec {    var_id :: Id, var_type :: Type, 
                var_init :: Maybe Expression    }
    deriving (Eq, Show)

-- Identifier
newtype Id = 
    Id    { id_token :: Token }
    deriving (Eq, Show)

-- Type
newtype Type = 
    Type { type_token :: Token } 
    deriving (Eq, Show)

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