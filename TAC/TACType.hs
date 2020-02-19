module TACType where

class SymEntryCompatible a where
  getSymID :: a -> String

data (SymEntryCompatible a) => ThreeAddressCode a b = ThreeAddressCode
  { tacOperand :: Operation,
    tacLvalue  :: Maybe (Operand a b),
    tacRvalue1 :: Maybe (Operand a b),
    tacRvalue2 :: Maybe (Operand a b)
  }
  deriving (Eq)

instance (SymEntryCompatible a, Show a, Show b) => Show (ThreeAddressCode a b) where
  show (ThreeAddressCode Assign (Just x) (Just y) _)              = "\t" ++ show x ++ " := " ++ show y
  show (ThreeAddressCode Add (Just x) (Just y) (Just z))          = "\t" ++ show x ++ " := " ++ show y ++ " + " ++ show z
  show (ThreeAddressCode Minus (Just x) (Just y) Nothing)         = "\t" ++ show x ++ " := -" ++ show y 
  show (ThreeAddressCode Sub (Just x) (Just y) (Just z))          = "\t" ++ show x ++ " := " ++ show y ++ " - " ++ show z
  show (ThreeAddressCode Mult (Just x) (Just y) (Just z))         = "\t" ++ show x ++ " := " ++ show y ++ " * " ++ show z
  show (ThreeAddressCode Div (Just x) (Just y) (Just z))          = "\t" ++ show x ++ " := " ++ show y ++ " / " ++ show z
  show (ThreeAddressCode (Cast _ toType) (Just x) (Just y) _)     = "\t" ++ show x ++ " := " ++ toType ++ "(" ++ show y ++ ")"
  show (ThreeAddressCode Not (Just x) (Just y) _)                 = "\t" ++ show x ++ " := ~" ++ show y
  show (ThreeAddressCode And (Just x) (Just y) (Just z))          = "\t" ++ show x ++ " := " ++ show y ++ " && " ++ show z
  show (ThreeAddressCode Or (Just x) (Just y) (Just z))           = "\t" ++ show x ++ " := " ++ show y ++ " || " ++ show z
  show (ThreeAddressCode GoTo Nothing Nothing (Just label))       = "\t" ++ "goto " ++ show label
  show (ThreeAddressCode GoTo Nothing Nothing Nothing)            = "\t" ++ "goto __"
  show (ThreeAddressCode If Nothing (Just b) (Just label))        = "\t" ++ "if " ++ show b ++ " then goto " ++ show label
  show (ThreeAddressCode If Nothing (Just b) Nothing)             = "\t" ++ "if " ++ show b ++ " then goto __"
  show (ThreeAddressCode Eq (Just x) (Just y) (Just label))       = "\t" ++ "if " ++ show x ++ " = " ++ show y ++ " then goto " ++ show label
  show (ThreeAddressCode Neq (Just x) (Just y) (Just label))      = "\t" ++  "if " ++ show x ++ " != " ++ show y ++ " then goto " ++ show label
  show (ThreeAddressCode Lt (Just x) (Just y) (Just label))       = "\t" ++ "if " ++ show x ++ " < " ++ show y ++ " then goto " ++ show label
  show (ThreeAddressCode Gt (Just x) (Just y) (Just label))       = "\t" ++ "if " ++ show x ++ " > " ++ show y ++ " then goto " ++ show label
  show (ThreeAddressCode Lte (Just x) (Just y) (Just label))      = "\t" ++  "if " ++ show x ++ " <= " ++ show y ++ " then goto " ++ show label
  show (ThreeAddressCode Gte (Just x) (Just y) (Just label))      = "\t" ++  "if " ++ show x ++ " >= " ++ show y ++ " then goto " ++ show label
  show (ThreeAddressCode Get (Just x) (Just y) (Just i))          = "\t" ++ show x ++ " := " ++ show y ++ "[" ++ show i ++ "]"
  show (ThreeAddressCode Set (Just x) (Just i) (Just y))          = "\t" ++ show x ++ "[" ++ show i ++ "] := " ++ show y
  show (ThreeAddressCode NewLabel Nothing (Just label) Nothing)   = show label ++ ":"
  show (ThreeAddressCode New (Just x) (Just size) Nothing)        = "\t" ++ show x ++ " := malloc(" ++ show size ++ ")"
  show (ThreeAddressCode Free Nothing (Just addr) Nothing)        = "\tfree(" ++ show addr ++ ")"
  show (ThreeAddressCode Ref (Just x) (Just y) Nothing)           = "\t" ++ show x ++ " := &" ++ show y
  show (ThreeAddressCode Param Nothing (Just p) Nothing)          = "\tparam " ++ show p
  show (ThreeAddressCode Call Nothing (Just l) (Just n))          = "\tcall " ++ show l ++ ", " ++ show n
  show (ThreeAddressCode Call (Just t) (Just l) (Just n))         = "\t" ++ show t ++ " := call " ++ show l ++ ", " ++ show n
  show (ThreeAddressCode Return Nothing Nothing Nothing)          = "\treturn" 
  show (ThreeAddressCode Return Nothing (Just t) Nothing)          = "\treturn " ++ show t 

  show tac = show (tacLvalue tac) ++ " := " ++ show (tacRvalue1 tac) ++ show (tacOperand tac) ++ show (tacRvalue2 tac)

data (SymEntryCompatible a) => Operand a b = 
  Id a | 
  Constant (String, b) | 
  Label String
  deriving (Eq)

instance (SymEntryCompatible a, Show a, Show b) => Show (Operand a b) where
  show (Id x) = show x
  show (Constant c) = fst c
  show (Label l) = l

data Operation =
    Assign        |
    -- Arithmetic
    -- | Addition
    Add            |
    -- | Substraction
    Sub           |
    -- | Unary minus
    Minus           |
    -- | Multiplication
    Mult          |
    -- | Division
    Div           |
    -- | Modulus
    Mod          |

    -- Logical
    -- | Logical and
    And               |
    -- | Logical or
    Or               |
    -- | Logical not
    Not             |

    -- Comparators
    -- | Greater than
    Gt           |
    -- | Greater than or equal
    Gte        |
    -- | Less than
    Lt           |
    -- | Less than or equal
    Lte        |
    -- | Equal
    Eq           |
    -- | Not equal
    Neq         |

    -- Jumping
    -- | goto <label>
    GoTo        |
    -- | if <var> goto <label>
    If          |
    -- | if ~<var> goto <label>
    IfFalse     |
    -- | New label
    NewLabel       |

    -- Calling functions
    -- | Define a parameter
    Param       |
    -- | Call function
    Call        |
    -- | Return from function
    Return      |

    -- Array operators
    -- | x=y[i]
    Get         |
    -- | x[i]=y
    Set         |
    -- | x:= 5:y
    Anexo             |
    -- | x:= y::z
    Concat            |

    -- Pointer operations
    -- | x=&y
    Ref         |
    -- | x=*y
    Deref       |
    -- | malloc(n, t)
    New         |
    -- | free(x)
    Free        |

    Cast String String
    deriving (Eq, Show)
