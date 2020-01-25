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
  show (ThreeAddressCode Assign (Just x) (Just y) _)          = show x ++ " = " ++ show y
  show (ThreeAddressCode Add (Just x) (Just y) (Just z))      = show x ++ " = " ++ show y ++ " + " ++ show z
  show (ThreeAddressCode Minus (Just x) (Just y) Nothing)     = show x ++ " = -" ++ show y 
  show (ThreeAddressCode Sub (Just x) (Just y) (Just z))      = show x ++ " = " ++ show y ++ " - " ++ show z
  show (ThreeAddressCode Mult (Just x) (Just y) (Just z))     = show x ++ " = " ++ show y ++ " * " ++ show z
  show (ThreeAddressCode Div (Just x) (Just y) (Just z))      = show x ++ " = " ++ show y ++ " / " ++ show z
  show (ThreeAddressCode (Cast _ toType) (Just x) (Just y) _) = show x ++ " = " ++ toType ++ "(" ++ show y ++ ")"
  show (ThreeAddressCode Not (Just x) (Just y) _)             = show x ++ " = ~" ++ show y
  show (ThreeAddressCode And (Just x) (Just y) (Just z))      = show x ++ " = " ++ show y ++ " && " ++ show z
  show (ThreeAddressCode Or (Just x) (Just y) (Just z))       = show x ++ " = " ++ show y ++ " || " ++ show z
  show (ThreeAddressCode GoTo Nothing Nothing (Just label))   = "goto " ++ show label
  show (ThreeAddressCode GoTo Nothing Nothing Nothing)        = "goto __"
  show (ThreeAddressCode If Nothing (Just b) (Just label))    = "if " ++ show b ++ " then goto " ++ show label
  show (ThreeAddressCode If Nothing (Just b) Nothing)         = "if " ++ show b ++ " then goto __"

  show tac = show (tacLvalue tac) ++ " = " ++ show (tacRvalue1 tac) ++ " (?) " ++ show (tacRvalue2 tac)

data (SymEntryCompatible a) => Operand a b = 
  Variable a | 
  Constant (String, b) | 
  Label Int
  deriving (Eq)

instance (SymEntryCompatible a, Show a, Show b) => Show (Operand a b) where
  show (Variable x) = show x
  show (Constant c) = fst c
  show (Label l) = show l

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

    -- Array operators
    -- | x=y[i]
    Get         |
    -- | x[i]=y
    Set         |

    -- Pointer operations
    -- | x=&y
    Ref         |
    -- | x=*y
    Deref       |

    Cast String String
    deriving (Eq, Show)
