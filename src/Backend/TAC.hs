module Backend.TAC where

class SymEntryCompatible a where
  getSymID :: a -> String


data (SymEntryCompatible a) => ThreeAddressCode a b = ThreeAddressCode
  { tacOperand :: Operation,
    tacLvalue  :: Maybe (Operand a b),
    tacRvalue1 :: Maybe (Operand a b),
    tacRvalue2 :: Maybe (Operand a b)
  }

data (SymEntryCompatible a) => Operand a b = Variable a | Constant b

data Operation =
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
    Deref       
    deriving (Eq)
