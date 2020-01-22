module TACType where

import Data.Maybe

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
  show (ThreeAddressCode Assign (Just x) (Just y) _) = show x ++ " = " ++ show y
  show (ThreeAddressCode Add (Just x) (Just y) (Just z)) = show x ++ " = " ++ show y ++ " + " ++ show z
  show tac = show (tacLvalue tac) ++ " = " ++ show (tacRvalue1 tac) ++ " (?) " ++ show (tacRvalue2 tac)

data (SymEntryCompatible a) => Operand a b = 
  Variable a | 
  Constant b | 
  Label Int
  deriving (Eq)

instance (SymEntryCompatible a, Show a, Show b) => Show (Operand a b) where
  show (Variable x) = show x
  show (Constant c) = show c
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
    Deref       
    deriving (Eq, Show)