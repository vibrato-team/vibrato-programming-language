module Backend.TAC.TAC where

import qualified AST
import Data.Maybe

class SymEntryCompatible a where
  getSymID :: a -> String

data (SymEntryCompatible a) => ThreeAddressCode a b = ThreeAddressCode
  { tacOperation :: Operation,
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
  show (ThreeAddressCode IfFalse Nothing (Just b) (Just label))   = "\t" ++ "if not " ++ show b ++ " then goto " ++ show label
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
  show (ThreeAddressCode Return Nothing (Just t) Nothing)         = "\treturn " ++ show t 
  show (ThreeAddressCode Sbrk (Just t) (Just sz) Nothing)         = "\t" ++ show t ++ " := sbrk(" ++ show sz ++ ")"  
  show (ThreeAddressCode Print Nothing (Just x) Nothing)          = "\tprint(" ++ show x ++ ")"
  show (ThreeAddressCode Read Nothing (Just x) Nothing)           = "\tread(" ++ show x ++ ")"
  show (ThreeAddressCode Entry Nothing Nothing Nothing)           = "\tENTRY"
  show (ThreeAddressCode Exit Nothing Nothing Nothing)            = "\tEXIT"

  show (ThreeAddressCode Load (Just reg) (Just x) Nothing)        = "\tLOAD " ++ show reg ++ " := " ++ show x
  show (ThreeAddressCode Store (Just reg) (Just x) Nothing)        = "\tSTORE " ++ show reg ++ " := " ++ show x


  show tac = show (tacLvalue tac) ++ " := " ++ show (tacRvalue1 tac) ++ show (tacOperation tac) ++ show (tacRvalue2 tac)

type Instruction = ThreeAddressCode Id AST.ASTType
type Value = Operand Id AST.ASTType

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
    Entry         |
    Exit          |

    Store         |
    Load          |

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
    -- sbrk(bytes)
    Sbrk        |
    -- | malloc(n, t)
    New         |
    -- | free(x)
    Free        |

    -- IO
    Print       |
    Read        |

    Cast String String
    deriving (Eq, Show)

conditionalJumpInsts = [If, IfFalse, Eq, Neq, Lt, Gt, Lte, Gte]

-- Operations that are going to be translated into jumps but not jump and link.
jumpInsts' = GoTo : conditionalJumpInsts

-- Operations that are going to be translated into jumps, including jump and link.
jumpInsts = Call : jumpInsts'

getDestiny :: (SymEntryCompatible a) => ThreeAddressCode a b -> Maybe (Operand a b)
getDestiny inst
  | tacOperation inst `elem` jumpInsts' =
    tacRvalue2 inst
  | otherwise =
    tacRvalue1 inst

-- Operations that are considerad assignments, without Casting
assignmentInsts = [Assign, Add, Sub, Minus, Mult, Div, Mod,  Get, Ref, Call, Sbrk]

isAnAssignment :: Operation -> Bool
isAnAssignment op
  | op `elem` assignmentInsts = True
  | otherwise =
    case op of
      Cast _ _ -> True
      _ -> False

getValues :: Instruction -> [Value]
getValues inst =
  if tacOperation inst `elem` conditionalJumpInsts 
    then catMaybes [tacLvalue inst, tacRvalue1 inst] 
    else catMaybes [tacLvalue inst, tacRvalue1 inst, tacRvalue2 inst]

getIds :: Instruction -> [Id]
getIds inst =
  let values = getValues inst in
    mapMaybe getId values

getId :: Value -> Maybe Id
getId val =
  case val of
    Id var@Temp{} -> Just var
    _ -> Nothing

-- Entry Node of the flow graph
entryNode = ThreeAddressCode Entry Nothing Nothing Nothing :: Instruction
-- Exit Node of the flow graph
exitNode = ThreeAddressCode Exit Nothing Nothing Nothing :: Instruction

data Id = 
  Temp  { temp_name  :: String, temp_type  :: AST.ASTType, temp_offset :: Maybe Int } |
  Var   { entry :: AST.Entry }  |
  Reg   { reg_name :: String, reg_type :: AST.ASTType }  |
  Global { global_name :: String, temp_type  :: AST.ASTType }
  deriving (Eq)

instance Show Id where
  show x@Temp{temp_offset=Just offset} = temp_name x ++ "_" ++ show offset
  show x@Global{} = global_name x
  show x@Reg{} = reg_name x
  show x = getSymID x ++ "_" ++ show (idOffset x)

instance Ord Id where
  compare x y = show x `compare` show y

idOffset :: Id -> Int
idOffset Temp{temp_offset=Just offset} = offset
idOffset Var{entry=e} = fromJust $ AST.offset $ AST.entry_category e

operandOffset :: Operand Id b -> Int
operandOffset (Id x) = idOffset x

instance SymEntryCompatible Id where
  getSymID t@Temp{} = temp_name t
  getSymID x@Var{entry=e} = AST.entry_name e
  getSymID (Reg name _) = name
  getSymID (Global name _) = name

getType :: Value -> AST.ASTType
getType (Constant (_, t)) = t
getType (Id x) = getTypeOfId x

getTypeOfId :: Id -> AST.ASTType
getTypeOfId Temp{temp_type=t} = t
getTypeOfId Var{entry=e} = fromJust $ AST.entry_type e
getTypeOfId Global{temp_type=t} = t
getTypeOfId Reg{reg_type=t} = t