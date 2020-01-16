module TACType where

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
  Suma            |
  Resta           |
  Multiplicacion  |
  Division        |
  DivisionEntera  |
  Y               |
  O               |
  Modulo          |
  Mayor           |
  MayorQue        |
  Menor           |
  MenorQue        |
  Igual           |
  NoIgual         |
  Concatenacion   |
  AnexoConcat
  deriving (Eq)
