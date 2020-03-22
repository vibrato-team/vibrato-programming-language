module Util.Arquitecture where

-- | Arquitecture word
arqWord :: Int
arqWord = 4

halfWord = arqWord `div` 2

doubleWord :: Int
doubleWord = 4*2

arqByte :: Int
arqByte = 1

-- | Calculates next word
nextWord :: Int -> Int -> Int
nextWord offset width = ((offset + width + (arqWord-1)) `div` arqWord) * arqWord

-- General purpose registers
type Reg = Int
generalPurposeRegs :: [Reg]
generalPurposeRegs = [8..25]

numberOfRegs = length generalPurposeRegs

{--
----------------------------------------------------------
linked list of allocated objects for this frame
-----------------------------------------------------------
return address
-----------------------------------------------------------
first auxiliar temporal for spills
-----------------------------------------------------------
second auxilliar temporal for spills
-----------------------------------------------------------
third auxiliar temporal for spills
-----------------------------------------------------------
--}
initialOffset       = 5*arqWord