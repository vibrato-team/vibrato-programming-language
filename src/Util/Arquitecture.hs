module Util.Arquitecture where

-- | Arquitecture word
arqWord :: Int
arqWord = 4

doubleWord :: Int
doubleWord = 4*2

arqByte :: Int
arqByte = 1

-- | Calculates next word
nextWord :: Int -> Int -> Int
nextWord offset width = ((offset + width + (arqWord-1)) `div` arqWord) * arqWord