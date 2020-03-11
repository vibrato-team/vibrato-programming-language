module Backend.FlowGraph.Block where

import qualified Data.Set as Set
import qualified Backend.TAC.TAC as TAC

data Block = Block {
    insts           :: [TAC.Instruction],
    from_idx        :: Int,
    to_idx          :: Int,
    live_vars       :: Set.Set TAC.Id,
    edges_set       :: Set.Set Edge
} deriving (Eq)

instance Show Block where
    show (Block _ fromIdx toIdx liveVars edgesSet) = "B [" ++ show fromIdx ++ ", " ++ show toIdx ++ "), => " ++ show edgesSet

type Edge = Int

type BlockList = [Block]
