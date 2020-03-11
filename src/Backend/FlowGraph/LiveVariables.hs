module Backend.FlowGraph.LiveVariables where

import qualified Control.Monad.State.Lazy as State
import Control.Monad.Trans
import qualified Backend.TAC.TAC as TAC
import qualified Backend.FlowGraph.Block as Block
import qualified Backend.FlowGraph.FlowGraph as FlowGraph
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Maybe

type Idx = Int
type LiveVars = Set.Set TAC.Id
type LiveVarsMap = Map.Map Idx LiveVars -- LiveVars[s] == IN[s]

data LVState = LVState {
    live_vars_map :: LiveVarsMap,
    tac_map       :: Map.Map Idx TAC.Instruction,
    block_map     :: Map.Map Idx Block.Block,
    in_changed    :: Bool
}

type LVMonad = State.StateT LVState IO

getInitialState :: [TAC.Instruction] -> [Block.Block] -> LVState
-- IN[EXIT] = empty set
getInitialState tac blocks = 
    let tacMap = Map.fromList $ snd $ foldl (\(idx, tacMap') inst -> (idx+1, (idx, inst):tacMap')) (0, []) tac
        blockMap = Map.fromList $ map (\b -> (Block.from_idx b, b)) blocks in
        LVState (Map.fromList $ zip [0..(length tac)] (repeat Set.empty)) tacMap blockMap True

computeLiveVarsOfInst :: Idx -> LVMonad ()
computeLiveVarsOfInst idx = do
    state@LVState{live_vars_map=liveVarsMap, tac_map=tacMap, block_map=blockMap, in_changed=prevInChanged} <- State.get
    let Just prevLiveVars = Map.lookup idx liveVarsMap
        Just (leaderIdx, block) = Map.lookupLE idx blockMap
        inst        = fromJust $ Map.lookup idx tacMap
        operation   = TAC.tacOperation inst
        lValueMaybe = TAC.tacLvalue inst
        idMaybe     = TAC.getId =<< lValueMaybe
        definedVars = if TAC.isAnAssignment operation && isJust idMaybe
            then Set.singleton $ fromJust idMaybe
            else Set.empty
        usedVars = Set.fromList $ TAC.getIds inst
        succs = if idx + 1 == Block.to_idx block then Set.toList (Block.edges_set block) else [idx+1]
        liveVarsOfSuccs = map (\idx' -> fromJust $ Map.lookup idx' liveVarsMap ) succs
        outInst = foldl Set.union Set.empty liveVarsOfSuccs
        liveVars = usedVars `Set.union` (outInst `Set.difference` definedVars)
        inChanged = liveVars /= prevLiveVars

    State.put state{ live_vars_map = Map.insert idx liveVars liveVarsMap, in_changed = prevInChanged || inChanged }

computeLiveVars :: LVMonad ()
computeLiveVars = do
    state@LVState{tac_map=tacMap, in_changed=inChanged} <- State.get
    if not inChanged
        then return ()
        else do
            State.put state{ in_changed = False }
            mapM_ computeLiveVarsOfInst [1..(Map.size tacMap - 1)]
            computeLiveVars


getLiveVarsMap :: [TAC.Instruction] -> [Block.Block] -> IO LiveVarsMap
getLiveVarsMap tac blocks = do
    let initialState = getInitialState tac blocks
        moderatoLabel = TAC.Label "moderato"
        moderatoInst = TAC.ThreeAddressCode TAC.NewLabel Nothing (Just moderatoLabel) Nothing
        isModerato (inst,_) = inst == moderatoInst
        moderatoIdx  = snd $ head $ filter isModerato (zip tac [0..])

    finalState <- State.execStateT computeLiveVars initialState
    return $ live_vars_map finalState