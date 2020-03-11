module Backend.FlowGraph.FlowGraph where

import qualified Backend.TAC.Monad as TACMonad
import qualified Backend.TAC.TAC as TAC
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import qualified Backend.FlowGraph.Block as Block
import qualified Data.Graph as Graph

-- | From TAC, get a Set of Labels that appear in a GoTo/If instruction
getReachableLabels :: [TAC.Instruction] -> Set.Set String -> Set.Set String
getReachableLabels [] set = set
getReachableLabels (inst:tac) set =
    case TAC.tacRvalue2 inst of
        Just (TAC.Label labelStr) ->
            getReachableLabels tac $ Set.insert labelStr set
        _ ->
            case TAC.tacOperation inst of
                TAC.Call ->
                    let Just (TAC.Label labelStr) = TAC.tacRvalue1 inst in
                        getReachableLabels tac $ Set.insert labelStr set
                _ ->
                    getReachableLabels tac set

-- | From TAC, get indexes of instructions that define a reachable Label, and filter out the non reachable ones.
getReachedByGoto :: [TAC.Instruction] -> Int -> Set.Set String -> Set.Set Int -> ([TAC.Instruction], Set.Set Int)
getReachedByGoto ( newLabelInst@(TAC.ThreeAddressCode TAC.NewLabel Nothing (Just (TAC.Label labelStr)) Nothing) :tac) idx reachableLabels set =
    if Set.member labelStr reachableLabels
        then let (tac', set') = getReachedByGoto tac (idx+1) reachableLabels $ Set.insert idx set in (newLabelInst:tac', set')
        else getReachedByGoto tac idx reachableLabels set

getReachedByGoto [] _ _ set = ([], set)

getReachedByGoto (inst:tac) idx reachableLabels set =
    let (tac', set') = getReachedByGoto tac (idx+1) reachableLabels set in
        (inst:tac', set')

-- | From TAC, get indexes of instructions that follow a GoTo/If instruction
getFollowingGoto :: [TAC.Instruction] -> Int -> Set.Set Int -> Set.Set Int
getFollowingGoto [] _ set = set
getFollowingGoto (inst:tac) idx set =
    if TAC.tacOperation inst `elem` TAC.jumpInsts
        then getFollowingGoto tac (idx+1) $ Set.insert (idx+1) set
        else getFollowingGoto tac (idx+1) set

-- | From TAC, return an optimized TAC and set of block leader instructions
getBlockLeaders :: [TAC.Instruction] -> ([TAC.Instruction], Set.Set Int)
getBlockLeaders tac =
    let reachableLabels = getReachableLabels tac Set.empty
        (tac', reachableInsts) = getReachedByGoto tac 0 reachableLabels $ Set.singleton 1 in
            (tac', getFollowingGoto tac' 0 reachableInsts)

-- | From TAC, return a Map from Label (String) to its index in the TAC.
getLabelIdxs :: [TAC.Instruction] -> Int -> Map.Map String Int -> Map.Map String Int
getLabelIdxs [] _ map = map

getLabelIdxs (TAC.ThreeAddressCode TAC.NewLabel Nothing (Just (TAC.Label labelStr)) Nothing : tac) idx map =
    getLabelIdxs tac (idx+1) $ Map.insert labelStr idx map

getLabelIdxs (inst:tac) idx map =
    getLabelIdxs tac (idx+1) map

getBlocks :: [TAC.Instruction] -> Int -> Int -> Map.Map String Int -> Set.Set Int -> Set.Set Block.Edge -> [TAC.Instruction] -> Block.BlockList -> Block.BlockList
getBlocks [] _ _ _ _ _ _ blocksList = blocksList
getBlocks currTac@(inst:tac) idx start labelMap blockLeaders edgesSet blockInsts blocksList
    -- If it is a block leader but not the current one
    | Set.member idx blockLeaders && idx /= start =
        case start of
            -1 -> -- If it is first block
                getBlocks currTac idx idx labelMap blockLeaders Set.empty [] blocksList
            _ -> -- Otherwise
                let prevInst = head blockInsts
                    -- Add an edge from closing block to this one if closing one does not terminate in a goto.
                    edgesSet' = if TAC.tacOperation prevInst `elem` [TAC.GoTo, TAC.Return] then edgesSet else Set.insert idx edgesSet
                    -- Generate closing block
                    block = Block.Block (reverse blockInsts) start idx Set.empty edgesSet'
                    -- Insert closing block into set of blocks
                    blocksList' = block : blocksList in
                        
                getBlocks currTac idx idx labelMap blockLeaders Set.empty [] blocksList'

    -- If instruction is a jump, generate an edge to destiny block.
    | TAC.tacOperation inst `elem` TAC.jumpInsts =
        -- Lookup for Label's index
        let Just (TAC.Label labelStr) = TAC.getDestiny inst
            Just labelIdx = Map.lookup labelStr labelMap
            -- Insert new edge
            edgesSet' = Set.insert labelIdx edgesSet
            -- Add instruction to block's instructions
            blockInsts' = (inst:blockInsts) in

        getBlocks tac (idx+1) start labelMap blockLeaders edgesSet' blockInsts' blocksList

    | TAC.tacOperation inst `notElem` [TAC.Entry, TAC.Exit] =
        -- Add instruction to block's instructions
        let blockInsts' = (inst:blockInsts) in
        getBlocks tac (idx+1) start labelMap blockLeaders edgesSet blockInsts' blocksList

    | otherwise =
        getBlocks tac (idx+1) start labelMap blockLeaders edgesSet blockInsts blocksList
