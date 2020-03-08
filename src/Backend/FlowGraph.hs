module Backend.FlowGraph where

import qualified Backend.TAC.Monad as TACMonad
import qualified Backend.TAC.TAC as TAC
import qualified Data.Set as Set

-- | From TAC, get a Set of Labels that appear in a GoTo/If instruction
getReachableLabels :: [TAC.Instruction] -> Set.Set String -> Set.Set String
getReachableLabels [] set = set
getReachableLabels (inst:tac) set =
    case TAC.tacRvalue2 inst of
        Just (TAC.Label labelStr) ->
            getReachableLabels tac $ Set.insert labelStr set
        _ ->
            case TAC.tacOperand inst of
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
    if TAC.tacOperand inst `elem` [TAC.GoTo, TAC.If, TAC.IfFalse, TAC.Eq, TAC.Neq, TAC.Lt, TAC.Gt, TAC.Lte, TAC.Gte, TAC.Call]
        then getFollowingGoto tac (idx+1) $ Set.insert (idx+1) set
        else getFollowingGoto tac (idx+1) set

getBlockLeaders :: [TAC.Instruction] -> ([TAC.Instruction], Set.Set Int)
getBlockLeaders tac =
    let reachableLabels = getReachableLabels tac Set.empty
        (tac', reachableInsts) = getReachedByGoto tac 0 reachableLabels $ Set.singleton 0 in
            (tac', getFollowingGoto tac' 0 reachableInsts)