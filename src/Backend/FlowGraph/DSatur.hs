module Backend.FlowGraph.DSatur where

import qualified AST
import Backend.FlowGraph.LiveVariables
import Backend.FlowGraph.InterferenceGraph
import qualified Control.Monad.State.Lazy as State
import Control.Monad.Trans
import qualified Backend.TAC.TAC as TAC
import qualified Backend.FlowGraph.Block as Block
import qualified Backend.FlowGraph.FlowGraph as FlowGraph
import qualified Backend.TAC.Monad as TACMonad
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Util.Arquitecture
import Control.Monad

type Color = Reg

colors = generalPurposeRegs
colorsSet = Set.fromList colors
k = numberOfRegs+1

----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

getDegree :: TAC.Id -> LVMonad Int
getDegree var = do
    LVState{ady_map=adyMap} <- State.get
    return $ fromMaybe 0 $ Map.lookup var adyMap >>= \set -> return $ Set.size set

getVarWithMaximalDegree :: [TAC.Id] -> LVMonad (Maybe TAC.Id)
getVarWithMaximalDegree vars@(v:_) = do
    let getBest p@(bestVar, bestDegree) p'@(var, degree) = if bestDegree < degree then return p' else return p
    d <- getDegree v
    (bestVar, _) <- foldM (\p@(bestVar, bestDegree) var -> getDegree var >>= \degree -> getBest p (var, degree)) (v, d) vars
    return $ Just bestVar

getVarWithMaximalDegree [] = return Nothing

computeDSaturs :: LVMonad ()
computeDSaturs = do
    state@LVState{ ady_map=adyMap } <- State.get
    mapM_ computeDSatur $ Map.toList adyMap

getNeighboursColors :: TAC.Id -> Set.Set TAC.Id -> LVMonad [Color]
getNeighboursColors var ady = do
    state@LVState{ var_reg_map=varRegMap } <- State.get
    return $ nub $ mapMaybe (`Map.lookup` varRegMap) $ Set.toList ady

removeVarFromDSaturEntry var oldDSatur dSaturMap =
    if null value'
        then Map.delete oldDSatur dSaturMap
        else dSaturMap'
    where   value = fromMaybe [] $ Map.lookup oldDSatur dSaturMap
            value' = filter (var /=) value
            dSaturMap' = Map.insert oldDSatur value' dSaturMap 

computeDSatur :: (TAC.Id, Set.Set TAC.Id) -> LVMonad Int
computeDSatur (var, ady) = do
    state@LVState{ var_reg_map=varRegMap, d_satur_map=dSaturMap, id_d_satur_map=idDSaturMap } <- State.get
    let oldDSatur = fromMaybe 0 $ Map.lookup var idDSaturMap
        dSaturMapWithoutVar = removeVarFromDSaturEntry var oldDSatur dSaturMap

    if not (Map.member var varRegMap)
        then do
            regs <- getNeighboursColors var ady
            let dSatur = length regs
                -- Update DSatur Map, first deleting `var` from old DSatur entry and then inserting it into new DSatur entry.
                dSaturMap' = Map.insertWith (++) dSatur [var] dSaturMapWithoutVar
                idDSaturMap' = Map.insert var dSatur idDSaturMap

            State.put $ state{ d_satur_map = dSaturMap', id_d_satur_map = idDSaturMap' }
            return dSatur
        else do
            let idDSaturMap' = Map.insert var 0 idDSaturMap
            State.put $ state{ d_satur_map = dSaturMapWithoutVar, id_d_satur_map = idDSaturMap' }
            return 0

getVarWithMaximalDSatur :: LVMonad (Maybe TAC.Id)
getVarWithMaximalDSatur = do
    LVState{d_satur_map=dSaturMap} <- State.get
    let maybeValue = Map.lookupLE k dSaturMap
    case maybeValue of
        Just value -> do
            let maxDSaturVars = snd value
            getVarWithMaximalDegree maxDSaturVars
        Nothing -> return Nothing

colorVar :: TAC.Id -> LVMonad ()
colorVar var = do
    state@LVState{ady_map=adyMap, var_reg_map=varRegMap} <- State.get

    let ady = fromJust $ Map.lookup var adyMap

    neighbourColors <- getNeighboursColors var ady
    let availableColors = Set.toList $ colorsSet `Set.difference` Set.fromList neighbourColors

    if null availableColors
        then addVarToSpill var
        else do
            let color = head availableColors
            State.put $ state{ var_reg_map = Map.insert var color varRegMap }

    computeDSaturs

addVarToSpill :: TAC.Id -> LVMonad ()
addVarToSpill var = do
    state@LVState{to_spill=toSpillSet, var_reg_map=varRegMap} <- State.get
    State.put $ state{ to_spill = Set.insert var toSpillSet,
        var_reg_map = Map.insert var (-1) varRegMap }

----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

dSaturAlgorithm :: LVMonad ()
dSaturAlgorithm = do
    computeLiveVars
    computeInterferenceGraph
    varsList <- getVarsList
    Just u <- getVarWithMaximalDegree varsList
    colorVar u
    dSaturRecursion

dSaturRecursion :: LVMonad ()
dSaturRecursion = do
    LVState{var_reg_map=varRegMap, ady_map=adyMap, d_satur_map=dSaturMap} <- State.get
    varsList <- getVarsList
    varMaybe <- getVarWithMaximalDSatur
    case varMaybe of
        Just var -> do
            colorVar var
            dSaturRecursion
        Nothing ->
            if Map.size varRegMap == Map.size adyMap
                then return ()
                else do
                    let coloredVarsSet = Map.keysSet varRegMap
                        allVarsSet = Map.keysSet adyMap
                        nonColoredVars = Set.toList $ allVarsSet `Set.difference` coloredVarsSet
                    Just u <- getVarWithMaximalDegree nonColoredVars
                    colorVar u
                    dSaturRecursion


----------------------------------------------------------------------------------------------------------------------
--------------------------------Gen TAC for Epilogue and Prologue-----------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

genEpilProl :: [(Idx, TAC.Instruction)] -> LVMonad ()
genEpilProl [] = return ()
genEpilProl ((idx, inst):tac) = do
    state@LVState{var_reg_map = varRegMap, new_tac = newTac, live_vars_map = liveVarsMap} <- State.get
    case TAC.tacOperation inst of
        TAC.Call -> do
            let liveVarsSet0 = fromJust $ Map.lookup idx liveVarsMap
                liveVarsSet1 = fromJust $ Map.lookup (idx+1) liveVarsMap
                liveVars = Set.toList $ liveVarsSet0 `Set.intersection` liveVarsSet1
                regs = mapMaybe (`Map.lookup` varRegMap) liveVars
                varsRegs = zip liveVars regs
                newTac' = inst : insertRawSpillsForEpilProl TAC.Store varsRegs newTac
                newTac'' = insertRawSpillsForEpilProl TAC.Load varsRegs newTac'
            State.put $ state{ new_tac = newTac'' }

        _ -> genSpillIfNecessary inst
        -- _ -> return ()
    genEpilProl tac

insertRawSpillsForEpilProl :: TAC.Operation -> [(TAC.Id, Reg)] -> [TAC.Instruction] -> [TAC.Instruction]
insertRawSpillsForEpilProl op varsRegs tac = fromJust $ foldl (\(Just insts) p -> maybe (Just insts) (\i -> Just (i:insts)) $ genRawSpillForEpilProl op p ) (Just tac) varsRegs

genRawSpillForEpilProl :: TAC.Operation -> (TAC.Id, Reg) -> Maybe TAC.Instruction
genRawSpillForEpilProl _ (_, -1) = Nothing
genRawSpillForEpilProl op (var, reg) =
    Just $ TAC.ThreeAddressCode op (Just $ TAC.Id $ intToReg reg (TAC.getTypeOfId var)) (Just $ TAC.Id var) Nothing

----------------------------------------------------------------------------------------------------------------------
-------------------------------------------Gen TAC for Spills---------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
genSpillIfNecessary :: TAC.Instruction -> LVMonad ()
genSpillIfNecessary inst = do
    state@LVState{new_tac=newTac} <- State.get
    (epilogue, newInst, prologue) <- foldM (\(epil, inst', prol) valuePos -> getRegOrGenSpill valuePos inst' >>= \(epil', inst'', prol') -> return (epil'++epil, inst'', prol'++prol)) ([], inst, []) [0,1,2]
    State.put $ state{ new_tac = epilogue ++ (newInst : prologue) ++ newTac }

-- | return a tuple (epilogue_of_spill, new_inst, prologue_of_spill)
getRegOrGenSpill :: Int -> TAC.Instruction -> LVMonad ([TAC.Instruction], TAC.Instruction, [TAC.Instruction])
getRegOrGenSpill valuePos inst = do
    state@LVState{var_reg_map=varRegMap} <- State.get

    let maybeValue = case valuePos of 
                        0 -> TAC.tacLvalue inst
                        1 -> TAC.tacRvalue1 inst
                        2 -> TAC.tacRvalue2 inst
        defaultRet = ([], inst, [])

    case maybeValue of
        -- If it is not a value
        Nothing -> return defaultRet
        -- otherwise
        Just value -> do
            let maybeId = TAC.getId value
            case maybeId of
                -- if it is not a variable/temp/etc
                Nothing -> return defaultRet
                -- otherwise
                Just var -> do
                    liftIO $ print var
                    let maybeReg = Map.lookup var varRegMap
                    case maybeReg of
                        -- It is never alive. TODO: Check if this line is correct
                        Nothing -> return defaultRet
                        Just reg ->
                            if reg == -1
                                -- If it should generate a spill
                                then do
                                    -- base[4], base[8] and base[12] are auxiliars for spills.
                                    let auxReg = TAC.Id $ intToReg (valuePos + head colors) (TAC.getType value)
                                        idxValue = TACMonad.toQuarterConstant valuePos
                                        -- Spill instructions
                                        auxStoreProl    = TAC.ThreeAddressCode TAC.Store (Just auxReg) (Just TACMonad.base) (Just idxValue)
                                        auxLoadProl     = TAC.ThreeAddressCode TAC.Load (Just auxReg) maybeValue Nothing
                                        newInst         = substituteValueByReg inst valuePos auxReg
                                        auxStoreEpil    = TAC.ThreeAddressCode TAC.Store (Just auxReg) maybeValue Nothing
                                        auxLoadEpil     = TAC.ThreeAddressCode TAC.Load (Just auxReg) (Just TACMonad.base) (Just idxValue)

                                    return ([auxLoadEpil, auxStoreEpil], newInst, [auxLoadProl, auxStoreProl])
                                
                                -- otherwise
                                else do
                                    let regValue = TAC.Id $ intToReg reg (TAC.getType value)
                                    return ([], substituteValueByReg inst valuePos regValue, [])
                               


----------------------------------------------------------------------------------------------------------------------
------------------------------------------------- Helpers ------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

intToReg :: Int -> AST.ASTType -> TAC.Id
intToReg num = TAC.Reg ("$" ++ show num)

getUsedVars :: TAC.Instruction -> Set.Set TAC.Id
getUsedVars inst = Set.fromList $ TAC.getIds inst

substituteValueByReg :: TAC.Instruction -> Int -> TAC.Value -> TAC.Instruction
substituteValueByReg inst valuePos auxReg =
    case valuePos of
        0 -> inst{ TAC.tacLvalue=Just auxReg }
        1 -> inst{ TAC.tacRvalue1=Just auxReg }
        2 -> inst{ TAC.tacRvalue2=Just auxReg }

----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

run = do
    dSaturAlgorithm
    LVState{tac_map=tacMap} <- State.get
    genEpilProl $ Map.toList tacMap

    state@LVState{new_tac=newTac} <- State.get
    State.put state{ new_tac = reverse newTac }

returnState :: [TAC.Instruction] -> [Block.Block] -> IO LVState
returnState tac blocks = do
    let initialState = getInitialState tac blocks
    State.execStateT run initialState