module Backend.FlowGraph.InterferenceGraph where

import Backend.FlowGraph.LiveVariables
import qualified Control.Monad.State.Lazy as State
import Control.Monad.Trans
import qualified Backend.TAC.TAC as TAC
import qualified Backend.FlowGraph.Block as Block
import qualified Backend.FlowGraph.FlowGraph as FlowGraph
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Maybe
import Util.Arquitecture

computeInterferenceGraph :: LVMonad()
computeInterferenceGraph = do
    state@LVState{live_vars_map=liveVarsMap} <- State.get
    let liveVarsList = Map.toList liveVarsMap
    mapM_ createEdges liveVarsList

createEdges :: (Idx, LiveVars) -> LVMonad()
createEdges (_, varsSet) = do
    let varsList = Set.toList varsSet
    mapM_ (addEdges varsList) varsList

addEdges :: [TAC.Id] -> TAC.Id -> LVMonad()
addEdges vs u =
    mapM_ (addEdge u) vs

addEdge :: TAC.Id -> TAC.Id -> LVMonad ()
addEdge u v
    | u/= v = do
        state@LVState{ady_map=adyMap} <- State.get
        let adyMap' = Map.insertWith Set.union v (Set.singleton u) $ Map.insertWith Set.union u (Set.singleton v) adyMap
        State.put $ state{ ady_map = adyMap' }
    | otherwise = return ()