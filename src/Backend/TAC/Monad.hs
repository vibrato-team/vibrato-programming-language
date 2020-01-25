module Backend.TAC.Monad where
    
import qualified Control.Monad.RWS.Lazy as RWS
import qualified Backend.TAC.TAC as TAC
import qualified AST
import qualified Frontend.Tokens as Tokens
import qualified Control.Monad
import qualified Data.Map.Lazy as Map
import Control.Monad.RWS.Lazy

type Idx = Int
type IdxList = [Idx]
type BackpatchMap = Map.Map Idx IdxList

-- | State of the monad
data TACState = TACState {
    label_count     :: Int,
    temp_count      :: Int,
    inst_count      :: Int,
    bp_map   :: BackpatchMap
} deriving (Eq, Show)

type TACMonad = RWS.RWST () [TAC.Instruction] TACState IO

initialState :: TACState
initialState = TACState 1 1 0 Map.empty

tacTrue     = TAC.Constant ("true",  AST.Simple "whole")
tacFalse    = TAC.Constant ("false", AST.Simple "whole")

newLabel :: TACMonad TAC.Value
newLabel = do
    state@TACState{label_count=labelCount} <- RWS.get
    RWS.put state{label_count=labelCount+1}
    return $ TAC.Label labelCount

newTemp :: AST.Type -> TACMonad TAC.Value
newTemp astType = do
    state@TACState{temp_count=tempCount} <- RWS.get
    RWS.put state{temp_count=tempCount+1}
    return $ TAC.Variable $ TAC.Entry ("_t" ++ show tempCount) astType Nothing

nextInst :: TACMonad Int
nextInst = do
    state@TACState{inst_count=instCount} <- RWS.get
    return instCount

-- | Get name from Id
getStringFromId :: AST.Id -> String
getStringFromId = Tokens.token . AST.id_token

-- | Get name from IdExp
getStringFromExp :: AST.Expression -> String
getStringFromExp AST.IdExp{AST.exp_id=expId} = getStringFromId expId
getStringFromExp AST.Literal{AST.exp_token=expToken} = Tokens.token expToken

-- | IdExp to TAC.Entry
expToEntry :: AST.Expression -> TAC.Entry
expToEntry (AST.IdExp expId expType expScope) = TAC.Entry (getStringFromId expId) expType (Just expScope)

-- | Generate TAC for binary operation
{-# ANN genForBinOp "HLint: warn" #-}
genForBinOp :: AST.Expression -> TAC.Operation -> TACMonad (TAC.Value, IdxList, IdxList)

-- Or Expression
genForBinOp exp@(AST.OrExp expLeft expRight expType) op@TAC.Or = do
    (rValue1, truelist1, falselist1) <- genForExp expLeft
    inst <- nextInst
    (rValue2, truelist2, falselist2) <- genForExp expRight

    bindLabel falselist1 inst
    let truelist    = merge truelist1 truelist2
        falselist   = falselist2

    temp <- newTemp expType
    -- genRaw [TAC.ThreeAddressCode op (Just temp) (Just rValue1) (Just rValue2)]
    return (temp, truelist, falselist)

-- And Expression
genForBinOp exp@(AST.AndExp expLeft expRight expType) op@TAC.And = do
    (rValue1, truelist1, falselist1) <- genForExp expLeft
    inst <- nextInst
    (rValue2, truelist2, falselist2) <- genForExp expRight

    bindLabel truelist1 inst
    let falselist    = merge falselist1 falselist2
        truelist     = truelist2

    temp <- newTemp expType
    -- genRaw [TAC.ThreeAddressCode op (Just temp) (Just rValue1) (Just rValue2)]
    return (temp, truelist, falselist)

-- Every other binary expression
genForBinOp exp op = do
    (rValue1, _, _) <- genForExp $ AST.exp_left exp
    (rValue2, _, _) <- genForExp $ AST.exp_right exp
    temp            <- newTemp $ AST.exp_type exp
    genRaw [TAC.ThreeAddressCode op (Just temp) (Just rValue1) (Just rValue2)]
    return (temp, [], [])

-- | Generate corersponding TAC for LValue
genForLValue idExp@AST.IdExp{} = return (TAC.Variable $ expToEntry idExp, [], [])

-- | Generate corresponding TAC to Expression
genForExp :: AST.Expression -> TACMonad (TAC.Value, IdxList, IdxList)

-- Literal expression
genForExp exp@(AST.Literal expToken expType) = return (TAC.Constant (Tokens.token expToken, expType), [], [])

-- False
genForExp idExp@(AST.IdExp (AST.Id (Tokens.MinToken "min" _ _)) _ _) = do
    nextinst <- nextInst
    let falselist = makelist nextinst
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]
    return (tacFalse, [], falselist)

-- True
genForExp idExp@(AST.IdExp (AST.Id (Tokens.MajToken "maj" _ _)) _ _) = do
    nextinst <- nextInst
    let truelist = makelist nextinst
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]
    return (tacTrue, truelist, [])

-- Identifier
genForExp idExp@AST.IdExp{AST.exp_type=AST.Simple "whole", AST.exp_scope=expScope} = do
    let identifier = getStringFromExp idExp
        entry = TAC.Entry identifier (AST.Simple "whole") (Just expScope)
        value = TAC.Variable entry

    inst1 <- nextInst
    let truelist = makelist inst1
    genRaw [TAC.ThreeAddressCode TAC.If Nothing (Just value) Nothing]

    inst2 <- nextInst
    let falselist = makelist inst2
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

    return (value, truelist, falselist)

genForExp idExp@AST.IdExp{} = genForLValue idExp

-- Logical Not
genForExp (AST.NotExp exp expType) = do
    (rValue, truelist, falselist)   <- genForExp exp
    temp                            <- newTemp expType
    -- genRaw [TAC.ThreeAddressCode TAC.Not (Just temp) (Just rValue) Nothing]
    return (temp, falselist, truelist)

-- Addition arithmetic expression
genForExp (AST.NegativeExp exp expType) = do
    (rValue, _, _)  <- genForExp exp
    temp            <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Minus (Just temp) (Just rValue) Nothing]
    return (temp, [], [])

-- Casting expression
genForExp (AST.CastExp exp (AST.Simple fromType) to@(AST.Simple toType)) = do
    (rValue, _, _)  <- genForExp exp
    temp            <- newTemp to
    genRaw [TAC.ThreeAddressCode (TAC.Cast fromType toType) (Just temp) (Just rValue) Nothing]
    return (temp, [], [])

-- Addition arithmetic expression
genForExp exp@AST.AdditionExp{} = genForBinOp exp TAC.Add

-- Substraction arithmetic expression
genForExp exp@AST.SubstractionExp{} = genForBinOp exp TAC.Sub

-- Module arithmetic expression
genForExp exp@AST.ModExp{} = genForBinOp exp TAC.Mod

-- Multiplication arithmetic expression
genForExp exp@AST.MultExp{} = genForBinOp exp TAC.Mult

-- Division arithmetic expression
genForExp exp@AST.DivExp{} = genForBinOp exp TAC.Div

-- Logical And
genForExp exp@AST.AndExp{} = genForBinOp exp TAC.And

-- Logical Or
genForExp exp@AST.OrExp{} = genForBinOp exp TAC.Or

-- TODO: Pow, Relations, Conditional

-- | Insert a list of raw instructions into final Three Address Code
genRaw :: [TAC.Instruction] -> TACMonad ()
genRaw lst = do
    state@(TACState _ _ instCount _) <- RWS.get
    RWS.tell lst
    RWS.put state{inst_count=instCount+1}

-- | Gen corresponding TAC for blocks
genForList :: [AST.Instruction] -> TACMonad IdxList
genForList [] = return []
genForList stmts@(s:ss) = do
    nextlist1 <- gen s
    nextinst <- nextInst
    bindLabel nextlist1 nextinst
    genForList ss

-- | Generate corresponding TAC for Instruction
gen :: AST.Instruction -> TACMonad IdxList
gen (AST.VarDecInst _) = return []

gen (AST.AssignInst leftExp rightExp) = do
    (lValue, _, _) <- genForLValue leftExp
    case AST.exp_type rightExp of
        AST.Simple "whole" -> do
            (rValue, truelist, falselist)   <- genForExp rightExp

            nextinst <- nextInst
            bindLabel truelist nextinst
            genRaw [TAC.ThreeAddressCode TAC.Assign (Just lValue) (Just tacTrue) Nothing]

            nextinst <- nextInst
            let nextlist = makelist nextinst
            genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

            nextinst <- nextInst
            bindLabel falselist nextinst
            genRaw [TAC.ThreeAddressCode TAC.Assign (Just lValue) (Just tacFalse) Nothing ]

            return nextlist 

        _ -> do
            (rValue, _, _)  <- genForExp rightExp
            genRaw [TAC.ThreeAddressCode TAC.Assign (Just lValue) (Just rValue) Nothing]
            return []
----------------------------------------------------------------------------
----------------------------- Backpatching ---------------------------------
----------------------------------------------------------------------------
makelist :: Idx -> IdxList
makelist inst = [inst]

merge :: IdxList -> IdxList -> IdxList
merge [] xs = xs
merge xs [] = xs
merge l1@(x:xs) l2@(y:ys)
    | x <= y = x : merge xs l2
    | otherwise = y : merge l1 ys

bindLabel :: IdxList -> Idx -> TACMonad ()
bindLabel lst idx = do
    state <- RWS.get
    let bpMap   = bp_map state
        bpMap'  = Map.insertWith merge idx lst bpMap
    RWS.put state{bp_map=bpMap'}

----------------------------------------------------------------------------
----------------------------- For executing monad -----------------------------
----------------------------------------------------------------------------
-- getTacForInstructions :: BackpatchMap -> [TAC.Instruction] -> TAC.Instruction
-- getTacForInstructions bpMap insts =

-- | For each key, backpatch the instructions of its value
backpatchAll :: BackpatchMap -> [TAC.Instruction] -> [TAC.Instruction]
backpatchAll bpMap insts =
    let f insts' label idxs = (backpatch label idxs insts' 0, idxs) in
        fst $ Map.mapAccumWithKey f insts bpMap


-- | Traverse list of indexes and list of instructions and backpatch label
backpatch :: Int -> IdxList -> [TAC.Instruction] -> Idx -> [TAC.Instruction]
backpatch _ [] insts _ = insts
backpatch label l1@(idx:idxs) (inst:insts) i
    | idx /= i = inst : backpatch label l1 insts (i+1)
    | otherwise = inst{TAC.tacRvalue2=Just $ TAC.Label label} : backpatch label idxs insts (i+1)
