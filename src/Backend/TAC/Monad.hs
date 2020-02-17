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

{-# ANN genForBinOp "HLint: warn" #-}
{-# ANN genForComp "HLint: warn" #-}

-- | State of the monad
data TACState = TACState {
    label_count     :: Int,
    temp_count      :: Int,
    inst_count      :: Int,
    bp_map          :: BackpatchMap,
    sym_table       :: AST.SymbolTable 
} deriving (Eq, Show)

type TACMonad = RWS.RWST () [TAC.Instruction] TACState IO

initialState :: AST.SymbolTable -> TACState
initialState = TACState 1 1 0 Map.empty

tacTrue     = TAC.Constant ("true",  AST.Simple "whole")
tacFalse    = TAC.Constant ("false", AST.Simple "whole")

----------------------------------------------------------------------------
----------------------------- Generate TAC ---------------------------------
----------------------------------------------------------------------------

-- | Generate TAC for binary operation
genForBinOp :: AST.Expression -> TAC.Operation -> TACMonad (Maybe TAC.Value, IdxList, IdxList)

-- Or Expression
genForBinOp exp@(AST.OrExp expLeft expRight expType) op@TAC.Or = do
    (_, truelist1, falselist1) <- genForExp expLeft

    inst <- nextInst
    label@(TAC.Label l) <- newLabel
    
    (_, truelist2, falselist2) <- genForExp expRight

    bindLabel falselist1 l
    let truelist    = merge truelist1 truelist2
        falselist   = falselist2

    return (Nothing, truelist, falselist)

-- And Expression
genForBinOp exp@(AST.AndExp expLeft expRight expType) op@TAC.And = do
    (_, truelist1, falselist1) <- genForExp expLeft

    inst <- nextInst
    label@(TAC.Label l) <- newLabel

    (_, truelist2, falselist2) <- genForExp expRight

    bindLabel truelist1 l
    let falselist    = merge falselist1 falselist2
        truelist     = truelist2

    return (Nothing, truelist, falselist)

-- Every other binary expression
genForBinOp exp op = do
    (maybeRValue1, _, _) <- genForExp $ AST.exp_left exp
    (maybeRValue2, _, _) <- genForExp $ AST.exp_right exp
    temp            <- newTemp $ AST.exp_type exp
    genRaw [TAC.ThreeAddressCode op (Just temp) maybeRValue1 maybeRValue2]
    return (Just temp, [], [])

genAndStoreLogicalExp :: AST.Expression -> TACMonad TAC.Value
genAndStoreLogicalExp exp = do
    ---------------------------------------------------------------------------
    -- Store value of left expression inside `temp1`
    (_, t1, f1) <- genForExp exp

    -- if expression is true
    label@(TAC.Label l) <- newLabel
    temp1               <- newTemp $ AST.Simple "whole"
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp1) (Just tacTrue) Nothing]
    bindLabel t1 l
    i1 <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

    -- if expression is false
    label@(TAC.Label l) <- newLabel
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp1) (Just tacFalse) Nothing]
    bindLabel f1 l

    -- jump if true
    label@(TAC.Label l) <- newLabel
    bindLabel [i1] l

    return temp1

-- | Generate three address code for comparators
genForComp :: AST.Expression -> TAC.Operation -> TACMonad (Maybe TAC.Value, IdxList, IdxList)
genForComp exp op
    | AST.exp_type (AST.exp_left exp) == AST.Simple "whole" = do
        temp1 <- genAndStoreLogicalExp $ AST.exp_left exp
        temp2 <- genAndStoreLogicalExp $ AST.exp_right exp

        inst1 <- nextInst
        let truelist = makelist inst1
        genRaw [TAC.ThreeAddressCode op (Just temp1) (Just temp2) Nothing]

        inst2 <- nextInst
        let falselist = makelist inst2
        genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

        return (Nothing, truelist, falselist)

    | otherwise = do
        (Just rValue1, _, _) <- genForExp $ AST.exp_left exp
        (Just rValue2, _, _) <- genForExp $ AST.exp_right exp

        inst1 <- nextInst
        let truelist = makelist inst1
        genRaw [TAC.ThreeAddressCode op (Just rValue1) (Just rValue2) Nothing]

        inst2 <- nextInst
        let falselist = makelist inst2
        genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

        return (Nothing, truelist, falselist)

-- | Generate TAC for new array
genForArray :: TAC.Value -> AST.ASTType -> TACMonad TAC.Value
genForArray size expType = do
    temp <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.New (Just temp) (Just size) Nothing]
    return temp

-- | Generate TAC for LValues
genForLValue :: AST.Expression -> TACMonad (TAC.Value -> TACMonad ())
genForLValue exp@AST.IdExp{AST.exp_entry=Just entry} = do
    let lValue = TAC.Id $ TAC.Var entry
    return $ \rValue -> genRaw [TAC.ThreeAddressCode TAC.Assign (Just lValue) (Just rValue) Nothing]

genForLValue exp@AST.IndexingExp{AST.exp_left=expLeft, AST.exp_right=expRight, AST.exp_type=expType} = do
    (Just temp, _, _) <- genForExp expLeft
    lValue <- newTemp $ AST.exp_type expLeft
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just lValue) (Just temp) Nothing]

    (Just temp, _, _) <- genForExp expRight
    w <- getSize expType
    index <- newTemp $ AST.Simple "quarter"
    genRaw [TAC.ThreeAddressCode TAC.Mult (Just index) (Just $ TAC.Constant (show w, AST.Simple "quarter")) (Just temp) ]

    return $ \rValue -> genRaw [TAC.ThreeAddressCode TAC.Set (Just lValue) (Just index) (Just rValue)]

-- | Generate corresponding TAC to Expression
genForExp :: AST.Expression -> TACMonad (Maybe TAC.Value, IdxList, IdxList)

-- Literal expression
genForExp exp@(AST.LiteralExp expToken expType)
    -- if it's a string
    | expType == AST.Compound "Melody" (AST.Simple "half") = do
        let string = init $ tail $ getStringFromExp exp
            size = length string

        temp <- newTemp expType
        genRaw [TAC.ThreeAddressCode TAC.New (Just temp) (Just $ TAC.Constant (show size, AST.Simple "quarter")) Nothing]
        
        let chars = map (\c -> TAC.Constant (show c, AST.Simple "half")) string
        foldM_ ( pushArrayElement temp 1 ) 0 chars

        return (Just temp, [], [])

    | otherwise = return (Just $ TAC.Constant (Tokens.token expToken, expType), [], [])

-- Melody literals
genForExp exp@AST.MelodyLiteral{AST.exp_exps=expList, AST.exp_type=expType} = do
    let size = length expList
        sizeValue = TAC.Constant (show size, AST.Simple "quarter")
    temp <- genForArray sizeValue expType

    tempList <- mapM genForArrayElement expList
    typeSize <- getSize expType
    foldM_ ( pushArrayElement temp typeSize ) 0 tempList

    return (Just temp, [], [])

genForExp exp@AST.MelodyLiteral'{AST.exp_size=sizeExp, AST.exp_type=expType} = do
    (Just sizeValue, _, _) <- genForExp sizeExp
    temp <- genForArray sizeValue expType
    return (Just temp, [], [])

-- False
genForExp idExp@(AST.IdExp (AST.Id (Tokens.MinToken "min" _ _)) _ _) = do
    nextinst <- nextInst
    let falselist = makelist nextinst
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]
    return (Just tacFalse, [], falselist)

-- True
genForExp idExp@(AST.IdExp (AST.Id (Tokens.MajToken "maj" _ _)) _ _) = do
    nextinst <- nextInst
    let truelist = makelist nextinst
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]
    return (Just tacTrue, truelist, [])

-- Identifier
genForExp idExp@AST.IdExp{AST.exp_type=AST.Simple "whole", AST.exp_entry=Just expEntry} = do
    let entry = TAC.Var expEntry
        value = TAC.Id entry

    inst1 <- nextInst
    let truelist = makelist inst1
    genRaw [TAC.ThreeAddressCode TAC.If Nothing (Just value) Nothing]

    inst2 <- nextInst
    let falselist = makelist inst2
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

    return (Nothing, truelist, falselist)

genForExp idExp@AST.IdExp{AST.exp_entry=Just entry} = return (Just $ TAC.Id $ TAC.Var entry, [], [])

-- For array indexing
genForExp exp@AST.IndexingExp{AST.exp_left=expLeft, AST.exp_right=expRight, AST.exp_type=expType} = do
    (Just temp1, _, _) <- genForExp expLeft
    temp1' <- newTemp $ AST.exp_type expLeft
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp1') (Just temp1) Nothing]

    (Just temp2, _, _) <- genForExp expRight
    w <- getSize expType
    temp2' <- newTemp $ AST.Simple "quarter"
    genRaw [TAC.ThreeAddressCode TAC.Mult (Just temp2') (Just $ TAC.Constant (show w, AST.Simple "quarter")) (Just temp2)]


    temp <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Get (Just temp) (Just temp1') (Just temp2')]
    return (Just temp, [], [])

-- Logical Not
genForExp (AST.NotExp exp expType) = do
    (_, truelist, falselist)   <- genForExp exp
    return (Nothing, falselist, truelist)

-- Addition arithmetic expression
genForExp (AST.NegativeExp exp expType) = do
    (Just rValue, _, _)  <- genForExp exp
    temp            <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Minus (Just temp) (Just rValue) Nothing]
    return (Just temp, [], [])

-- Casting expression
genForExp (AST.CastExp exp (AST.Simple fromType) to@(AST.Simple toType)) = do
    (Just rValue, _, _)  <- genForExp exp
    temp            <- newTemp to
    genRaw [TAC.ThreeAddressCode (TAC.Cast fromType toType) (Just temp) (Just rValue) Nothing]
    return (Just temp, [], [])

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

-- ==
genForExp exp@AST.EqualExp{} = genForComp exp TAC.Eq
-- !=
genForExp exp@AST.NotEqualExp{} = genForComp exp TAC.Neq
-- <
genForExp exp@AST.LessExp{} = genForComp exp TAC.Lt
-- >
genForExp exp@AST.GreaterExp{} = genForComp exp TAC.Gt
-- <=
genForExp exp@AST.LessEqualExp{} = genForComp exp TAC.Lte
-- >=
genForExp exp@AST.GreaterEqualExp{} = genForComp exp TAC.Gte

-- TODO: Pow, Relations, Conditional

-- | Insert a list of raw instructions into final Three Address Code
genRaw :: [TAC.Instruction] -> TACMonad ()
genRaw lst = do
    state@TACState{inst_count=instCount} <- RWS.get
    RWS.tell lst
    RWS.put state{inst_count=instCount+1}

-- | Gen corresponding TAC for blocks
genForList :: [AST.Instruction] -> TACMonad IdxList
genForList [] = return []
genForList stmts@(s:ss) = do
    nextlist1 <- gen s
    label@(TAC.Label l) <- newLabel

    bindLabel nextlist1 l
    genForList ss

-- | Generate corresponding TAC for array elements
genForArrayElement :: AST.Expression -> TACMonad TAC.Value
genForArrayElement astExp =
    case AST.exp_type astExp of
        AST.Simple "whole" -> genAndStoreLogicalExp astExp
        _ -> do
            (Just temp, _, _) <- genForExp astExp
            return temp

-- | Generate corresponding TAC for Instruction
gen :: AST.Instruction -> TACMonad IdxList
gen (AST.VarDecInst _) = return []

gen inst@(AST.AssignInst leftExp rightExp) = do
    genAssignment <- genForLValue leftExp
    case AST.exp_type rightExp of
        AST.Simple "whole" -> do
            (_, truelist, falselist)   <- genForExp rightExp

            nextinst <- nextInst
            label@(TAC.Label l) <- newLabel

            bindLabel truelist l
            genAssignment tacTrue

            nextinst <- nextInst
            let nextlist = makelist nextinst
            genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

            nextinst <- nextInst
            label@(TAC.Label l) <- newLabel

            bindLabel falselist l
            genAssignment tacFalse

            return nextlist

        _ -> do
            (Just rValue, _, _)  <- genForExp rightExp
            genAssignment rValue
            return []

gen AST.IfInst{AST.inst_exp=instExp, AST.inst_inst=instInst, AST.inst_else=instElse} = do
    (_, truelist, falselist) <- genForExp instExp
    nextinst <- nextInst
    label@(TAC.Label l) <- newLabel

    bindLabel truelist l
    nextlist1 <- gen instInst

    case instElse of
        Nothing -> return $ merge falselist nextlist1
        Just inst1 -> do
            nextinst1 <- nextInst
            genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

            nextinst2 <- nextInst
            label@(TAC.Label l) <- newLabel
            bindLabel falselist l

            nextlist2 <- gen inst1
            let t = merge (makelist nextinst1) nextlist2
            return $ merge nextlist1 t

gen AST.BlockInst{AST.inst_block=instBlock} = 
    genForList $ AST.statements instBlock

gen AST.SharpExp{AST.inst_exp=instExp} =
    genForAbrev instExp TAC.Add

gen AST.FlatExp{AST.inst_exp=instExp} =
    genForAbrev instExp TAC.Sub

gen AST.FreeInst{AST.inst_exp=instExp} = do
    (Just addr, _, _) <- genForExp instExp
    genRaw [TAC.ThreeAddressCode TAC.Free Nothing (Just addr) Nothing]
    return []

-- | Generate TAC for increment or decrement instruction
genForAbrev :: AST.Expression -> TAC.Operation -> TACMonad IdxList
genForAbrev exp op = do
    let expType = AST.exp_type exp
        one = TAC.Constant ("1", expType)

    genAssignment <- genForLValue exp
    (Just element, _, _) <- genForExp exp

    rValue <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Add (Just rValue) (Just one) (Just element)]

    genAssignment rValue
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

----------------------------------------------------------------------------
--------------------------- Monadic helpers --------------------------------
----------------------------------------------------------------------------

newLabel :: TACMonad TAC.Value
newLabel = do
    state@TACState{label_count=labelCount} <- RWS.get
    RWS.put state{label_count=labelCount+1}

    let label = TAC.Label labelCount
    genRaw [TAC.ThreeAddressCode TAC.NewLabel Nothing (Just label) Nothing]

    return label

newTemp :: AST.ASTType -> TACMonad TAC.Value
newTemp astType = do
    state@TACState{temp_count=tempCount} <- RWS.get
    RWS.put state{temp_count=tempCount+1}
    return $ TAC.Id $ TAC.Temp ("_t" ++ show tempCount) astType

nextInst :: TACMonad Int
nextInst = do
    state@TACState{inst_count=instCount} <- RWS.get
    return instCount

pushArrayElement :: TAC.Value -> Int -> Int -> TAC.Value -> TACMonad Int
pushArrayElement addr w offset rValue = do
    genRaw [TAC.ThreeAddressCode TAC.Set (Just addr) (Just $ TAC.Constant (show offset, AST.Simple "quarter")) (Just rValue) ]
    return $ offset + w

-- | Get chain from a symbol
getChain :: String -> TACMonad (Maybe [AST.Entry])
getChain symbol = do
    TACState{sym_table=table} <- RWS.get
    -- Get chain of matching entries
    return $ Map.lookup symbol table

-- | Get size of a type
getSize :: AST.ASTType -> TACMonad Int
getSize AST.Compound{} = return 4
getSize AST.Simple{AST.type_str=typeStr} = do
    (Just [ AST.Entry{ AST.entry_category = cat } ]) <- getChain typeStr
    return $ AST.type_size cat

----------------------------------------------------------------------------
---------------------------------- Helpers ---------------------------------
----------------------------------------------------------------------------

-- | Get name from Id
getStringFromId :: AST.Id -> String
getStringFromId = Tokens.token . AST.id_token

-- | Get name from IdExp
getStringFromExp :: AST.Expression -> String
getStringFromExp AST.IdExp{AST.exp_id=expId} = getStringFromId expId
getStringFromExp AST.LiteralExp{AST.exp_token=expToken} = Tokens.token expToken

-- | IdExp to TAC.Id
expToEntry :: AST.Expression -> TAC.Id
expToEntry (AST.IdExp _ _ (Just expEntry)) = TAC.Var expEntry