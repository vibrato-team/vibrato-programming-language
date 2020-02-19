module Backend.TAC.Monad where
    
import qualified Control.Monad.RWS.Lazy as RWS
import qualified Backend.TAC.TAC as TAC
import qualified AST
import qualified Frontend.Tokens as Tokens
import qualified Control.Monad
import qualified Data.Map.Lazy as Map
import Control.Monad.RWS.Lazy
import qualified Frontend.Parser.Monad as PMonad
import qualified Frontend.Parser.Parser as Parser

type Label = String
type InstList = [Int]
type BackpatchMap = Map.Map Label InstList

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

trueConstant    = TAC.Constant ("true",  AST.Simple "whole")
falseConstant   = TAC.Constant ("false", AST.Simple "whole")
arqWordConstant = TAC.Constant (show PMonad.arqWord, AST.Simple "quarter")
zeroConstant    = TAC.Constant ("0", AST.Simple "quarter")
base            = TAC.Id $ TAC.Temp "_base" $ AST.Simple "quarter"

toQuarterConstant :: (Show a) => a -> TAC.Value 
toQuarterConstant x = TAC.Constant (show x, AST.Simple "quarter")

----------------------------------------------------------------------------
----------------------------- Generate TAC ---------------------------------
----------------------------------------------------------------------------

-- | Generate TAC for binary operation
genForBinOp :: AST.Expression -> TAC.Operation -> TACMonad (Maybe TAC.Value, InstList, InstList)

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

genAndBindLogicalExp :: AST.Expression -> TACMonad TAC.Value
genAndBindLogicalExp exp = do
    ---------------------------------------------------------------------------
    -- Store value of left expression inside `temp1`
    (_, t1, f1) <- genForExp exp

    -- if expression is true
    label@(TAC.Label l) <- newLabel
    temp1               <- newTemp $ AST.Simple "whole"
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp1) (Just trueConstant) Nothing]
    bindLabel t1 l
    i1 <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

    -- if expression is false
    label@(TAC.Label l) <- newLabel
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp1) (Just falseConstant) Nothing]
    bindLabel f1 l

    -- jump if true
    label@(TAC.Label l) <- newLabel
    bindLabel [i1] l

    return temp1

-- | Generate three address code for comparators
genForComp :: AST.Expression -> TAC.Operation -> TACMonad (Maybe TAC.Value, InstList, InstList)
genForComp exp op
    | AST.exp_type (AST.exp_left exp) == AST.Simple "whole" = do
        temp1 <- genAndBindLogicalExp $ AST.exp_left exp
        temp2 <- genAndBindLogicalExp $ AST.exp_right exp

        inst1 <- nextInst
        let truelist = makelist inst1
        genRaw [TAC.ThreeAddressCode op (Just temp1) (Just temp2) Nothing]

        inst2 <- nextInst
        let falselist = makelist inst2
        genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

        return (Nothing, truelist, falselist)

    | AST.type_str (AST.exp_type (AST.exp_left exp)) == "Melody" = do
        (Just temp1, _, _) <- genForExp $ AST.exp_left exp
        (Just temp2, _, _) <- genForExp $ AST.exp_right exp
        genForArrayComp temp1 temp2

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

-- | Get size to allocate
getSizeForArray :: TAC.Value -> AST.ASTType -> TACMonad TAC.Value
getSizeForArray len expType@AST.Compound{AST.type_type=innerType} = do
    let quarterType = AST.Simple "quarter"
    -- Size * width
    w <- getSize innerType
    temp0 <- newTemp quarterType
    genRaw [TAC.ThreeAddressCode TAC.Mult (Just temp0) (Just $ TAC.Constant (show w, AST.Simple "quarter")) (Just len)]

    -- First element is an integer of a word with the size
    size <- newTemp quarterType
    genRaw [TAC.ThreeAddressCode TAC.Add (Just size) (Just temp0) (Just arqWordConstant)]

    return size

getSizeForArray _ (AST.Simple "empty_list") = return zeroConstant

-- | Generate TAC for new array
genForArray :: TAC.Value -> AST.ASTType -> TACMonad TAC.Value
genForArray len expType = do
    size <- getSizeForArray len expType
    -- Allocate memory
    temp <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.New (Just temp) (Just size) Nothing]

    -- Assign first element to len
    genRaw [TAC.ThreeAddressCode TAC.Set (Just temp) (Just zeroConstant) (Just len)]

    return temp

-- | Generate TAC for LValues
genForLValue :: AST.Expression -> TACMonad (TAC.Value -> TACMonad ())
genForLValue exp@AST.IdExp{AST.exp_entry=Just entry} =
    return $ \rValue -> do
        let lValue = TAC.Id $ TAC.Var entry
        genRaw [TAC.ThreeAddressCode TAC.Assign (Just lValue) (Just rValue) Nothing]

genForLValue exp@AST.IndexingExp{AST.exp_left=expLeft, AST.exp_right=expRight, AST.exp_type=expType} =
    return $ \rValue -> do
        (Just temp, _, _) <- genForExp expLeft
        lValue <- newTemp $ AST.exp_type expLeft
        genRaw [TAC.ThreeAddressCode TAC.Assign (Just lValue) (Just temp) Nothing]

        -- Calculate offset
        (Just temp, _, _) <- genForExp expRight
        w <- getSize expType
        index <- newTemp $ AST.Simple "quarter"
        genRaw [TAC.ThreeAddressCode TAC.Mult (Just index) (Just $ TAC.Constant (show w, AST.Simple "quarter")) (Just temp) ]

        -- Add one word, because first element is the size
        index' <- newTemp $ AST.Simple "quarter"
        genRaw [TAC.ThreeAddressCode TAC.Add (Just index') (Just arqWordConstant) (Just index)]

        genRaw [TAC.ThreeAddressCode TAC.Set (Just lValue) (Just index') (Just rValue)]

-- | Generate corresponding TAC to Expression
genForExp :: AST.Expression -> TACMonad (Maybe TAC.Value, InstList, InstList)

-- Literal expression
genForExp exp@(AST.LiteralExp expToken expType)
    -- if it's a string
    | expType == AST.Compound "Melody" (AST.Simple "half") = do
        let string = init (tail $ getStringFromExp exp) ++ ['\0']
            len = length string
            lenValue = TAC.Constant (show len, AST.Simple "quarter")
            size = PMonad.arqWord + len -- Allocate one int for size and one byte for NUL
            sizeValue = TAC.Constant (show size, AST.Simple "quarter")

        temp <- newTemp expType
        genRaw [TAC.ThreeAddressCode TAC.New (Just temp) (Just sizeValue ) Nothing]
        
        -- Store size
        genRaw [TAC.ThreeAddressCode TAC.Set (Just temp) (Just zeroConstant) (Just lenValue ) ]

        let chars = map (\c -> TAC.Constant (show c, AST.Simple "half")) string
        foldM_ ( pushArrayElement temp 1 ) PMonad.arqWord chars

        return (Just temp, [], [])

    | otherwise = return (Just $ TAC.Constant (Tokens.token expToken, expType), [], [])

-- Melody literals
genForExp exp@AST.MelodyLiteral{AST.exp_exps=expList, AST.exp_type=expType} = do
    let size = length expList
        sizeValue = TAC.Constant (show size, AST.Simple "quarter")
    temp <- genForArray sizeValue expType

    tempList <- mapM genAndBindExp expList
    typeSize <- getSize expType
    foldM_ ( pushArrayElement temp typeSize ) PMonad.arqWord tempList

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
    return (Just falseConstant, [], falselist)

-- True
genForExp idExp@(AST.IdExp (AST.Id (Tokens.MajToken "maj" _ _)) _ _) = do
    nextinst <- nextInst
    let truelist = makelist nextinst
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]
    return (Just trueConstant, truelist, [])

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
    let quarterType = AST.Simple "quarter"

    -- Get addr of left expression
    (Just temp1, _, _) <- genForExp expLeft
    temp1' <- newTemp $ AST.exp_type expLeft
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp1') (Just temp1) Nothing]

    -- Get offset
    (Just temp2, _, _) <- genForExp expRight
    w <- getSize expType
    temp2' <- newTemp quarterType
    genRaw [TAC.ThreeAddressCode TAC.Mult (Just temp2') (Just $ TAC.Constant (show w, AST.Simple "quarter")) (Just temp2)]

    -- Increment by one word, because first element is an int with size information
    temp2'' <- newTemp quarterType
    genRaw [TAC.ThreeAddressCode TAC.Add (Just temp2'') (Just arqWordConstant) (Just temp2')]


    temp <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Get (Just temp) (Just temp1') (Just temp2'')]
    return (Just temp, [], [])

genForExp exp@AST.LengthExp{AST.exp_exp=expExp, AST.exp_type=expType} = do
    -- Get addr of array
    (Just arr, _, _) <- genForExp expExp
    arr' <- newTemp $ AST.exp_type expExp
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just arr') (Just arr) Nothing]

    -- Get first element, because it's the array's size
    temp <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Get (Just temp) (Just arr') (Just zeroConstant) ]

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

-- Calling a function
genForExp exp@AST.CallExp{AST.exp_id=expId, AST.exp_params=params, AST.exp_type=expType, AST.exp_offset=offset, AST.exp_entry=Just entry} = do
    -- Generate TAC to each param
    tempList <- mapM genAndBindExp params
    
    -- Store current `base`
    genRaw [TAC.ThreeAddressCode TAC.Set (Just base) (Just $ toQuarterConstant offset) (Just base)]
    
    -- Increment `base`
    temp <- newTemp $ AST.Simple "quarter"
    genRaw [TAC.ThreeAddressCode TAC.Add (Just temp) (Just arqWordConstant) (Just $ toQuarterConstant offset)]
    genRaw [TAC.ThreeAddressCode TAC.Add (Just base) (Just base) (Just temp) ]

    -- Push params to stack
    mapM_ (\t -> genRaw [TAC.ThreeAddressCode TAC.Param Nothing (Just t) Nothing]) tempList
    
    -- Call function
    let n = length params
        name = AST.entry_name entry
    if expType == Parser.voidType
        then do
            genRaw [TAC.ThreeAddressCode TAC.Call Nothing (Just $ TAC.Label name) (Just $ toQuarterConstant n) ]
            return (Nothing, [], [])
        else do
            ret <- newTemp expType
            genRaw [TAC.ThreeAddressCode TAC.Call (Just ret) (Just $ TAC.Label name) (Just $ toQuarterConstant n) ]
            return (Just ret, [], [])

-- | Insert a list of raw instructions into final Three Address Code
genRaw :: [TAC.Instruction] -> TACMonad ()
genRaw lst = do
    state@TACState{inst_count=instCount} <- RWS.get
    RWS.tell lst
    let n = length lst
    RWS.put state{inst_count=instCount+n}

-- | Gen corresponding TAC for blocks
genForList :: [AST.Instruction] -> TACMonad InstList
genForList [] = return []
genForList stmts@(s:ss) = do
    nextlist1 <- gen s
    label@(TAC.Label l) <- newLabel

    bindLabel nextlist1 l
    genForList ss

-- | Generate corresponding TAC for array elements
genAndBindExp :: AST.Expression -> TACMonad TAC.Value
genAndBindExp astExp =
    case AST.exp_type astExp of
        AST.Simple "whole" -> genAndBindLogicalExp astExp
        _ -> do
            (Just temp, _, _) <- genForExp astExp
            return temp

-- | Generate corresponding TAC for Instruction
gen :: AST.Instruction -> TACMonad InstList
gen (AST.VarDecInst _) = return []

gen inst@(AST.AssignInst leftExp rightExp) = do
    let expType = AST.exp_type rightExp
    genAssignment <- genForLValue leftExp
    case expType of
        AST.Simple "whole" -> do
            (_, truelist, falselist)   <- genForExp rightExp

            nextinst <- nextInst
            label@(TAC.Label l) <- newLabel

            bindLabel truelist l
            genAssignment trueConstant

            nextinst <- nextInst
            let nextlist = makelist nextinst
            genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

            nextinst <- nextInst
            label@(TAC.Label l) <- newLabel

            bindLabel falselist l
            genAssignment falseConstant

            return nextlist

        AST.Compound "Melody" _ -> do
            (Just rValue, _, _)  <- genForExp rightExp
            case rightExp of
                AST.LiteralExp{AST.exp_type=AST.Compound{AST.type_str="Melody"}} ->
                    genAssignment rValue
                _ -> do
                    copy <- genForDeepCopy rValue
                    genAssignment copy
            return []

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

gen AST.ReturnInst{AST.inst_maybe_exp=maybeExp} = do
    let offsetConstant = toQuarterConstant $ -4
    fp <- newTemp $ AST.Simple "quarter"

    genRaw [TAC.ThreeAddressCode TAC.Get (Just fp) (Just base) (Just offsetConstant),
            TAC.ThreeAddressCode TAC.Assign (Just base) (Just fp) Nothing ]

    case maybeExp of
        Nothing -> genRaw [TAC.ThreeAddressCode TAC.Return Nothing Nothing Nothing]
        Just exp -> do
            temp <- genAndBindExp exp
            genRaw [TAC.ThreeAddressCode TAC.Return Nothing (Just temp) Nothing]

    return []

-- | Generate TAC for assignments
genForDeepCopy :: TAC.Value -> TACMonad TAC.Value
genForDeepCopy value1
    | AST.type_str (TAC.getType value1) == "Melody" = do
        let valueType@AST.Compound{AST.type_type=innerType} = TAC.getType value1
        -- Get the length of array, which is stored one word after the address
        len <- newTemp $ AST.Simple "quarter"
        genRaw [TAC.ThreeAddressCode TAC.Get (Just len) (Just value1) (Just zeroConstant)]

        -- Allocate memory
        addr <- genForArray len valueType
        size <- getSizeForArray len valueType

        i <- newTemp $ AST.Simple "quarter"
        w <- getSize innerType
        genRaw [TAC.ThreeAddressCode TAC.Assign (Just i) (Just arqWordConstant) Nothing]

        -- Iterate through array
        whileLabel <- newLabel
        guardInst <- nextInst
        genRaw [ TAC.ThreeAddressCode TAC.Gte (Just i) (Just size) Nothing ]

        -- Body
        temp1 <- newTemp innerType
        genRaw [TAC.ThreeAddressCode TAC.Get (Just temp1) (Just value1) (Just i)]

        copy <- genForDeepCopy temp1
        genRaw [TAC.ThreeAddressCode TAC.Set (Just addr) (Just i) (Just copy),
                TAC.ThreeAddressCode TAC.Add (Just i) (Just i) (Just $ TAC.Constant (show w, AST.Simple "quarter")),
                TAC.ThreeAddressCode TAC.GoTo Nothing Nothing (Just whileLabel)]

        -- Final
        label@(TAC.Label l) <- newLabel
        bindLabel [guardInst] l

        return addr

    | otherwise = return value1

-- | Generate TAC for comparing arrays
genForArrayComp :: TAC.Value -> TAC.Value -> TACMonad (Maybe TAC.Value, InstList, InstList)
genForArrayComp value1 value2
    | AST.type_str (TAC.getType value1) == "Melody" = do
        let valueType@AST.Compound{AST.type_type=innerType} = TAC.getType value1

        arr1 <- newTemp $ TAC.getType value1
        arr2 <- newTemp $ TAC.getType value2
        genRaw [TAC.ThreeAddressCode TAC.Assign (Just arr1) (Just value1) Nothing,
                TAC.ThreeAddressCode TAC.Assign (Just arr2) (Just value2) Nothing]

        -- Get the length of array, which is stored one word after the address
        len1 <- newTemp $ AST.Simple "quarter"
        len2 <- newTemp $ AST.Simple "quarter"
        genRaw [TAC.ThreeAddressCode TAC.Get (Just len1) (Just arr1) (Just zeroConstant),
                TAC.ThreeAddressCode TAC.Get (Just len2) (Just arr2) (Just zeroConstant)]

        -- Value to return
        ret <- newTemp $ AST.Simple "whole"

        -- If they are not of same size, break
        compInst <- nextInst
        genRaw [TAC.ThreeAddressCode TAC.Neq (Just len1) (Just len2) Nothing ]

        size <- getSizeForArray len1 valueType

        i <- newTemp $ AST.Simple "quarter"
        w <- getSize innerType
        genRaw [TAC.ThreeAddressCode TAC.Assign (Just i) (Just arqWordConstant) Nothing]

        -- Iterate through array
        whileLabel <- newLabel
        guardInst <- nextInst
        genRaw [ TAC.ThreeAddressCode TAC.Gte (Just i) (Just size) Nothing ]

        -- Body
        temp1 <- newTemp innerType
        temp2 <- newTemp innerType
        genRaw [TAC.ThreeAddressCode TAC.Get (Just temp1) (Just arr1) (Just i),
                TAC.ThreeAddressCode TAC.Get (Just temp2) (Just arr2) (Just i)]

        (_, truelist, falselist) <- genForArrayComp temp1 temp2
        TAC.Label lName <- newLabel
        bindLabel truelist lName
        genRaw [TAC.ThreeAddressCode TAC.Add (Just i) (Just i) (Just $ TAC.Constant (show w, AST.Simple "quarter")),
                TAC.ThreeAddressCode TAC.GoTo Nothing Nothing (Just whileLabel)]

        return (Nothing, [guardInst], merge [compInst] falselist )

    | otherwise = do
        temp1 <- newTemp $ TAC.getType value1
        temp2 <- newTemp $ TAC.getType value2

        trueInst <- nextInst
        genRaw [TAC.ThreeAddressCode TAC.Eq (Just temp1) (Just temp2) Nothing ]

        falseInst <- nextInst
        genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

        return (Nothing, [trueInst], [falseInst])


-- | Generate TAC for function
genForFunction :: AST.Entry -> TACMonad ()
genForFunction entry = do
    let Just (AST.Block stmts) = AST.function_block $ AST.entry_category entry
        name    = AST.entry_name entry

    genRaw [TAC.ThreeAddressCode TAC.NewLabel Nothing (Just $ TAC.Label name) Nothing]
    genForList stmts
    return ()

-- | Generate TAC for increment or decrement instruction
genForAbrev :: AST.Expression -> TAC.Operation -> TACMonad InstList
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
makelist :: Int -> InstList
makelist inst = [inst]

merge :: InstList -> InstList -> InstList
merge [] xs = xs
merge xs [] = xs
merge l1@(x:xs) l2@(y:ys)
    | x <= y = x : merge xs l2
    | otherwise = y : merge l1 ys

bindLabel :: InstList -> Label -> TACMonad ()
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
backpatch :: Label -> InstList -> [TAC.Instruction] -> Int -> [TAC.Instruction]
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

    let label = TAC.Label $ show labelCount
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