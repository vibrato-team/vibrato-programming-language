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
import Data.Maybe
import Data.Char
import Util.Arquitecture

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
    curr_offset     :: Int,
    loop_stack       :: [(InstList, InstList)], -- continue and break instructions
    sym_table       :: AST.SymbolTable
} deriving (Eq, Show)

type TACMonad = RWS.RWST () [TAC.Instruction] TACState IO

initialState :: AST.SymbolTable -> TACState
initialState = TACState 1 1 0 Map.empty 0 []

trueConstant    = TAC.Constant ("1",  AST.Simple "whole")
falseConstant   = TAC.Constant ("0", AST.Simple "whole")
arqWordConstant = TAC.Constant (show arqWord, AST.Simple "eighth")
doubleWordConstant = TAC.Constant (show doubleWord, AST.Simple "eighth")
zeroConstant    = TAC.Constant ("0", AST.Simple "eighth")
oneConstant     = TAC.Constant ("1", AST.Simple "eighth")
nullConstant    = zeroConstant

baseReg        = TAC.Reg "$sp" (AST.Simple "eighth")
base            = TAC.Id baseReg

-- TODO: Store head in temp everytime it is used
memoryHeadGlobal  = TAC.Global "head" (AST.Simple "eighth")
memoryHead      = TAC.Id memoryHeadGlobal

toEighthTemp :: Int -> TACMonad TAC.Value 
toEighthTemp x = do
    temp <- newTemp $ AST.Simple "eighth"
    let constant = TAC.Constant (show x, AST.Simple "eighth")
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp) (Just constant) Nothing]
    return temp

toEighthConstant :: Int -> TAC.Value
toEighthConstant x = TAC.Constant (show x, AST.Simple "eighth")

toWholeTemp :: Bool -> TACMonad TAC.Value
toWholeTemp b = do
    temp <- newTemp $ AST.Simple "whole"
    let constant = toWholeConstant b
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp) (Just constant) Nothing]
    return temp

toWholeConstant :: Bool -> TAC.Value
toWholeConstant b = TAC.Constant (if b then "1" else "0", AST.Simple "whole")

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

transformToLez :: TAC.Operation -> TAC.Value -> TAC.Value -> TACMonad TAC.Value
transformToLez op rValue1 rValue2= do
    temp <- newTemp $ TAC.getType rValue1
    if op `elem` [TAC.Lt, TAC.Lte]
        then genRaw [TAC.ThreeAddressCode TAC.Sub (Just temp) (Just rValue1) (Just rValue2)]
        else genRaw [TAC.ThreeAddressCode TAC.Sub (Just temp) (Just rValue2) (Just rValue1)]
    Control.Monad.when (op `elem` [TAC.Lt, TAC.Gt]) $ genRaw [TAC.ThreeAddressCode TAC.Add (Just temp) (Just temp) (Just oneConstant)]
    return temp

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
        if op `notElem` [TAC.Eq, TAC.Neq]
            then do
                temp <- transformToLez op rValue1 rValue2
            
                inst1 <- nextInst
                let truelist = makelist inst1
                genRaw [TAC.ThreeAddressCode TAC.Lez (Just temp) Nothing Nothing]

                inst2 <- nextInst
                let falselist = makelist inst2
                genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

                return (Nothing, truelist, falselist)
            
            else do
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
    let quarterType = AST.Simple "eighth"
    -- Size * width
    w <- getSize innerType
    temp0 <- newTemp quarterType
    constantTemp <- toEighthTemp w
    genRaw [TAC.ThreeAddressCode TAC.Mult (Just temp0) (Just constantTemp) (Just len)]

    -- First element is an integer of a word with the size
    size <- newTemp quarterType
    genRaw [TAC.ThreeAddressCode TAC.Add (Just size) (Just temp0) (Just arqWordConstant)]

    return size

getSizeForArray _ (AST.Simple "empty_list") = return zeroConstant

genForNew :: AST.ASTType -> TACMonad TAC.Value
genForNew astType = do
    size <- getSize astType
    sizeValue <- toEighthTemp size
    Just temp <- genForCallWithGC "malloc" sizeValue
    return temp

-- | Generate TAC for new array
genForArray :: TAC.Value -> AST.ASTType -> TACMonad TAC.Value
genForArray len expType = do
    size <- getSizeForArray len expType
    -- Allocate memory
    Just temp <- genForCallWithGC "malloc" size

    -- Assign first element to len
    genRaw [TAC.ThreeAddressCode TAC.Set (Just temp) (Just zeroConstant) (Just len)]

    return temp

-- | Generate TAC for LValues
genForLValue :: AST.Expression -> TACMonad (TAC.Value -> TACMonad ())
genForLValue exp@AST.IdExp{AST.exp_entry=Just entry} =
    return $ \rValue -> do
        let lValue = TAC.Id $ TAC.Var entry
        genRaw [TAC.ThreeAddressCode TAC.Store (Just rValue) (Just lValue) Nothing]

genForLValue exp@AST.DereferenceExp{AST.exp_exp=expExp} =
    return $ \rValue -> do
        (Just lexp', _, _) <- genForExp expExp
        lexp <- newTemp $ AST.exp_type expExp
        genRaw [TAC.ThreeAddressCode TAC.Assign (Just lexp) (Just lexp') Nothing]

        genRaw [TAC.ThreeAddressCode TAC.Set (Just lexp) (Just zeroConstant) (Just rValue)]

genForLValue exp@AST.IndexingExp{AST.exp_left=expLeft, AST.exp_right=expRight, AST.exp_type=expType} =
    return $ \rValue -> do
        (Just temp, _, _) <- genForExp expLeft
        lValue <- newTemp $ AST.exp_type expLeft
        genRaw [TAC.ThreeAddressCode TAC.Assign (Just lValue) (Just temp) Nothing]

        -- Calculate offset
        (Just temp, _, _) <- genForExp expRight
        w <- getSize expType
        index <- newTemp $ AST.Simple "eighth"
        constantTemp <- toEighthTemp w
        genRaw [TAC.ThreeAddressCode TAC.Mult (Just index) (Just constantTemp) (Just temp) ]

        -- Add one word, because first element is the size
        index' <- newTemp $ AST.Simple "eighth"
        genRaw [TAC.ThreeAddressCode TAC.Add (Just index') (Just index) (Just arqWordConstant),
                TAC.ThreeAddressCode TAC.Add (Just index') (Just index') (Just lValue),
                TAC.ThreeAddressCode TAC.Set (Just index') (Just zeroConstant) (Just rValue)]

genForIndexingExp exp@AST.IndexingExp{AST.exp_left=expLeft, AST.exp_right=expRight, AST.exp_type=expType} = do
    genComment "Idexing expression"
    let quarterType = AST.Simple "eighth"

    -- Get addr of left expression
    genComment "Get addr of left expression"
    (Just temp1, _, _) <- genForExp expLeft
    temp1' <- newTemp $ AST.exp_type expLeft
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp1') (Just temp1) Nothing]

    -- Get offset
    genComment "Get offset"
    (Just temp2, _, _) <- genForExp expRight
    w <- getSize expType
    temp2' <- newTemp quarterType
    constantTemp <- toEighthTemp w
    genRaw [TAC.ThreeAddressCode TAC.Mult (Just temp2') (Just constantTemp) (Just temp2)]

    -- Increment by one word, because first element is an int with size information
    genComment "Increment by one word, because first element is an int with size information"
    temp2'' <- newTemp quarterType
    genRaw [TAC.ThreeAddressCode TAC.Add (Just temp2'') (Just temp2') (Just arqWordConstant)]

    genComment "Get"
    temp <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Get (Just temp) (Just temp1') (Just temp2'')]
    return (Just temp, [], [])

-- | Generate corresponding TAC to Expression
genForExp :: AST.Expression -> TACMonad (Maybe TAC.Value, InstList, InstList)

-- Literal expression
genForExp exp@(AST.LiteralExp expToken expType)
    -- if it's a string
    | expType == AST.Compound "Melody" (AST.Simple "half") = do
        let string = init (tail $ getStringFromExp exp) ++ ['\0']
            len = length string
            size = arqWord + len -- Allocate one int for size and one byte for NUL

        lenValue <- toEighthTemp len
        sizeValue <- toEighthTemp size

        Just temp <- genForCallWithGC "malloc" sizeValue
        
        -- Store size
        genRaw [TAC.ThreeAddressCode TAC.Set (Just temp) (Just zeroConstant) (Just lenValue ) ]

        let chars = map (\c -> TAC.Constant (show $ ord c, AST.Simple "half")) string
        charTemps <- mapM (\c -> newTemp (AST.Simple "half") >>= \t -> genRaw [TAC.ThreeAddressCode TAC.Assign (Just t) (Just c) Nothing] >> return t) chars
        foldM_ ( pushArrayElement temp 1 ) arqWord charTemps

        return (Just temp, [], [])

    | otherwise = do
        let constant = TAC.Constant (Tokens.token expToken, expType)
        temp <- newTemp expType
        genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp) (Just constant) Nothing]
        return (Just temp, [], [])

-- Melody literals
genForExp exp@AST.MelodyLiteral{AST.exp_exps=expList, AST.exp_type=expType } = do
    let size = length expList
    sizeValue <- toEighthTemp size

    temp <- genForArray sizeValue expType
    tempList <- mapM genForArrayElement expList
    typeSize <- getSize $ AST.type_type expType
    foldM_ ( pushArrayElement temp typeSize ) arqWord tempList

    return (Just temp, [], [])

genForExp exp@AST.MelodyLiteral'{AST.exp_size=sizeExp, AST.exp_type=expType } = do
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

genForExp idExp@AST.IdExp{AST.exp_entry=Just entry, AST.exp_type=expType} = do
    temp <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Load (Just temp) (Just $ TAC.Id $ TAC.Var entry) Nothing]
    return (Just temp, [], [])

-- For array indexing

genForExp exp@AST.IndexingExp{AST.exp_left=expLeft, AST.exp_right=expRight, AST.exp_type=AST.Simple "whole"} = do
    (Just value, _, _) <- genForIndexingExp exp
    inst1 <- nextInst
    let truelist = makelist inst1
    genRaw [TAC.ThreeAddressCode TAC.If Nothing (Just value) Nothing]

    inst2 <- nextInst
    let falselist = makelist inst2
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

    return (Nothing, truelist, falselist)

genForExp exp@AST.IndexingExp{AST.exp_left=expLeft, AST.exp_right=expRight, AST.exp_type=expType} =
    genForIndexingExp exp

genForExp exp@AST.LengthExp{AST.exp_exp=expExp, AST.exp_type=expType} = do
    -- Get addr of array
    (Just arr, _, _) <- genForExp expExp
    arr' <- newTemp $ AST.exp_type expExp
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just arr') (Just arr) Nothing]

    -- Get first element, because it's the array's size
    temp <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Get (Just temp) (Just arr') (Just zeroConstant) ]

    return (Just temp, [], [])

-- New
genForExp exp@AST.NewExp{AST.exp_init=initMaybe, AST.exp_type=AST.Compound{AST.type_type=innerType} } = do
    ptr <- genForNew innerType

    case initMaybe of
        Nothing -> return (Just ptr, [], [])
        Just exp' -> do
            temp <- genAndBindExp exp'
            genRaw [TAC.ThreeAddressCode TAC.Set (Just ptr) (Just zeroConstant) (Just temp)]
            return (Just ptr, [], [])

genForExp exp@AST.DereferenceExp{AST.exp_exp=expExp, AST.exp_type=expType } = do
    (Just lexp', _, _) <- genForExp expExp
    lexp <- newTemp $ AST.exp_type expExp
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just lexp) (Just lexp') Nothing]

    temp <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Get (Just temp) (Just lexp) (Just zeroConstant)]
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
genForExp exp@AST.CallExp{AST.exp_id=expId, AST.exp_params=params, AST.exp_type=expType , AST.exp_entry=Just entry} = do
    -- Generate TAC to each param
    tempList <- mapM genAndBindExp params
    genForNewFrame tempList

    -- Call function
    let n = length params
        name = AST.entry_name entry
    if expType == Parser.voidType
        then do
            genRaw [TAC.ThreeAddressCode TAC.Call Nothing (Just $ TAC.Label name) (Just $ toEighthConstant n) ]
            return (Nothing, [], [])
        else do
            ret <- newTemp expType
            genRaw [TAC.ThreeAddressCode TAC.Call (Just ret) (Just $ TAC.Label name) (Just $ toEighthConstant n) ]
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

genForArrayElement :: AST.Expression -> TACMonad TAC.Value
genForArrayElement astExp = do
    temp <- genAndBindExp astExp
    genForDeepCopy temp

-- | Generate corresponding TAC for Instruction
gen :: AST.Instruction -> TACMonad InstList
gen (AST.VarDecInst _) = return []

gen inst@(AST.AssignInst leftExp rightExp) = do
    genComment "Assignment"
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
    genComment "If"
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
    genForCallWithGC "free" addr
    return []

gen AST.ReturnInst{AST.inst_maybe_exp=maybeExp} = do
    case maybeExp of
        Nothing -> genForReturn Nothing
        
        Just exp -> do
            temp' <- genAndBindExp exp
            temp <- newTemp $ AST.exp_type exp
            genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp) (Just temp') Nothing]
            
            genForReturn $ Just temp

    return []

gen AST.WhileInst { AST.inst_exp = instExp, AST.inst_block = instInst } = do
    genComment "While"
    pushLoopScope

    -- LoopLabel
    label@(TAC.Label loop) <- newLabel
    -- TAC Bool 
    (_, truelist, falselist) <- genForExp instExp
    -- True Label
    labeltrue@(TAC.Label ltrue) <- newLabel
    bindLabel truelist ltrue
    
    -- TAC BlockInstr
    nextlist1 <- gen (AST.BlockInst instInst)

    -- All continues go to beginning
    bindContinue label

    -- Goto LoopLabel
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing (Just label) ]

    -- False Label
    labelfalse@(TAC.Label lfalse) <- newLabel
    bindLabel falselist lfalse
    bindBreak labelfalse

    popLoopScope
    return []

gen (AST.ForInst inst_id inst_type inst_entry inst_block inst_start inst_end inst_step ) = do
    genComment "For"
    pushLoopScope

    -- Var declare and assign 
    genComment "Loop variable"
    let var = TAC.Id $ TAC.Var inst_entry
    varTemp <- getVarForLoop inst_type
        
    -- Asignar exp_start a var
    genComment "Assign start to var"
    case inst_start of
        Nothing -> genRaw [TAC.ThreeAddressCode TAC.Assign (Just varTemp) (Just zeroConstant) Nothing,
                            TAC.ThreeAddressCode TAC.Store (Just TAC.zeroReg) (Just var) Nothing ]
        Just start -> do 
            (Just temp, _, _) <- genForExp start
            genRaw [TAC.ThreeAddressCode TAC.Assign (Just varTemp) (Just temp) Nothing,
                    TAC.ThreeAddressCode TAC.Store (Just varTemp) (Just var) Nothing ]
    
    -- Generar codigo para Exp_End
    genComment "Code for end"
    (Just tempToCompare, _, _) <- genForExp inst_end

    -- LoopLabel
    genComment "Loop"
    label@(TAC.Label loop) <- newLabel

    -- Create If
    genComment "Check if loop var is in range"
    tempLez <- transformToLez TAC.Lt varTemp tempToCompare
    nextinstIf <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.Lez (Just tempLez) Nothing Nothing]
    nextinstfalse <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing ]
    
    -- True Label
    labeltrue@(TAC.Label ltrue) <- newLabel
    bindLabel [nextinstIf] ltrue

    -- TAC BlockInstr
    genComment "Loop block"
    nextlist1 <- gen (AST.BlockInst inst_block)

    -- Incremento de variable de iteracion
    genComment "Increment loop var"
    labelContinue <- newLabel
    bindContinue labelContinue

    case inst_step of
        Nothing -> genRaw [TAC.ThreeAddressCode TAC.Add (Just varTemp) (Just varTemp) (Just oneConstant),
                            TAC.ThreeAddressCode TAC.Store (Just varTemp) (Just var) Nothing ]
        Just step -> do
             -- Generar codigo para Exp_step
            (Just tempToStep, _, _) <- genForExp step
            genRaw [TAC.ThreeAddressCode TAC.Add (Just varTemp) (Just varTemp) (Just tempToStep),
                    TAC.ThreeAddressCode TAC.Store (Just varTemp) (Just var) Nothing]

    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing (Just label)]

    -- False Label
    labelfalse@(TAC.Label lfalse) <- newLabel
    bindLabel [nextinstfalse] lfalse
    bindBreak labelfalse

    popLoopScope
    return []

gen AST.NextInst = do
    inst <- nextInst
    addContinueInst inst
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]
    return []

gen AST.StopInst = do
    inst <- nextInst
    addBreakInst inst
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]
    return []

gen AST.RecordInst{ AST.inst_exps = exps } = do
    genComment "Read"
    tempList <- mapM genAndBindExp exps
    genAssignments <- mapM genForLValue exps
    mapM_ (\(t, genAssignment) -> genRaw [TAC.ThreeAddressCode TAC.Read Nothing (Just t) Nothing] >> genAssignment t) $ zip tempList genAssignments
    return []

gen AST.PlayInst{ AST.inst_exps = exps } = do
    genComment "Print"
    tempList <- mapM genAndBindExp exps
    mapM_ genPrintForTemp tempList
    return []

gen x = return []

genPrintForTemp :: TAC.Value -> TACMonad ()
genPrintForTemp temp =
    case TAC.getType temp of
        AST.Compound "Melody" (AST.Simple "half") -> do
            sz <- newTemp $ AST.Simple "eighth"
            addr <- newTemp $ AST.Simple "eighth"
            genRaw [TAC.ThreeAddressCode TAC.Get (Just sz) (Just temp) (Just zeroConstant),
                    TAC.ThreeAddressCode TAC.Add (Just addr) (Just temp) (Just arqWordConstant),
                    TAC.ThreeAddressCode TAC.Print Nothing (Just addr) (Just sz)]
        _ -> genRaw [TAC.ThreeAddressCode TAC.Print Nothing (Just temp) Nothing]
    
-- | Auxiliar for get Iterate var of Loop
getVarForLoop :: Maybe AST.ASTType -> TACMonad TAC.Value
getVarForLoop maybeType =
    newTemp $ fromMaybe (AST.Simple "eighth") maybeType

-- | Generate TAC for assignments
genForDeepCopy :: TAC.Value -> TACMonad TAC.Value
genForDeepCopy value1
    | AST.type_str (TAC.getType value1) == "Melody" = do
        genComment "Deep copy of melody"
        let valueType@AST.Compound{AST.type_type=innerType} = TAC.getType value1

        -- Get the length of array, which is stored on the first word at the address
        genComment "Get the length of array, which is stored on the first word at the address"
        arr1 <- newTemp valueType
        genRaw [TAC.ThreeAddressCode TAC.Assign (Just arr1) (Just value1) Nothing]

        len <- newTemp $ AST.Simple "eighth"
        genRaw [TAC.ThreeAddressCode TAC.Get (Just len) (Just arr1) (Just zeroConstant)]

        -- Allocate memory
        genComment "Allocate memory"
        addr <- genForArray len valueType
        size <- getSizeForArray len valueType

        i <- newTemp $ AST.Simple "eighth"
        w <- getSize innerType
        genRaw [TAC.ThreeAddressCode TAC.Assign (Just i) (Just arqWordConstant) Nothing]

        -- Iterate through array
        genComment "Iterate through array"
        whileLabel <- newLabel
        tempLez <- transformToLez TAC.Gte i size
        guardInst <- nextInst
        genRaw [ TAC.ThreeAddressCode TAC.Lez (Just tempLez) Nothing Nothing ]

        -- Body
        genComment "Body"
        temp1 <- newTemp innerType
        genRaw [TAC.ThreeAddressCode TAC.Get (Just temp1) (Just arr1) (Just i)]

        copy <- genForDeepCopy temp1
        i' <- newTemp $ AST.Simple "eighth"
        genRaw [TAC.ThreeAddressCode TAC.Add (Just i') (Just i) (Just addr),
                TAC.ThreeAddressCode TAC.Set (Just i') (Just zeroConstant) (Just copy),
                TAC.ThreeAddressCode TAC.Add (Just i) (Just i) (Just $ toEighthConstant w),
                TAC.ThreeAddressCode TAC.GoTo Nothing Nothing (Just whileLabel)]

        -- Final
        genComment "Final"
        label@(TAC.Label l) <- newLabel
        bindLabel [guardInst] l

        return addr

    | otherwise =
        case value1 of
            TAC.Id TAC.Temp{} -> return value1
            _ -> do
                temp <- newTemp $ TAC.getType value1
                genRaw [TAC.ThreeAddressCode TAC.Assign (Just temp) (Just value1) Nothing]
                return temp

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
        len1 <- newTemp $ AST.Simple "eighth"
        len2 <- newTemp $ AST.Simple "eighth"
        genRaw [TAC.ThreeAddressCode TAC.Get (Just len1) (Just arr1) (Just zeroConstant),
                TAC.ThreeAddressCode TAC.Get (Just len2) (Just arr2) (Just zeroConstant)]

        -- Value to return
        ret <- newTemp $ AST.Simple "whole"

        -- If they are not of same size, break
        compInst <- nextInst
        genRaw [TAC.ThreeAddressCode TAC.Neq (Just len1) (Just len2) Nothing ]

        size <- getSizeForArray len1 valueType

        i <- newTemp $ AST.Simple "eighth"
        w <- getSize innerType
        genRaw [TAC.ThreeAddressCode TAC.Assign (Just i) (Just arqWordConstant) Nothing]

        -- Iterate through array
        whileLabel <- newLabel
        tempLez <- transformToLez TAC.Gte i size
        guardInst <- nextInst
        genRaw [ TAC.ThreeAddressCode TAC.Lez (Just tempLez) Nothing Nothing ]

        -- Body
        temp1 <- newTemp innerType
        temp2 <- newTemp innerType
        genRaw [TAC.ThreeAddressCode TAC.Get (Just temp1) (Just arr1) (Just i),
                TAC.ThreeAddressCode TAC.Get (Just temp2) (Just arr2) (Just i)]

        (_, truelist, falselist) <- genForArrayComp temp1 temp2
        TAC.Label lName <- newLabel
        bindLabel truelist lName
        genRaw [TAC.ThreeAddressCode TAC.Add (Just i) (Just i) (Just $ toEighthConstant w),
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

-- | Generate TAC for return instruction
genForReturn' :: Maybe TAC.Value -> TACMonad ()
genForReturn' maybeValue = do
    genComment "Return"
    let offsetConstant = toEighthConstant arqWord
    fp <- newTemp $ AST.Simple "eighth"

    genRaw [TAC.ThreeAddressCode TAC.Get (Just TAC.raReg) (Just base) (Just $ toEighthConstant $ -arqWord),
            TAC.ThreeAddressCode TAC.Get (Just fp) (Just base) (Just offsetConstant),
            TAC.ThreeAddressCode TAC.Assign (Just base) (Just fp) Nothing]
    
    genRaw [TAC.ThreeAddressCode TAC.Return Nothing maybeValue Nothing]

genForReturn :: Maybe TAC.Value -> TACMonad ()
genForReturn maybeValue = do
    collectGarbage
    genForReturn' maybeValue

-- | Generate TAC for function
genForFunction :: AST.Entry -> TACMonad ()
genForFunction entry = do
    let cat = AST.entry_category entry
        Just (AST.Block stmts) = AST.function_block cat
        maxOffset       = fromJust $ AST.max_offset cat 
        name            = AST.entry_name entry
        params          = AST.function_params cat
        paramStrings    = map (Tokens.token . AST.id_token . AST.var_id) params
        Just level      = AST.entry_level entry

    setOffset maxOffset
    genRaw [TAC.ThreeAddressCode TAC.NewLabel Nothing (Just $ TAC.Label name) Nothing,
            -- Linked list of allocated objects set to NULL
            TAC.ThreeAddressCode TAC.Set (Just base) (Just zeroConstant) (Just TAC.zeroReg),
            -- Return address
            TAC.ThreeAddressCode TAC.Set (Just base) (Just $ toEighthConstant (-arqWord)) (Just TAC.raReg)]
            -- Next three words are reserverd for spills.

    -- Get entries of each param
    paramEntries <- mapM (lookupInScope level) paramStrings
    mapM_ genForTrackParam paramEntries

    -- Generate TAC for each instruction inside block
    genForList stmts
    return ()

-- | Add new allocated address to set (linked list, actually) of allocated address by the caller
trackNewAddr :: TAC.Value -> TAC.Value -> TACMonad ()
trackNewAddr addr isRecursive = do
    genComment "Track new address"
    -- If it is a recursive call of malloc, don't do anything
    isRecursiveInst <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.If Nothing (Just isRecursive) Nothing]

    prevBase <- getPrevBase

    temp <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Get (Just temp) (Just prevBase) (Just zeroConstant)]

    Just node <- genForCall "malloc" doubleWordConstant True
    genRaw [TAC.ThreeAddressCode TAC.Set (Just node) (Just zeroConstant) (Just temp),
            TAC.ThreeAddressCode TAC.Set (Just node) (Just arqWordConstant) (Just addr),
            TAC.ThreeAddressCode TAC.Set (Just prevBase) (Just zeroConstant) (Just node)]

    TAC.Label labelStr <- newLabel
    bindLabel [isRecursiveInst] labelStr

collectGarbage :: TACMonad ()
collectGarbage = do
    genComment "Collect garbage"
    iter <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Get (Just iter) (Just base) (Just zeroConstant)]

    -- If iter == NULL, break
    whileLabel@(TAC.Label whileLabelStr) <- newLabel
    isIterNullInst <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.Eq (Just iter) (Just TAC.zeroReg) Nothing]
    
    -----------------------------------------------------------------------------
    -- Get next node
    nextIter <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Get (Just nextIter) (Just iter) (Just zeroConstant)]

    -- Free address stored in node
    addr <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Get (Just addr) (Just iter) (Just arqWordConstant)]
    genForCallWithGC "free" addr

    -- Free address of node
    genForCallWithGC "free" iter

    -- Move to next iter
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just iter) (Just nextIter) Nothing]

    -- Jump to guard
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing (Just whileLabel)]
    -----------------------------------------------------------------------------

    TAC.Label labelStr <- newLabel
    bindLabel [isIterNullInst] labelStr

-- | Generate malloc function
genForMallocFunction :: TACMonad ()
genForMallocFunction = do
    setOffset initialOffset
    genRaw [TAC.ThreeAddressCode TAC.NewLabel Nothing (Just $ TAC.Label "malloc") Nothing,
            -- Linked list of allocated objects set to NULL
            TAC.ThreeAddressCode TAC.Set (Just base) (Just zeroConstant) (Just TAC.zeroReg),
            -- Return address
            TAC.ThreeAddressCode TAC.Set (Just base) (Just $ toEighthConstant (-arqWord)) (Just TAC.raReg)]

    size <- newTemp $ AST.Simple "eighth"
    isRecursive <- newTemp $ AST.Simple "whole"
    arqWord3Value <- toEighthTemp $ 3*arqWord
    genRaw [TAC.ThreeAddressCode TAC.Get (Just size) (Just base) (Just $ toEighthConstant $ -initialOffset),
            TAC.ThreeAddressCode TAC.Add (Just size) (Just size) (Just arqWord3Value),
            TAC.ThreeAddressCode TAC.Get (Just isRecursive) (Just base) (Just $ toEighthConstant $ -(initialOffset + arqWord))]

    --------------------------------------------------------------
    -- Initialization
    -- Iterate thorugh linked list
    prev <- newTemp $ AST.Simple "eighth"
    iter <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just prev) (Just zeroConstant) Nothing,
            TAC.ThreeAddressCode TAC.Load (Just iter) (Just memoryHead) Nothing]

    --------------------------------------------------------------
    -- Guard
    -- while (iter != null) { check; iter = iter.next }
    whileLabel@(TAC.Label whileLabelStr) <- newLabel
    compInst <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.Eq (Just iter) (Just TAC.zeroReg) Nothing]

    -- Get next block
    nextIter <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Get (Just nextIter) (Just iter) (Just zeroConstant)]

    ---------------------------------------------------------------------
    -- BODY
    -- Check if block is free and its size
    isFree <- newTemp $ AST.Simple "eighth"    -- 0: free, 1: allocated
    tempSize <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Get (Just isFree) (Just iter) (Just arqWordConstant),
            TAC.ThreeAddressCode TAC.Get (Just tempSize) (Just iter) (Just doubleWordConstant)]

    -- If it is allocated, move to next block
    isFreeInst <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.Neq (Just isFree) (Just TAC.zeroReg) Nothing]

    -- If it's size is not enough, move to next block
    tempLez <- transformToLez TAC.Lt tempSize size
    sizeInst <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.Lez (Just tempLez) Nothing Nothing]

    -- Split block and return
    nextBlock <- newTemp $ AST.Simple "eighth"
    nextSize <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Add (Just nextBlock)   (Just iter)                 (Just size),
            TAC.ThreeAddressCode TAC.Sub (Just nextSize)    (Just tempSize)             (Just size)]

    -- Generate blocks
    genForBlock nextBlock nextIter TAC.zeroReg nextSize
    oneTemp <- toEighthTemp 1
    genForBlock iter nextBlock oneTemp size

    trackNewAddr iter isRecursive
    genRaw [TAC.ThreeAddressCode TAC.Add (Just iter) (Just iter) (Just $ toEighthConstant $ 3*arqWord)]
    genForReturn' $ Just iter

    --------------------------------------------------------------
    -- Move to next block
    moveLabel@(TAC.Label moveLabelStr) <- newLabel
    bindLabel [isFreeInst, sizeInst] moveLabelStr

    genRaw [TAC.ThreeAddressCode TAC.Assign (Just prev) (Just iter) Nothing,
            TAC.ThreeAddressCode TAC.Get (Just iter) (Just iter) (Just zeroConstant),
            TAC.ThreeAddressCode TAC.GoTo Nothing Nothing (Just whileLabel)]

    --------------------------------------------------------------
    -- Finish loop and allocate new block
    allocateLabel@(TAC.Label allocateLabelStr) <- newLabel
    bindLabel [compInst] allocateLabelStr

    newBlock <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Sbrk (Just newBlock) (Just size) Nothing]
    oneTemp <- toEighthTemp 1
    genForBlock newBlock TAC.zeroReg oneTemp size

    -- If prev == NULL:
    prevNullComp <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.Neq (Just prev) (Just TAC.zeroReg) Nothing,
            TAC.ThreeAddressCode TAC.Store (Just newBlock) (Just memoryHead) Nothing]

    finalGoTo <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.GoTo Nothing Nothing Nothing]

    -- If prev != NULL:
    TAC.Label labelStr <- newLabel
    bindLabel [prevNullComp] labelStr
    genRaw [TAC.ThreeAddressCode TAC.Set (Just prev) (Just zeroConstant) (Just newBlock)]

    --------------------------------------------------------------
    -- Return new addr
    finalLabel@(TAC.Label finalLabelStr) <- newLabel
    bindLabel [finalGoTo] finalLabelStr

    trackNewAddr iter isRecursive

    genRaw [TAC.ThreeAddressCode TAC.Add (Just newBlock) (Just newBlock) (Just $ toEighthConstant $ 3*arqWord)]
    genForReturn' $ Just newBlock

-- | Generate free function
genForFreeFunction :: TACMonad ()
genForFreeFunction = do
    setOffset initialOffset
    genRaw [TAC.ThreeAddressCode TAC.NewLabel Nothing (Just $ TAC.Label "free") Nothing,
            -- Linked list of allocated objects set to NULL
            TAC.ThreeAddressCode TAC.Set (Just base) (Just zeroConstant) (Just TAC.zeroReg),
            -- Return address
            TAC.ThreeAddressCode TAC.Set (Just base) (Just $ toEighthConstant (-arqWord)) (Just TAC.raReg)]

    addr <- newTemp $ AST.Simple "eighth"
    isRecursive <- newTemp $ AST.Simple "whole"
    arqWord3Value <- toEighthTemp $ 3*arqWord
    genRaw [TAC.ThreeAddressCode TAC.Get (Just addr) (Just base) (Just $ toEighthConstant $ -initialOffset),
            TAC.ThreeAddressCode TAC.Sub (Just addr) (Just addr) (Just arqWord3Value),
            TAC.ThreeAddressCode TAC.Get (Just isRecursive) (Just base) (Just $ toEighthConstant $ -(initialOffset + arqWord))]

    --------------------------------------------------------------
    genComment "--------------------------------------------------------------"
    -- Initialization
    -- Iterate through linked list
    genComment "Iterate through linked list"
    prev <- newTemp $ AST.Simple "eighth"
    iter <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just prev) (Just zeroConstant) Nothing,
            TAC.ThreeAddressCode TAC.Load (Just iter) (Just memoryHead) Nothing]

    --------------------------------------------------------------
    genComment "--------------------------------------------------------------"
    -- Guard
    -- while (iter != null) { check; iter = iter.next }
    genComment "while (iter != null) { check; iter = iter.next }"
    whileLabel@(TAC.Label whileLabelStr) <- newLabel
    compInst <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.Eq (Just iter) (Just TAC.zeroReg) Nothing]

    -- Get next block
    genComment "Get next block"
    nextIter <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Get (Just nextIter) (Just iter) (Just zeroConstant)]

    ---------------------------------------------------------------------
    genComment "--------------------------------------------------------------"
    -- BODY
    genComment "Body"

    -- If it is not the addr, move to next block
    genComment "If it is not the addr, move to next block"
    iterEqAddrInst <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.Neq (Just iter) (Just addr) Nothing,
            TAC.ThreeAddressCode TAC.Set (Just iter) (Just arqWordConstant) (Just TAC.zeroReg)]

    ---------------------------------------------------------------------
    genComment "---------------------------------------------------------------------"
    -- if prev != NULL, and prev[1] == 0, merge with prev
    genComment "if prev != NULL, and prev[1] == 0, merge with prev"

    genForMerge iter nextIter
    genForMerge prev iter

    genForReturn' Nothing

    --------------------------------------------------------------
    genComment "--------------------------------------------------------------"
    -- Move to next block
    genComment "Move to next block"
    moveLabel@(TAC.Label moveLabelStr) <- newLabel
    bindLabel [iterEqAddrInst] moveLabelStr

    genRaw [TAC.ThreeAddressCode TAC.Assign (Just prev) (Just iter) Nothing,
            TAC.ThreeAddressCode TAC.Assign (Just iter) (Just nextIter) Nothing,
            TAC.ThreeAddressCode TAC.GoTo Nothing Nothing (Just whileLabel)]

    --------------------------------------------------------------
    genComment "--------------------------------------------------------------"
    -- Finish loop
    genComment "Finish loop"
    finalLabel@(TAC.Label finalLabelStr) <- newLabel
    bindLabel [compInst] finalLabelStr

    genForReturn' Nothing

-- | Generate TAC for block of metadata
genForBlock :: TAC.Value -> TAC.Value -> TAC.Value -> TAC.Value -> TACMonad ()
genForBlock block next allocated size =
    genRaw [TAC.ThreeAddressCode TAC.Set (Just block)   (Just zeroConstant)         (Just next),
            TAC.ThreeAddressCode TAC.Set (Just block)   (Just arqWordConstant)      (Just allocated),
            TAC.ThreeAddressCode TAC.Set (Just block)   (Just doubleWordConstant)   (Just size)]

-- | Generate TAC for checking if an address points to a free block. If it's free, merge
genForMerge :: TAC.Value -> TAC.Value -> TACMonad ()
genForMerge prev iter = do
    -- Deallocate and merge
    genComment "Deallocate and merge"
    -- if prev != NULL:
    prevNullInst <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.Eq (Just prev) (Just TAC.zeroReg) Nothing ]

    -- if prev[1] == 0:
    temp <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Get (Just temp) (Just prev) (Just arqWordConstant)]

    prevFreeInst <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.Neq (Just temp) (Just TAC.zeroReg) Nothing]

    --------------------------------------------------------------
    -- Merge
    checkIterIsNullInst <- nextInst
    genRaw [TAC.ThreeAddressCode TAC.Eq (Just iter) (Just TAC.zeroReg) Nothing]

    next <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Get (Just next) (Just iter) (Just zeroConstant)]

    size <- newTemp $ AST.Simple "eighth"
    size1 <- newTemp $ AST.Simple "eighth"
    size2 <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Get (Just size1) (Just prev) (Just doubleWordConstant),
            TAC.ThreeAddressCode TAC.Get (Just size2) (Just iter) (Just doubleWordConstant),
            TAC.ThreeAddressCode TAC.Add (Just size) (Just size1) (Just size2)]

    genForBlock prev next TAC.zeroReg size

    --------------------------------------------------------------
    -- if prev is not mergeable
    finalLabel@(TAC.Label finalLabelStr) <- newLabel
    bindLabel [prevNullInst, prevFreeInst, checkIterIsNullInst] finalLabelStr

-- | Generate TAC for increment or decrement instruction
genForAbrev :: AST.Expression -> TAC.Operation -> TACMonad InstList
genForAbrev exp op = do
    let expType = AST.exp_type exp
        one = TAC.Constant ("1", expType)

    genAssignment <- genForLValue exp
    (Just element, _, _) <- genForExp exp

    rValue <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Add (Just rValue) (Just element) (Just one)]

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
genComment cmnt = genRaw [TAC.ThreeAddressCode TAC.Comment Nothing (Just $ TAC.Label cmnt) Nothing]

getPrevBase :: TACMonad TAC.Value
getPrevBase = do
    let minusFour = toEighthConstant arqWord
    prevBase <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Get (Just prevBase) (Just base) (Just minusFour)]
    return prevBase

genForIncrementBase :: TACMonad ()
genForIncrementBase = do
    genComment "Increment Base"
    -- Increment `base` and Store current `base`
    temp <- newTemp $ AST.Simple "eighth"
    currOffset <- getAndIncrementOffsetBy arqWord
    currOffsetValue <- toEighthTemp currOffset
    genRaw [TAC.ThreeAddressCode TAC.Add (Just temp) (Just currOffsetValue) (Just arqWordConstant),
            TAC.ThreeAddressCode TAC.Sub (Just currOffsetValue) (Just base) (Just currOffsetValue),
            TAC.ThreeAddressCode TAC.Set (Just currOffsetValue) (Just zeroConstant) (Just base),
            TAC.ThreeAddressCode TAC.Sub (Just base) (Just base) (Just temp) ]
    
genForNewFrame :: [TAC.Value] -> TACMonad ()
genForNewFrame params = do
    currOffset <- getOffset
    foldM_ genForParamInst (currOffset + 2*arqWord) params
    genForIncrementBase

----------------------------------------------------------------------------
-- | Generate TAC for a call to `malloc` or `free` with Garbage Collector
genForCallWithGC name param' = genForCall name param' False

genForCall :: String -> TAC.Value -> Bool -> TACMonad (Maybe TAC.Value)
genForCall "free" param' isRecursive = do
    genComment "Call to `free`"
    param <- newTemp $ TAC.getType param'
    tempConstant <- toWholeTemp isRecursive
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just param) (Just param') Nothing]
    genForNewFrame [param, tempConstant]
    genRaw [TAC.ThreeAddressCode TAC.Call Nothing (Just $ TAC.Label "free") (Just $ toEighthConstant 2) ]
    return Nothing

genForCall "malloc" param' isRecursive = do
    genComment "Call to `malloc`"
    param <- newTemp $ TAC.getType param'
    temp1 <- newTemp $ TAC.getType param'
    temp2 <- newTemp $ TAC.getType param'
    tempConstant <- toWholeTemp isRecursive

    arqWordTemp <- toEighthTemp arqWord

    genRaw [TAC.ThreeAddressCode TAC.Assign (Just param) (Just param') Nothing,
            TAC.ThreeAddressCode TAC.Add (Just temp1) (Just param) (Just $ toEighthConstant $ arqWord - 1),
            TAC.ThreeAddressCode TAC.Div (Just temp2) (Just temp1) (Just arqWordTemp),
            TAC.ThreeAddressCode TAC.Mult (Just param) (Just temp2) (Just arqWordTemp)]

    genForNewFrame [param, tempConstant]

    ret <- newTemp $ AST.Simple "eighth"
    genRaw [TAC.ThreeAddressCode TAC.Call (Just ret) (Just $ TAC.Label "malloc") (Just $ toEighthConstant 2) ]
    return $ Just ret

genForParamInst :: Int -> TAC.Value -> TACMonad Int
genForParamInst offset param = do
    genComment "Insert parameter"
    size <- getSize $ TAC.getType param
    let offsetValue = toEighthConstant $ -(initialOffset + offset)
    genRaw [TAC.ThreeAddressCode TAC.Set (Just base) (Just offsetValue) (Just param)]
    return $ offset + size

genForTrackParam :: AST.Entry -> TACMonad ()
genForTrackParam e@AST.Entry{AST.entry_type=Just entryType, AST.entry_category=cat} =
    case entryType of
        AST.Compound "Melody" _ -> do
            -- Get param
            let Just offset = AST.offset cat
                offsetValue = toEighthConstant $ -offset
            param <- newTemp entryType
            genRaw [TAC.ThreeAddressCode TAC.Get (Just param) (Just base) (Just offsetValue)]

            -- Create deep copy
            paramCopy <- genForDeepCopy param
            genRaw [TAC.ThreeAddressCode TAC.Set (Just base) (Just offsetValue) (Just paramCopy)]
        _ ->
            return ()

newLabel :: TACMonad TAC.Value
newLabel = do
    state@TACState{label_count=labelCount} <- RWS.get
    RWS.put state{label_count=labelCount+1}

    let label = TAC.Label $ "label_" ++ show labelCount
    genRaw [TAC.ThreeAddressCode TAC.NewLabel Nothing (Just label) Nothing]

    return label

newTemp :: AST.ASTType -> TACMonad TAC.Value
newTemp astType = do
    size <- getSize astType
    offset <- getAndIncrementOffsetBy size

    state@TACState{temp_count=tempCount} <- RWS.get
    RWS.put state{temp_count=tempCount+1}

    return $ TAC.Id $ TAC.Temp ("$t" ++ show tempCount) astType (Just offset)

nextInst :: TACMonad Int
nextInst = do
    state@TACState{inst_count=instCount} <- RWS.get
    return instCount

setOffset :: Int -> TACMonad ()
setOffset newOffset = do
    state <- RWS.get
    RWS.put state{curr_offset=newOffset}

getOffset :: TACMonad Int
getOffset = do
    TACState{curr_offset=currOffset} <- RWS.get
    return currOffset

getAndIncrementOffsetBy :: Int -> TACMonad Int
getAndIncrementOffsetBy add = do
    currOffset <- getOffset
    setOffset $ nextWord currOffset add
    return currOffset

pushArrayElement :: TAC.Value -> Int -> Int -> TAC.Value -> TACMonad Int
pushArrayElement addr w offset rValue = do
    genRaw [TAC.ThreeAddressCode TAC.Set (Just addr) (Just $ toEighthConstant offset) (Just rValue) ]
    return $ offset + w

-- | Get chain from a symbol
getChain :: String -> TACMonad (Maybe [AST.Entry])
getChain symbol = do
    TACState{sym_table=table} <- RWS.get
    -- Get chain of matching entries
    return $ Map.lookup symbol table

-- | Get entry of a symbol in a specific scope
lookupInScope :: Int -> String -> TACMonad AST.Entry
lookupInScope scope symbol = do
    Just chain <- getChain symbol
    -- Get entry that matches symbol
    return $ head $ filter (\e -> AST.entry_scope e == scope) chain

-- | Get size of a type
getSize :: AST.ASTType -> TACMonad Int
getSize AST.Compound{} = return 4
getSize AST.Simple{AST.type_str=typeStr} = do
    (Just [ AST.Entry{ AST.entry_category = cat } ]) <- getChain typeStr
    return $ AST.type_size cat

pushLoopScope :: TACMonad ()
pushLoopScope = do
    state@TACState{ loop_stack = currLoops } <- RWS.get
    RWS.put $ state{ loop_stack = ([], []):currLoops }

popLoopScope :: TACMonad ()
popLoopScope = do
    state@TACState{ loop_stack = _:currLoops } <- RWS.get
    RWS.put $ state{ loop_stack = currLoops }

addContinueInst :: Int -> TACMonad ()
addContinueInst inst = do
    state@TACState{ loop_stack = (cs, bs):lst } <- RWS.get
    RWS.put $ state{ loop_stack = (inst:cs, bs):lst }

addBreakInst :: Int -> TACMonad ()
addBreakInst inst = do
    state@TACState{ loop_stack = (cs, bs):lst } <- RWS.get
    RWS.put $ state{ loop_stack = (cs, inst:bs):lst }

getCurrLoop :: TACMonad (InstList, InstList)
getCurrLoop = do
    state@TACState{ loop_stack = curr:_ } <- RWS.get
    return curr

bindContinue :: TAC.Value -> TACMonad ()
bindContinue label@(TAC.Label labelStr) = do
    (cs, _) <- getCurrLoop
    bindLabel cs labelStr

bindBreak :: TAC.Value -> TACMonad ()
bindBreak label@(TAC.Label labelStr) = do
    (_, bs) <- getCurrLoop
    bindLabel bs labelStr

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