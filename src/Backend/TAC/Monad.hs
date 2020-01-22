module Backend.TAC.Monad where
    
import qualified Control.Monad.RWS.Lazy as RWS
import qualified Backend.TAC.TAC as TAC
import qualified AST
import qualified Frontend.Tokens as Tokens
import qualified Control.Monad

-- | State of the monad
data TACState = TACState {
    label_count :: Int,
    temp_count  :: Int
} deriving (Eq, Show)

type TACMonad = RWS.RWST () [TAC.Instruction] TACState IO

initialState :: TACState
initialState = TACState 1 1

newLabel :: TACMonad TAC.Value
newLabel = do
    (TACState labelCount tempCount) <- RWS.get
    RWS.put $ TACState (labelCount + 1) tempCount
    return $ TAC.Label labelCount

newTemp :: AST.Type -> TACMonad TAC.Value
newTemp astType = do
    (TACState labelCount tempCount) <- RWS.get
    RWS.put $ TACState labelCount $ tempCount + 1
    return $ TAC.Variable $ TAC.Entry ("_t" ++ show tempCount) astType Nothing

-- | Get name from Id
getStringFromId :: AST.Id -> String
getStringFromId = Tokens.token . AST.id_token

-- | Get name from IdExp
getStringFromIdExp :: AST.Expression -> String
getStringFromIdExp = getStringFromId . AST.exp_id

-- | IdExp to TAC.Entry
expToEntry :: AST.Expression -> TAC.Entry
expToEntry (AST.IdExp expId expType expScope) = TAC.Entry (getStringFromId expId) expType (Just expScope)

-- | Generate TAC for binary operation
genForBinOp :: AST.Expression -> TAC.Operation -> TACMonad TAC.Value
genForBinOp exp op = do
    rValue1 <- genForExp $ AST.exp_left exp
    rValue2 <- genForExp $ AST.exp_right exp
    temp <- newTemp $ AST.exp_type exp
    genRaw [TAC.ThreeAddressCode op (Just temp) (Just rValue1) (Just rValue2)]
    return temp

-- | Generate corresponding TAC to Expression
genForExp :: AST.Expression -> TACMonad TAC.Value

-- Literal expression
genForExp exp@AST.Literal{} = return $ TAC.Constant exp

-- Identifier
genForExp idExp@AST.IdExp{} = return $ TAC.Variable $ expToEntry idExp

-- Addition arithmetic expression
genForExp (AST.NegativeExp exp expType) = do
    rValue <- genForExp exp
    temp <- newTemp expType
    genRaw [TAC.ThreeAddressCode TAC.Minus (Just temp) (Just rValue) Nothing]
    return temp

-- Casting expression
genForExp (AST.CastExp exp (AST.Simple fromType) to@(AST.Simple toType)) = do
    rValue <- genForExp exp
    temp <- newTemp to
    genRaw [TAC.ThreeAddressCode (TAC.Cast fromType toType) (Just temp) (Just rValue) Nothing]
    return temp

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

-- TODO: CastExp, Pow, Logical Expressions, Conditional

-- | Insert a list of raw instructions into final Three Address Code
genRaw :: [TAC.Instruction] -> TACMonad ()
genRaw = RWS.tell

-- | Gen corresponding TAC for blocks
genForBlock :: AST.Block -> TACMonad ()
genForBlock (AST.Block stmts) = mapM_ gen stmts

-- | Generate corresponding TAC for Instruction
gen :: AST.Instruction -> TACMonad ()
gen (AST.VarDecInst _) = return ()

gen (AST.AssignInst leftExp rightExp) = do
    lValue <- genForExp leftExp
    rValue <- genForExp rightExp
    genRaw [TAC.ThreeAddressCode TAC.Assign (Just lValue) (Just rValue) Nothing]
