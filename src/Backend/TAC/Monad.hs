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

-- | Generate corresponding TAC to Expression
genForExp :: AST.Expression -> TACMonad TAC.Value
genForExp exp@AST.Literal{} = return $ TAC.Constant exp

genForExp idExp@AST.IdExp{} = return $ TAC.Variable $ expToEntry idExp

genForExp (AST.AdditionExp expLeft expRight expType) = do
    temp <- newTemp expType
    rValue1 <- genForExp expLeft
    rValue2 <- genForExp expRight
    genRaw [TAC.ThreeAddressCode TAC.Add (Just temp) (Just rValue1) (Just rValue2)]
    return temp

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