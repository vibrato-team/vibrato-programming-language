module Backend.TAC.Monad where
    
import qualified Control.Monad.RWS.Lazy as RWS
import qualified Backend.TAC.TAC as TAC
import qualified Backend.TAC.Entry as TACEntry
import qualified AST

-- | State of the monad
data TACState = TACState {
    label_count :: Int,
    temp_count  :: Int
} deriving (Eq, Show)

type TACMonad = RWS.RWST () [TAC.TAC TACEntry.Entry AST.Expression] TACState IO

initialState :: TACState
initialState = TACState 1 1
