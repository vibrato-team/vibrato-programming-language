module IdSpec where

import Test.Hspec
import Lexer
import Utils
import Tokens

spec :: Spec
spec = describe "Lexer.ID" $ do

    it "accepts `aADssa436aDA5__24erR''''` as a valid Ids" $ do
        let x = "aADssa436aDA5__24erR''''"
        let atok = getTok $ runAlexScan x    
        case atok of
            [IdToken tk _ _ ]-> tk `shouldBe` x
            _ -> error "Pattern failed"
    
    it "accepts `Quarter Whole21` as a valid Types Id" $ do
        let x = "Quarter Whole21"
        let atok = getTok $ runAlexScan x    
        case atok of
            [IdTypeToken tk _ _, IdTypeToken tl _ _ ]-> do
                tk `shouldBe` "Quarter"
                tl `shouldBe` "Whole21"
            _ -> error "Pattern failed"