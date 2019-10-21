module TokenSpec where

import Test.Hspec
import Lexer
import Utils
import Tokens

spec :: Spec
spec = describe "Lexer.Tokens" $ do

    it "accepts `1000` as a valid Token" $ do
        let x = "1000"
        let atok = head $ getTok $ runAlexScan x
        token atok `shouldBe` x
        