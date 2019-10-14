module LiteralsSpec where

import Test.Hspec
import Lexer
import Utils
import Tokens

spec :: Spec
spec = describe "Lexer" $ do

    it "accepts `0` as a valid int literal" $ do
        let x = "0"
        let atok = getTok $ runAlexScan x    
        case atok of
            [IntToken tk _ _ ]-> tk `shouldBe` x
            _ -> error "Pattern failed"

    -- it "accepts `1` as a valid int literal" $ do
    --     let x = "1"
    --     s <- runAlexScan x
    --     case s of
    --         Just toks -> do
    --             let atok = getToken $ head toks
    --             atok `shouldBe` IntToken
    --         Nothing ->
    --             error "rejected as an invalid token"

    -- it "accepts `100` as a valid int literal" $ do
    --     let x = "100"
    --     s <- runAlexScan x
    --     case s of
    --         Just toks -> do
    --             let atok = getToken $ head toks
    --             atok `shouldBe` IntToken
    --         Nothing ->
    --             error "rejected as an invalid token"
